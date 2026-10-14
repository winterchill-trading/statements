open! Core
module Amount = Money.Amount
module Currency = Money.Currency
module Account_id = Ledger.Account_id
module Balances = Ledger.Balances
module Entry = Ledger.Entry
module Record = Ledger.Record
module Idempotency_key = Ledger.Idempotency_key

module Transfer = struct
  module Status = struct
    type t =
      | Pending
      | Settled of { at : Time_ns.Alternate_sexp.t }
      | Failed of { at : Time_ns.Alternate_sexp.t }
    [@@deriving sexp, compare, equal]

    let resolved_at = function
      | Pending -> None
      | Settled { at } | Failed { at } -> Some at
    ;;
  end

  type t =
    { id : Ledger.Hold_id.t
    ; account : Account_id.t
    ; amount : Amount.t
    ; created_at : Time_ns.Alternate_sexp.t
    ; status : Status.t
    ; entries : Idempotency_key.t list
    ; idempotency_key : Idempotency_key.t
    ; code : Entry.Code.t
    ; reference : string
    ; annotation : Annotation.t option
    }
  [@@deriving sexp, compare, equal]

  let pending_at t at =
    Time_ns.( < ) t.created_at at
    &&
    match Status.resolved_at t.status with
    | None -> true
    | Some resolved -> Time_ns.( >= ) resolved at
  ;;
end

type t =
  { annotate : (Entry.t -> Annotation.t) option
  ; records : Record.t list
  ; transfers : Transfer.t list option
  }

let of_ledger ?annotate records = { annotate; records; transfers = None }

let of_transfers ?annotate records ~transfers =
  { annotate; records; transfers = Some transfers }
;;

(* An entry booked for a transfer, where the journal has it. *)
type booked =
  { transfer : Transfer.t
  ; seq : int
  ; index : int
  ; entry : Entry.t
  ; delta : Amount.t (** its effect on the account's balance, on the normal side *)
  }

let position b = b.seq, b.index

(* A journal position: record sequence number and entry index. *)
module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let validate transfers =
  let open Or_error.Let_syntax in
  let%bind () =
    List.map transfers ~f:(fun (tr : Transfer.t) ->
      let bad msg =
        Or_error.error_s [%message msg ~transfer:(tr.id : Ledger.Hold_id.t)]
      in
      if not (Amount.is_positive tr.amount)
      then bad "transfer amount must be positive"
      else (
        match Transfer.Status.resolved_at tr.status with
        | Some at when Time_ns.( < ) at tr.created_at ->
          bad "transfer status time is before its creation"
        | _ -> Ok ()))
    |> Or_error.all_unit
  in
  let pairs =
    List.concat_map transfers ~f:(fun (tr : Transfer.t) ->
      List.map tr.entries ~f:(fun k -> k, tr))
  in
  match Idempotency_key.Map.of_alist pairs with
  | `Ok by_key -> Ok by_key
  | `Duplicate_key k ->
    Or_error.error_s
      [%message "an entry is named by two transfers" ~entry:(k : Idempotency_key.t)]
;;

(* An entry of a transfer without a description of its own takes the transfer's. *)
let annotate_with ?annotate by_key (e : Entry.t) =
  let a = (Option.value annotate ~default:Annotation.default) e in
  if not (String.is_empty a.description)
  then a
  else (
    match Map.find by_key e.idempotency_key with
    | Some { Transfer.annotation = Some from_transfer; _ } -> from_transfer
    | _ -> a)
;;

(* Every entry of [by_key] dated up to the end of the period, with its effect on [account]
   (and its pending sub-account, which ledger holds use) in [currency]. *)
let booked_entries records ~by_key ~account ~currency ~period =
  let open Or_error.Let_syntax in
  let touches (p : Ledger.Posting.t) =
    Account_id.equal p.account account
    || Account_id.equal p.account (Account_id.pending account)
  in
  let rec loop state acc = function
    | [] -> Ok acc
    | (r : Record.t) :: rest ->
      let entries = Record.entries r in
      let after_period =
        match entries with
        | e :: _ -> Date.( > ) e.business_date period.Period.to_date
        | [] -> false
      in
      if after_period
      then Ok acc
      else (
        let%bind state = Balances.apply state r in
        let normal_side =
          Option.map (Balances.find_account state account) ~f:(fun a ->
            Ledger.Account_kind.normal_side a.kind)
        in
        let acc =
          List.foldi entries ~init:acc ~f:(fun index acc (e : Entry.t) ->
            match Map.find by_key e.idempotency_key with
            | None -> acc
            | Some transfer ->
              let net =
                if Currency.equal (Entry.currency e) currency
                then
                  List.fold e.postings ~init:(Amount.zero currency) ~f:(fun sum p ->
                    if touches p
                    then Amount.add_exn sum (Ledger.Posting.signed p)
                    else sum)
                else Amount.zero currency
              in
              let delta =
                match normal_side with
                | Some Credit -> Amount.neg net
                | Some Debit | None -> net
              in
              { transfer; seq = r.seq; index; entry = e; delta } :: acc)
        in
        loop state acc rest)
  in
  let%map acc = loop Balances.empty [] records in
  List.rev acc
;;

let netted_with_transfers
  ~(ledger : Statement.t)
  ~annotate
  ~transfers
  ~booked
  ~period
  ~currency
  =
  let open Or_error.Let_syntax in
  let start = Period.start_time period
  and end_ = Period.end_time period in
  let of_transfer (tr : Transfer.t) =
    List.filter booked ~f:(fun b -> Ledger.Hold_id.equal b.transfer.id tr.id)
  in
  (* What the ledger has taken (a negative sum) for transfers still in flight at [at],
     counting the entries dated before [before]. Undoing it gives the netted balance. *)
  let in_flight ~at ~before =
    List.filter booked ~f:(fun b ->
      Transfer.pending_at b.transfer at && Date.( < ) b.entry.business_date before)
    |> List.map ~f:(fun b -> b.delta)
    |> Amount.sum currency
  in
  let%bind before_start = in_flight ~at:start ~before:period.Period.from_date in
  let%bind before_end =
    in_flight ~at:end_ ~before:(Date.add_days period.Period.to_date 1)
  in
  let opening = Amount.sub_exn ledger.totals.opening before_start in
  let expected_closing = Amount.sub_exn ledger.totals.closing before_end in
  let transfer_positions = List.map booked ~f:position |> Set.of_list (module Position) in
  let kept =
    List.filter ledger.lines ~f:(fun (l : Statement.Line.t) ->
      not (Set.mem transfer_positions (l.seq, l.index)))
  in
  let%bind settled =
    List.filter_map transfers ~f:(fun (tr : Transfer.t) ->
      match tr.status with
      | Settled { at } when Time_ns.( >= ) at start && Time_ns.( < ) at end_ ->
        Some (tr, at)
      | _ -> None)
    |> List.map ~f:(fun ((tr : Transfer.t), at) ->
      match
        List.max_elt (of_transfer tr) ~compare:(fun a b ->
          Position.compare (position a) (position b))
      with
      | None ->
        Or_error.error_s
          [%message
            "settled transfer has no ledger entry by the end of the period"
              ~transfer:(tr.id : Ledger.Hold_id.t)]
      | Some anchor ->
        Ok
          { Statement.Line.seq = anchor.seq
          ; index = anchor.index
          ; business_date = Time_ns.to_date at ~zone:Time_float.Zone.utc
          ; effective_at = at
          ; code = tr.code
          ; direction = Out
          ; amount = tr.amount
          ; balance = Amount.zero currency (* set below *)
          ; reference = tr.reference
          ; idempotency_key = anchor.entry.idempotency_key
          ; reversal = false
          ; hold = Some tr.id
          ; held_since = Some tr.created_at
          ; annotation =
              (match tr.annotation with
               | Some a -> a
               | None -> annotate anchor.entry)
          })
    |> Or_error.all
  in
  let lines, closing =
    List.sort (kept @ settled) ~compare:(fun (a : Statement.Line.t) b ->
      Position.compare (a.seq, a.index) (b.seq, b.index))
    |> List.fold_map ~init:opening ~f:(fun prev (l : Statement.Line.t) ->
      let balance = Amount.add_exn prev (Statement.Line.signed l) in
      balance, { l with balance })
    |> Tuple2.swap
  in
  let%bind () =
    if Amount.equal closing expected_closing
    then Ok ()
    else
      Or_error.error_s
        [%message
          "transfers do not reconcile with the ledger"
            ~closing_from_lines:(closing : Amount.t)
            ~ledger_closing_without_transfers_in_flight:(expected_closing : Amount.t)]
  in
  let pending_transfers =
    List.filter transfers ~f:(fun tr -> Transfer.pending_at tr end_)
    |> List.map ~f:(fun (tr : Transfer.t) ->
      let annotation =
        match tr.annotation, List.last (of_transfer tr) with
        | Some a, _ -> a
        | None, Some b -> annotate b.entry
        | None, None ->
          { Annotation.description = ""; counterparty = None; category = None }
      in
      { Statement.Pending.hold = tr.id
      ; placed_at = tr.created_at
      ; business_date = Time_ns.to_date tr.created_at ~zone:Time_float.Zone.utc
      ; amount = tr.amount
      ; expires_at = None
      ; code = tr.code
      ; reference = tr.reference
      ; idempotency_key = tr.idempotency_key
      ; annotation
      })
  in
  let pending =
    List.sort (ledger.pending @ pending_transfers) ~compare:(fun a b ->
      Ledger.Hold_id.compare a.hold b.hold)
  in
  let%bind held = Amount.sum currency (List.map pending ~f:(fun p -> p.amount)) in
  let held_for_pending = Amount.neg held in
  let money dir =
    Amount.sum
      currency
      (List.filter_map lines ~f:(fun (l : Statement.Line.t) ->
         Option.some_if (Statement.Direction.equal l.direction dir) l.amount))
  in
  let%bind money_in = money In in
  let%bind money_out = money Out in
  let statement =
    { ledger with
      totals = { opening; money_in; money_out; closing; lines = List.length lines }
    ; lines
    ; pending
    ; held_for_pending
    ; ledger_balance_at_end = Amount.add_exn closing held_for_pending
    }
  in
  let%map () = Statement.check statement in
  statement
;;

let statement t ~account ~currency ~period ~basis ~now =
  let open Or_error.Let_syntax in
  match t.transfers with
  | None ->
    Statement.create ?annotate:t.annotate t.records ~account ~currency ~period ~basis ~now
  | Some transfers ->
    let transfers =
      List.filter transfers ~f:(fun (tr : Transfer.t) ->
        Account_id.equal tr.account account
        && Currency.equal (Amount.currency tr.amount) currency)
    in
    let%bind by_key = validate transfers in
    let annotate = annotate_with ?annotate:t.annotate by_key in
    let%bind ledger =
      Statement.create ~annotate t.records ~account ~currency ~period ~basis ~now
    in
    (match basis with
     | Posting -> Ok ledger
     | Netted ->
       let%bind booked = booked_entries t.records ~by_key ~account ~currency ~period in
       netted_with_transfers ~ledger ~annotate ~transfers ~booked ~period ~currency)
;;
