open! Core
module Amount = Money.Amount
module Currency = Money.Currency
module Account_id = Ledger.Account_id
module Balances = Ledger.Balances
module Entry = Ledger.Entry
module Record = Ledger.Record

module Direction = struct
  module T = struct
    type t =
      | In
      | Out
    [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
  end

  include T

  let to_string = function
    | In -> "IN"
    | Out -> "OUT"
  ;;
end

module Line = struct
  module T = struct
    type t =
      { seq : int
      ; index : int
      ; business_date : Date.Stable.V1.t
      ; effective_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; code : Entry.Code.Stable.V1.t
      ; direction : Direction.T.t
      ; amount : Amount.Stable.V1.t
      ; balance : Amount.Stable.V1.t
      ; reference : string
      ; idempotency_key : Ledger.Idempotency_key.Stable.V1.t
      ; reversal : bool
      ; hold : Ledger.Hold_id.Stable.V1.t option
      ; held_since : Time_ns.Stable.Alternate_sexp.V1.t option
      ; annotation : Annotation.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  include T

  let signed t =
    match t.direction with
    | In -> t.amount
    | Out -> Amount.neg t.amount
  ;;

  let entry_ref t = sprintf "%d.%d" t.seq t.index
end

module Pending = struct
  module T = struct
    type t =
      { hold : Ledger.Hold_id.Stable.V1.t
      ; placed_at : Time_ns.Stable.Alternate_sexp.V1.t
      ; business_date : Date.Stable.V1.t
      ; amount : Amount.Stable.V1.t
      ; expires_at : Time_ns.Stable.Alternate_sexp.V1.t option
      ; code : Entry.Code.Stable.V1.t
      ; reference : string
      ; idempotency_key : Ledger.Idempotency_key.Stable.V1.t
      ; annotation : Annotation.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  include T
end

module Totals = struct
  module T = struct
    type t =
      { opening : Amount.Stable.V1.t
      ; money_in : Amount.Stable.V1.t
      ; money_out : Amount.Stable.V1.t
      ; closing : Amount.Stable.V1.t
      ; lines : int
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  include T

  module Stable = struct
    module V1 = T
  end
end

module T = struct
  type t =
    { account : Account_id.Stable.V1.t
    ; currency : Currency.Stable.V1.t
    ; basis : Basis.Stable.V1.t
    ; period : Period.Stable.V1.t
    ; as_of : Time_ns.Stable.Alternate_sexp.V1.t
    ; totals : Totals.T.t
    ; lines : Line.T.t list
    ; pending : Pending.T.t list
    ; held_for_pending : Amount.Stable.V1.t
    ; ledger_balance_at_end : Amount.Stable.V1.t
    }
  [@@deriving bin_io, sexp, compare, equal]
end

include T

let check t =
  let open Or_error.Let_syntax in
  let fail msg = Or_error.error_s [%message msg ~account:(t.account : Account_id.t)] in
  let same_currency a = Currency.equal (Amount.currency a) t.currency in
  let amounts =
    [ t.totals.opening
    ; t.totals.money_in
    ; t.totals.money_out
    ; t.totals.closing
    ; t.held_for_pending
    ; t.ledger_balance_at_end
    ]
    @ List.concat_map t.lines ~f:(fun l -> [ l.amount; l.balance ])
    @ List.map t.pending ~f:(fun p -> p.amount)
  in
  let%bind () =
    if List.for_all amounts ~f:same_currency
    then Ok ()
    else fail "amount in another currency"
  in
  let%bind () =
    if List.for_all t.lines ~f:(fun l -> Amount.is_positive l.amount)
       && List.for_all t.pending ~f:(fun p -> Amount.is_positive p.amount)
       && (not (Amount.is_negative t.totals.money_in))
       && not (Amount.is_negative t.totals.money_out)
    then Ok ()
    else fail "non-positive line amount or negative total"
  in
  let sum dir =
    Amount.sum
      t.currency
      (List.filter_map t.lines ~f:(fun l ->
         Option.some_if (Direction.equal l.direction dir) l.amount))
  in
  let%bind money_in = sum In in
  let%bind money_out = sum Out in
  let%bind () =
    if Amount.equal money_in t.totals.money_in
       && Amount.equal money_out t.totals.money_out
       && t.totals.lines = List.length t.lines
    then Ok ()
    else fail "totals do not match the lines"
  in
  let%bind expected_closing =
    let%bind x = Amount.add t.totals.opening money_in in
    Amount.sub x money_out
  in
  let%bind () =
    if Amount.equal expected_closing t.totals.closing
    then Ok ()
    else fail "opening + money in - money out <> closing"
  in
  let%bind last =
    List.fold_result t.lines ~init:t.totals.opening ~f:(fun prev l ->
      let%bind next = Amount.add prev (Line.signed l) in
      if Amount.equal next l.balance
      then Ok next
      else
        Or_error.error_s
          [%message "running balance breaks" ~entry_ref:(Line.entry_ref l : string)])
  in
  let%bind () =
    if Amount.equal last t.totals.closing then Ok () else fail "last balance <> closing"
  in
  let%bind () =
    let ordered =
      List.is_sorted_strictly t.lines ~compare:(fun a b ->
        [%compare: int * int] (a.seq, a.index) (b.seq, b.index))
    in
    if ordered
       && List.for_all t.lines ~f:(fun l -> Period.contains t.period l.business_date)
    then Ok ()
    else fail "lines out of order or outside the period"
  in
  let%bind held = Amount.sum t.currency (List.map t.pending ~f:(fun p -> p.amount)) in
  let%bind () =
    if Amount.equal (Amount.neg held) t.held_for_pending
       && (Basis.equal t.basis Netted || List.is_empty t.pending)
    then Ok ()
    else fail "held_for_pending does not match the pending holds"
  in
  let%bind at_end = Amount.add t.totals.closing t.held_for_pending in
  if Amount.equal at_end t.ledger_balance_at_end
  then Ok ()
  else fail "closing + held_for_pending <> ledger balance at end"
;;

module Builder = struct
  type t =
    { annotate : Entry.t -> Annotation.t
    ; account : Account_id.t
    ; currency : Currency.t
    ; period : Period.t
    ; basis : Basis.t
    ; state : Balances.t
    ; opening : Amount.t option
    ; lines_rev : Line.t list
    ; running : Amount.t option
    ; finished : bool (** a record dated after the period was seen *)
    }

  let create ?(annotate = Annotation.default) ~account ~currency ~period ~basis () =
    { annotate
    ; account
    ; currency
    ; period
    ; basis
    ; state = Balances.empty
    ; opening = None
    ; lines_rev = []
    ; running = None
    ; finished = false
    }
  ;;

  (* The balance the statement runs on: the account with its pending sub-account when
     holds are netted (placing a hold moves nothing out of that pair), the account alone
     on the posting basis. *)
  let view t state =
    match t.basis with
    | Netted -> Balances.posted state t.account t.currency
    | Posting -> Balances.balance state t.account t.currency
  ;;

  let touches t (p : Ledger.Posting.t) =
    Account_id.equal p.account t.account
    ||
    match t.basis with
    | Netted -> Account_id.equal p.account (Account_id.pending t.account)
    | Posting -> false
  ;;

  let held_since state (e : Entry.t) =
    let%bind.Option hold = Entry.Kind.hold e.kind in
    let%bind.Option h = Balances.find_hold state hold in
    let%map.Option _, placing = Balances.find_entry state h.placed_by in
    placing.effective_at
  ;;

  let add t (r : Record.t) =
    let open Or_error.Let_syntax in
    if t.finished
    then Ok t
    else (
      let entries = Record.entries r in
      let after_period =
        match entries with
        | e :: _ -> Date.( > ) e.business_date t.period.to_date
        | [] -> false
      in
      if after_period
      then Ok { t with finished = true }
      else (
        let in_period (e : Entry.t) =
          Period.contains t.period e.business_date
          && Currency.equal (Entry.currency e) t.currency
        in
        let opening =
          match t.opening with
          | None
            when List.exists entries ~f:(fun e ->
                   Period.contains t.period e.business_date) -> Some (view t t.state)
          | o -> o
        in
        let%bind state = Balances.apply t.state r in
        let normal_side =
          Option.map (Balances.find_account state t.account) ~f:(fun a ->
            Ledger.Account_kind.normal_side a.kind)
        in
        let lines_rev, running =
          List.foldi
            entries
            ~init:(t.lines_rev, t.running)
            ~f:(fun index (lines_rev, running) (e : Entry.t) ->
              if not (in_period e)
              then lines_rev, running
              else (
                (* Debits minus credits on the accounts the statement covers, turned to
                   the account's normal side. *)
                let net =
                  List.fold e.postings ~init:(Amount.zero t.currency) ~f:(fun acc p ->
                    if touches t p
                    then Amount.add_exn acc (Ledger.Posting.signed p)
                    else acc)
                in
                let delta =
                  match normal_side with
                  | Some Credit -> Amount.neg net
                  | Some Debit | None -> net
                in
                if Amount.is_zero delta
                then lines_rev, running
                else (
                  let prev =
                    match running, opening with
                    | Some b, _ | None, Some b -> b
                    | None, None -> Amount.zero t.currency
                  in
                  let balance = Amount.add_exn prev delta in
                  let hold = Entry.Kind.hold e.kind in
                  let line =
                    { Line.seq = r.seq
                    ; index
                    ; business_date = e.business_date
                    ; effective_at = e.effective_at
                    ; code = e.code
                    ; direction = (if Amount.is_positive delta then In else Out)
                    ; amount = Amount.abs delta
                    ; balance
                    ; reference = e.reference
                    ; idempotency_key = e.idempotency_key
                    ; reversal =
                        (match e.kind with
                         | Reversal _ -> true
                         | _ -> false)
                    ; hold
                    ; held_since =
                        (match t.basis with
                         | Netted -> held_since state e
                         | Posting -> None)
                    ; annotation = t.annotate e
                    }
                  in
                  line :: lines_rev, Some balance)))
        in
        Ok { t with state; opening; lines_rev; running }))
  ;;

  let pending t =
    match t.basis with
    | Posting -> Ok []
    | Netted ->
      Balances.open_holds t.state
      |> List.filter ~f:(fun (h : Balances.Hold_state.t) ->
        Account_id.equal h.account t.account
        && Currency.equal (Amount.currency h.remaining) t.currency)
      |> List.map ~f:(fun (h : Balances.Hold_state.t) ->
        match Balances.find_entry t.state h.placed_by with
        | None ->
          Or_error.error_s
            [%message "hold without its placing entry" (h.id : Ledger.Hold_id.t)]
        | Some (_, e) ->
          Ok
            { Pending.hold = h.id
            ; placed_at = e.effective_at
            ; business_date = e.business_date
            ; amount = h.remaining
            ; expires_at = h.expires_at
            ; code = e.code
            ; reference = e.reference
            ; idempotency_key = e.idempotency_key
            ; annotation = t.annotate e
            })
      |> Or_error.all
  ;;

  let finish t ~now =
    let open Or_error.Let_syntax in
    let%bind () =
      if Account_id.is_pending t.account
      then
        Or_error.error_s
          [%message
            "statements cover an account, not its pending sub-account"
              ~account:(t.account : Account_id.t)]
      else Ok ()
    in
    let%bind () =
      match Balances.find_account t.state t.account with
      | Some _ -> Ok ()
      | None ->
        Or_error.error_s
          [%message
            "account not opened by the end of the period"
              ~account:(t.account : Account_id.t)]
    in
    let closing = view t t.state in
    let opening = Option.value t.opening ~default:closing in
    let lines = List.rev t.lines_rev in
    let sum dir =
      Amount.sum
        t.currency
        (List.filter_map lines ~f:(fun l ->
           Option.some_if (Direction.equal l.direction dir) l.amount))
    in
    let%bind money_in = sum In in
    let%bind money_out = sum Out in
    let%bind pending = pending t in
    let held_for_pending =
      match t.basis with
      | Netted -> Amount.neg (Balances.held t.state t.account t.currency)
      | Posting -> Amount.zero t.currency
    in
    let statement =
      { account = t.account
      ; currency = t.currency
      ; basis = t.basis
      ; period = t.period
      ; as_of = Time_ns.min (Period.end_time t.period) now
      ; totals = { opening; money_in; money_out; closing; lines = List.length lines }
      ; lines
      ; pending
      ; held_for_pending
      ; ledger_balance_at_end = Balances.available t.state t.account t.currency
      }
    in
    let%map () = check statement in
    statement
  ;;
end

let create ?annotate records ~account ~currency ~period ~basis ~now =
  let%bind.Or_error b =
    List.fold_result
      records
      ~init:(Builder.create ?annotate ~account ~currency ~period ~basis ())
      ~f:Builder.add
  in
  Builder.finish b ~now
;;

let of_journal_file ?annotate path ~account ~currency ~period ~basis ~now =
  let%bind.Or_error b, _balances, _tail =
    Ledger.Journal.fold
      path
      ~init:(Ok (Builder.create ?annotate ~account ~currency ~period ~basis ()))
      ~f:(fun acc r -> Or_error.bind acc ~f:(fun b -> Builder.add b r))
  in
  let%bind.Or_error b in
  Builder.finish b ~now
;;

let file_name t ~prefix ~extension =
  sprintf
    "%s-statement-%s-%s.%s"
    prefix
    (Currency.to_string t.currency)
    (Period.slug t.period)
    extension
;;

let newest_lines t ~limit =
  let newest = List.rev t.lines in
  List.take newest limit, List.length newest > limit
;;

module Stable = struct
  module V1 = struct
    include T

    let t_of_sexp s =
      let t = t_of_sexp s in
      check t |> ok_exn;
      t
    ;;

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (T)
        (struct
          type nonrec t = t

          let to_binable = Fn.id

          let of_binable t =
            check t |> ok_exn;
            t
          ;;
        end)
  end
end
