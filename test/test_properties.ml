open! Core
open Statements
open Helpers
module Amount = Money.Amount

(* Random journals: a customer receives money, pays out, authorises card payments that
   later settle (in part or in full), are cancelled or stay open, and has entries
   reversed. Operations the ledger refuses (paying out more than is available) are
   skipped, as a bank would refuse them. *)
module Op = struct
  type t =
    | Deposit of int
    | Pay_out of int
    | Authorise of int
    | Settle of
        { pick : int
        ; part : int (** percent of the remaining hold *)
        ; release_rest : bool
        }
    | Release of int
    | Reverse of int
    | Other_customer of int
  [@@deriving sexp_of, quickcheck]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let cents = Int.gen_incl 1 50_000 in
    let pick = Int.gen_incl 0 1000 in
    Quickcheck.Generator.weighted_union
      [ (3., cents >>| fun c -> Deposit c)
      ; (2., cents >>| fun c -> Pay_out c)
      ; (3., cents >>| fun c -> Authorise c)
      ; ( 3.
        , let%map pick
          and part = Int.gen_incl 1 100
          and release_rest = Bool.quickcheck_generator in
          Settle { pick; part; release_rest } )
      ; (1., pick >>| fun p -> Release p)
      ; (1., pick >>| fun p -> Reverse p)
      ; (1., cents >>| fun c -> Other_customer c)
      ]
  ;;
end

let cents c = sprintf "%d.%02d" (c / 100) (c mod 100)
let start = Date.of_string "2026-01-01"

(* Each operation happens [gap] hours after the previous one: about four months of
   activity for a hundred operations. *)
let journal ops =
  let t = create () in
  let try_ f = Or_error.try_with f |> (ignore : unit Or_error.t -> unit) in
  let time =
    ref (Time_ns.of_date_ofday ~zone:Time_float.Zone.utc start Time_ns.Ofday.start_of_day)
  in
  let holds = ref [] in
  let keys = ref [] in
  List.iteri ops ~f:(fun i (gap, (op : Op.t)) ->
    time := Time_ns.add !time (Time_ns.Span.of_hr (Float.of_int gap));
    let when_ = !time in
    let k = sprintf "op-%d" i in
    let nth l p = List.nth l (p mod List.length l) in
    match op with
    | Deposit c ->
      try_ (fun () -> deposit t ~k ~when_ ~info:"in" (cents c));
      keys := k :: !keys
    | Pay_out c ->
      try_ (fun () ->
        pay_out t ~k ~when_ ~info:"out" (cents c);
        keys := k :: !keys)
    | Other_customer c -> try_ (fun () -> deposit t ~k ~to_:bob ~when_ (cents c))
    | Authorise c ->
      try_ (fun () ->
        authorise t ~hold:k ~k ~when_ ~info:"shop" (cents c);
        holds := k :: !holds)
    | Settle { pick; part; release_rest } when not (List.is_empty !holds) ->
      let hold = nth !holds pick |> Option.value_exn in
      let remaining =
        (Ledger.Balances.find_hold t.state (Ledger.Hold_id.of_string hold)
         |> Option.value_exn)
          .remaining
      in
      let amount =
        Amount.mul_bps remaining ~bps:(part * 100) |> Amount.to_decimal_string
      in
      try_ (fun () ->
        settle t ~hold ~k ~when_ ~amount ~release_remainder:release_rest ~info:"shop" ())
    | Release pick when not (List.is_empty !holds) ->
      try_ (fun () -> release t ~hold:(nth !holds pick |> Option.value_exn) ~k ~when_)
    | Reverse pick when not (List.is_empty !keys) ->
      try_ (fun () -> reverse t ~of_:(nth !keys pick |> Option.value_exn) ~k ~when_)
    | Settle _ | Release _ | Reverse _ -> ());
  records t
;;

let ops_generator =
  List.quickcheck_generator
    (Quickcheck.Generator.tuple2 (Int.gen_incl 0 30) Op.quickcheck_generator)
;;

let make records ~period ~basis =
  Statement.create
    records
    ~account:alice
    ~currency:Money.Currency.eur
    ~period
    ~basis
    ~now:(Time_ns.of_string_with_utc_offset "2030-01-01 00:00:00Z")
  |> ok_exn
;;

let months = List.init 6 ~f:(fun i -> Period.Year_month.of_date (Date.add_months start i))

let%test_unit "consecutive months chain, and two months equal their union" =
  Quickcheck.test
    ops_generator
    ~trials:200
    ~sexp_of:[%sexp_of: (int * Op.t) list]
    ~f:(fun ops ->
      let records = journal ops in
      List.iter Basis.all ~f:(fun basis ->
        List.iter
          (List.zip_exn (List.drop_last_exn months) (List.tl_exn months))
          ~f:(fun (m1, m2) ->
            let s1 = make records ~period:(Period.of_month m1) ~basis in
            let s2 = make records ~period:(Period.of_month m2) ~basis in
            [%test_result: Amount.t] s2.totals.opening ~expect:s1.totals.closing;
            let both =
              make
                records
                ~period:
                  (Period.of_days
                     ~from_date:(Period.Year_month.first_day m1)
                     ~to_date:(Period.Year_month.last_day m2)
                   |> ok_exn)
                ~basis
            in
            [%test_result: Statement.Line.t list] both.lines ~expect:(s1.lines @ s2.lines);
            [%test_result: Amount.t] both.totals.closing ~expect:s2.totals.closing)))
;;

let%test_unit "netted lines are the booked camt.053 entries" =
  Quickcheck.test
    ops_generator
    ~trials:200
    ~sexp_of:[%sexp_of: (int * Op.t) list]
    ~f:(fun ops ->
      let records = journal ops in
      List.iter months ~f:(fun m ->
        let period = Period.of_month m in
        let s = make records ~period ~basis:Netted in
        let camt =
          Ledger.Camt053.create
            records
            ~account:alice
            ~currency:Money.Currency.eur
            ~from_date:period.from_date
            ~to_date:period.to_date
            ~created_at:Time_ns.epoch
          |> ok_exn
        in
        let ours =
          List.map s.lines ~f:(fun l ->
            ( Statement.Line.entry_ref l
            , Amount.to_string l.amount
            , Statement.Direction.to_string l.direction ))
        in
        let theirs =
          List.map camt.entries ~f:(fun e ->
            ( e.entry_ref
            , Amount.to_string e.amount
            , match e.indicator with
              | Crdt -> "IN"
              | Dbit -> "OUT" ))
        in
        [%test_result: (string * string * string) list] ours ~expect:theirs;
        let closing =
          List.find_exn camt.balances ~f:(fun b ->
            Ledger.Camt053.Balance.Type.equal b.type_ Clbd)
        in
        [%test_result: string]
          (Amount.to_string (Amount.abs s.totals.closing))
          ~expect:(Amount.to_string closing.amount)))
;;

let%test_unit "both bases end on the same available balance" =
  Quickcheck.test
    ops_generator
    ~trials:200
    ~sexp_of:[%sexp_of: (int * Op.t) list]
    ~f:(fun ops ->
      let records = journal ops in
      List.iter months ~f:(fun m ->
        let period = Period.of_month m in
        let netted = make records ~period ~basis:Netted in
        let posting = make records ~period ~basis:Posting in
        [%test_result: Amount.t]
          posting.totals.closing
          ~expect:posting.ledger_balance_at_end;
        [%test_result: Amount.t]
          netted.ledger_balance_at_end
          ~expect:posting.totals.closing;
        [%test_result: Amount.t]
          (Amount.sub_exn netted.totals.closing posting.totals.closing)
          ~expect:(Amount.neg netted.held_for_pending)))
;;

let%test_unit "check rejects a tampered statement" =
  Quickcheck.test
    ops_generator
    ~trials:100
    ~sexp_of:[%sexp_of: (int * Op.t) list]
    ~f:(fun ops ->
      let records = journal ops in
      let s = make records ~period:(Period.of_month (List.hd_exn months)) ~basis:Netted in
      let one = Amount.of_minor_units_int Money.Currency.eur 1 in
      let tampered =
        { s with
          totals = { s.totals with closing = Amount.add_exn s.totals.closing one }
        }
      in
      assert (Or_error.is_error (Statement.check tampered));
      match s.lines with
      | [] -> ()
      | l :: rest ->
        let tampered =
          { s with lines = { l with amount = Amount.add_exn l.amount one } :: rest }
        in
        assert (Or_error.is_error (Statement.check tampered)))
;;

let%expect_test "the generator exercises every kind of line" =
  let codes = Hash_set.create (module String) in
  let pending = ref 0 in
  Quickcheck.iter ops_generator ~trials:50 ~f:(fun ops ->
    let records = journal ops in
    List.iter months ~f:(fun m ->
      let s = make records ~period:(Period.of_month m) ~basis:Netted in
      pending := !pending + List.length s.pending;
      List.iter s.lines ~f:(fun l ->
        Hash_set.add codes (Ledger.Entry.Code.to_string l.code))));
  print_s
    [%sexp (Hash_set.to_list codes |> List.sort ~compare:String.compare : string list)];
  print_s [%sexp (!pending > 0 : bool)];
  [%expect {|
    (CARD_SETTLEMENT DEPOSIT REVERSAL WITHDRAWAL)
    true
    |}]
;;
