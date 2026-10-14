open! Core
open Statements
open Helpers
module Transfer = Source.Transfer

(* Transfers that reserve money by moving it to an in-flight account when created: the
   settlement moves it on to the network, a failure moves it back. *)
let in_flight = Ledger.Account_id.of_string "internal:in-flight"

let journal () =
  let t = create () in
  push t (Account_opened (Ledger.Account.create in_flight Liability));
  deposit t ~k:"dep-1" ~when_:(at "2026-08-28") ~info:"Salary" "1000.00";
  t
;;

let start t ~id ~when_ a =
  transfer t ~k:(id ^ "-start") ~when_ ~code:Withdrawal ~from:alice ~to_:in_flight a
;;

let complete t ~id ~when_ a =
  transfer t ~k:(id ^ "-done") ~when_ ~code:Withdrawal ~from:in_flight ~to_:network a
;;

let give_back t ~id ~when_ a =
  transfer t ~k:(id ^ "-back") ~when_ ~code:Return ~from:in_flight ~to_:alice a
;;

let record ?(entries = []) ?annotation ~id ~created status a : Transfer.t =
  { id = Ledger.Hold_id.of_string id
  ; account = alice
  ; amount = eur a
  ; created_at = created
  ; status
  ; entries = List.map entries ~f:key
  ; idempotency_key = key id
  ; code = Withdrawal
  ; reference = id
  ; annotation =
      Option.map annotation ~f:(fun description ->
        { Annotation.description; counterparty = None; category = None })
  }
;;

let build ?(period = september_period) source basis =
  Source.statement source ~account:alice ~currency:Money.Currency.eur ~period ~basis ~now
;;

(* One of each: settled across the month start, failed, settled with no entry of its own,
   and still in flight at the end. *)
let in_flight_month () =
  let t = journal () in
  start t ~id:"tr-1" ~when_:(at "2026-08-30") "100.00";
  complete t ~id:"tr-1" ~when_:(at "2026-09-02") "100.00";
  start t ~id:"tr-2" ~when_:(at "2026-09-05") "40.00";
  give_back t ~id:"tr-2" ~when_:(at "2026-09-07") "40.00";
  start t ~id:"tr-3" ~when_:(at "2026-09-10") "25.00";
  start t ~id:"tr-4" ~when_:(at "2026-09-28") "60.00";
  complete t ~id:"tr-4" ~when_:(at "2026-10-02") "60.00";
  let transfers =
    [ record
        ~id:"tr-1"
        ~entries:[ "tr-1-start"; "tr-1-done" ]
        ~annotation:"Rent"
        ~created:(at "2026-08-30")
        (Settled { at = at "2026-09-02" })
        "100.00"
    ; record
        ~id:"tr-2"
        ~entries:[ "tr-2-start"; "tr-2-back" ]
        ~annotation:"Gym"
        ~created:(at "2026-09-05")
        (Failed { at = at "2026-09-07" })
        "40.00"
    ; record
        ~id:"tr-3"
        ~entries:[ "tr-3-start" ]
        ~annotation:"Books"
        ~created:(at "2026-09-10")
        (Settled { at = at "2026-09-11" ~hour:15 })
        "25.00"
    ; record
        ~id:"tr-4"
        ~entries:[ "tr-4-start"; "tr-4-done" ]
        ~annotation:"Phone"
        ~created:(at "2026-09-28")
        (Settled { at = at "2026-10-02" })
        "60.00"
    ]
  in
  Source.of_transfers (records t) ~transfers
;;

let%expect_test "netted: a transfer is one line when it settles; in flight is pending" =
  show (build (in_flight_month ()) Netted |> ok_exn);
  [%expect
    {|
    ((basis Netted) (as_of "2026-10-01 00:00:00Z") (opening "1000.00 EUR")
     (money_in "0.00 EUR") (money_out "125.00 EUR") (closing "875.00 EUR")
     (held_for_pending "-60.00 EUR") (ledger_balance_at_end "815.00 EUR"))
      9.0   2026-09-02 WITHDRAWAL        OUT   100.00    900.00  Rent held since 2026-08-30 09:00:00.000000000Z
      12.0  2026-09-11 WITHDRAWAL        OUT    25.00    875.00  Books held since 2026-09-10 09:00:00.000000000Z
      pending tr-4 2026-09-28 60.00 Phone
    |}]
;;

let%expect_test "posting: the ledger's own entries, transfer text on them" =
  show (build (in_flight_month ()) Posting |> ok_exn);
  [%expect
    {|
    ((basis Posting) (as_of "2026-10-01 00:00:00Z") (opening "900.00 EUR")
     (money_in "40.00 EUR") (money_out "125.00 EUR") (closing "815.00 EUR")
     (held_for_pending "0.00 EUR") (ledger_balance_at_end "815.00 EUR"))
      10.0  2026-09-05 WITHDRAWAL        OUT    40.00    860.00  Gym
      11.0  2026-09-07 RETURN            IN     40.00    900.00  Gym
      12.0  2026-09-10 WITHDRAWAL        OUT    25.00    875.00  Books
      13.0  2026-09-28 WITHDRAWAL        OUT    60.00    815.00  Phone
    |}]
;;

let%expect_test "the next month settles what was in flight" =
  show
    (build
       (in_flight_month ())
       ~period:(Period.of_month { year = 2026; month = Oct })
       Netted
     |> ok_exn);
  [%expect
    {|
    ((basis Netted) (as_of "2026-10-14 12:00:00Z") (opening "875.00 EUR")
     (money_in "0.00 EUR") (money_out "60.00 EUR") (closing "815.00 EUR")
     (held_for_pending "0.00 EUR") (ledger_balance_at_end "815.00 EUR"))
      14.0  2026-10-02 WITHDRAWAL        OUT    60.00    815.00  Phone held since 2026-09-28 09:00:00.000000000Z
    |}]
;;

let%expect_test "transfers booked only when they settle" =
  (* The ledger sees nothing until settlement, so a pending transfer reserves money the
     ledger still counts: the available balance is the closing balance minus it. *)
  let t = journal () in
  pay_out t ~k:"tr-5-paid" ~when_:(at "2026-09-16") "30.00";
  let transfers =
    [ record
        ~id:"tr-5"
        ~entries:[ "tr-5-paid" ]
        ~annotation:"Insurance"
        ~created:(at "2026-09-14")
        (Settled { at = at "2026-09-16" })
        "30.00"
    ; record ~id:"tr-6" ~annotation:"Tax" ~created:(at "2026-09-29") Pending "80.00"
    ]
  in
  let source = Source.of_transfers (records t) ~transfers in
  show (build source Netted |> ok_exn);
  show (build source Posting |> ok_exn);
  [%expect
    {|
    ((basis Netted) (as_of "2026-10-01 00:00:00Z") (opening "1000.00 EUR")
     (money_in "0.00 EUR") (money_out "30.00 EUR") (closing "970.00 EUR")
     (held_for_pending "-80.00 EUR") (ledger_balance_at_end "890.00 EUR"))
      8.0   2026-09-16 WITHDRAWAL        OUT    30.00    970.00  Insurance held since 2026-09-14 09:00:00.000000000Z
      pending tr-6 2026-09-29 80.00 Tax
    ((basis Posting) (as_of "2026-10-01 00:00:00Z") (opening "1000.00 EUR")
     (money_in "0.00 EUR") (money_out "30.00 EUR") (closing "970.00 EUR")
     (held_for_pending "0.00 EUR") (ledger_balance_at_end "970.00 EUR"))
      8.0   2026-09-16 WITHDRAWAL        OUT    30.00    970.00  Insurance
    |}]
;;

let%expect_test "without transfers the source is the ledger" =
  let records = records (september ()) in
  List.iter Basis.all ~f:(fun basis ->
    let direct =
      Statement.create
        records
        ~account:alice
        ~currency:Money.Currency.eur
        ~period:september_period
        ~basis
        ~now
    in
    [%test_result: Statement.t Or_error.t]
      (build (Source.of_ledger records) basis)
      ~expect:direct;
    [%test_result: Statement.t Or_error.t]
      (build (Source.of_transfers records ~transfers:[]) basis)
      ~expect:direct)
;;

let%expect_test "errors" =
  let t = journal () in
  start t ~id:"tr-1" ~when_:(at "2026-09-02") "100.00";
  let records = records t in
  let try_ transfers =
    print_s
      [%sexp
        (build (Source.of_transfers records ~transfers) Netted
         |> Or_error.map ~f:(fun _ -> ())
         : unit Or_error.t)]
  in
  let settled = Transfer.Status.Settled { at = at "2026-09-03" } in
  try_ [ record ~id:"tr-1" ~created:(at "2026-09-02") settled "0.00" ];
  try_
    [ record
        ~id:"tr-1"
        ~entries:[ "tr-1-start" ]
        ~created:(at "2026-09-02")
        settled
        "100.00"
    ; record
        ~id:"tr-2"
        ~entries:[ "tr-1-start" ]
        ~created:(at "2026-09-02")
        settled
        "100.00"
    ];
  try_
    [ record
        ~id:"tr-1"
        ~entries:[ "tr-1-start" ]
        ~created:(at "2026-09-02")
        (Settled { at = at "2026-09-01" })
        "100.00"
    ];
  try_ [ record ~id:"tr-1" ~created:(at "2026-09-02") settled "100.00" ];
  (* Says it failed, but the money never came back in the ledger. *)
  try_
    [ record
        ~id:"tr-1"
        ~entries:[ "tr-1-start" ]
        ~created:(at "2026-09-02")
        (Failed { at = at "2026-09-03" })
        "100.00"
    ];
  [%expect
    {|
    (Error ("transfer amount must be positive" (transfer tr-1)))
    (Error ("an entry is named by two transfers" (entry tr-1-start)))
    (Error ("transfer status time is before its creation" (transfer tr-1)))
    (Error
     ("settled transfer has no ledger entry by the end of the period"
      (transfer tr-1)))
    (Error
     ("transfers do not reconcile with the ledger"
      (closing_from_lines "1000.00 EUR")
      (ledger_closing_without_transfers_in_flight "900.00 EUR")))
    |}]
;;

(* The same payments run once as ledger holds (authorise, then capture or release) and
   once as transfers through the in-flight account must give the same statement on both
   bases, apart from codes, references and record numbers. *)
module Payment = struct
  type outcome =
    | Settle of int
    | Fail of int
    | Open
  [@@deriving sexp_of, quickcheck]

  type t =
    { cents : int
    ; created : int (** days after 2026-08-20 *)
    ; outcome : outcome (** days after [created] *)
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%map cents = Int.gen_incl 1 100_000
    and created = Int.gen_incl 0 45
    and outcome =
      Quickcheck.Generator.union
        [ (Int.gen_incl 1 10 >>| fun d -> Settle d)
        ; (Int.gen_incl 1 10 >>| fun d -> Fail d)
        ; return Open
        ]
    in
    { cents; created; outcome }
  ;;

  let day n = Time_ns.add (at "2026-08-20") (Time_ns.Span.of_day (Float.of_int n))

  let amount t =
    Money.Amount.of_minor_units_int Money.Currency.eur t.cents
    |> Money.Amount.to_decimal_string
  ;;
end

let comparable (s : Statement.t) =
  ( s.totals
  , s.held_for_pending
  , s.ledger_balance_at_end
  , List.map s.lines ~f:(fun l ->
      l.business_date, l.direction, l.amount, l.balance, l.held_since, l.hold)
  , List.map s.pending ~f:(fun p -> p.hold, p.business_date, p.amount, p.placed_at) )
;;

let%test_unit "transfers through an in-flight account read like ledger holds" =
  Quickcheck.test
    (List.gen_with_length 6 Payment.quickcheck_generator)
    ~trials:300
    ~sexp_of:[%sexp_of: Payment.t list]
    ~f:(fun payments ->
      let payments = List.mapi payments ~f:(fun i p -> sprintf "p%d" i, p) in
      (* Every step of every payment, in time order. *)
      let steps =
        List.concat_map payments ~f:(fun (id, (p : Payment.t)) ->
          (p.created, 0, id, p)
          ::
          (match p.outcome with
           | Settle d | Fail d -> [ p.created + d, 1, id, p ]
           | Open -> []))
        |> List.stable_sort ~compare:(fun (a, x, _, _) (b, y, _, _) ->
          [%compare: int * int] (a, x) (b, y))
      in
      let holds = create () in
      let flows = create () in
      push flows (Account_opened (Ledger.Account.create in_flight Liability));
      deposit holds ~k:"dep-1" ~when_:(at "2026-08-01") "1000000.00";
      deposit flows ~k:"dep-1b" ~when_:(at "2026-08-01") "1000000.00";
      List.iter steps ~f:(fun (day, phase, id, p) ->
        let when_ = Payment.day day in
        let a = Payment.amount p in
        match phase, p.outcome with
        | 0, _ ->
          authorise holds ~hold:id ~k:(id ^ "-start") ~when_ a;
          start flows ~id ~when_ a
        | _, Settle _ ->
          settle holds ~hold:id ~k:(id ^ "-done") ~when_ ();
          complete flows ~id ~when_ a
        | _, Fail _ ->
          release holds ~hold:id ~k:(id ^ "-back") ~when_;
          give_back flows ~id ~when_ a
        | _, Open -> assert false);
      let transfers =
        List.map payments ~f:(fun (id, p) ->
          let created = Payment.day p.created in
          let status, entries =
            match p.outcome with
            | Settle d ->
              ( Transfer.Status.Settled { at = Payment.day (p.created + d) }
              , [ id ^ "-start"; id ^ "-done" ] )
            | Fail d ->
              Failed { at = Payment.day (p.created + d) }, [ id ^ "-start"; id ^ "-back" ]
            | Open -> Pending, [ id ^ "-start" ]
          in
          record ~id ~entries ~created status (Payment.amount p))
      in
      List.iter Basis.all ~f:(fun basis ->
        let from_holds = build (Source.of_ledger (records holds)) basis |> ok_exn in
        let from_transfers =
          build (Source.of_transfers (records flows) ~transfers) basis |> ok_exn
        in
        let from_holds = comparable from_holds in
        let from_transfers = comparable from_transfers in
        if not
             ([%equal:
                Statement.Totals.t
                * Money.Amount.t
                * Money.Amount.t
                * (Date.t
                  * Statement.Direction.t
                  * Money.Amount.t
                  * Money.Amount.t
                  * Time_ns.Alternate_sexp.t option
                  * Ledger.Hold_id.t option)
                    list
                * (Ledger.Hold_id.t * Date.t * Money.Amount.t * Time_ns.Alternate_sexp.t)
                    list]
                from_holds
                from_transfers)
        then
          raise_s
            [%message
              "bases differ"
                (basis : Basis.t)
                (from_holds
                 : Statement.Totals.t
                   * Money.Amount.t
                   * Money.Amount.t
                   * (Date.t
                     * Statement.Direction.t
                     * Money.Amount.t
                     * Money.Amount.t
                     * Time_ns.Alternate_sexp.t option
                     * Ledger.Hold_id.t option)
                       list
                   * (Ledger.Hold_id.t
                     * Date.t
                     * Money.Amount.t
                     * Time_ns.Alternate_sexp.t)
                       list)
                (from_transfers
                 : Statement.Totals.t
                   * Money.Amount.t
                   * Money.Amount.t
                   * (Date.t
                     * Statement.Direction.t
                     * Money.Amount.t
                     * Money.Amount.t
                     * Time_ns.Alternate_sexp.t option
                     * Ledger.Hold_id.t option)
                       list
                   * (Ledger.Hold_id.t
                     * Date.t
                     * Money.Amount.t
                     * Time_ns.Alternate_sexp.t)
                       list)]))
;;
