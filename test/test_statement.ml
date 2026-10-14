open! Core
open Statements
open Helpers

let show (s : Statement.t) =
  print_s
    [%message
      ""
        ~basis:(s.basis : Basis.t)
        ~as_of:(s.as_of : Time_ns.Alternate_sexp.t)
        ~opening:(Money.Amount.to_string s.totals.opening)
        ~money_in:(Money.Amount.to_string s.totals.money_in)
        ~money_out:(Money.Amount.to_string s.totals.money_out)
        ~closing:(Money.Amount.to_string s.totals.closing)
        ~held_for_pending:(Money.Amount.to_string s.held_for_pending)
        ~ledger_balance_at_end:(Money.Amount.to_string s.ledger_balance_at_end)];
  List.iter s.lines ~f:(fun l ->
    printf
      "  %-5s %s %-17s %-3s %8s %9s  %s%s%s\n"
      (Statement.Line.entry_ref l)
      (Date.to_string l.business_date)
      (Ledger.Entry.Code.to_string l.code)
      (Statement.Direction.to_string l.direction)
      (Money.Amount.to_decimal_string l.amount)
      (Money.Amount.to_decimal_string l.balance)
      l.annotation.description
      (if l.reversal then " (reversal)" else "")
      (match l.held_since with
       | None -> ""
       | Some t -> " held since " ^ Time_ns.to_string_utc t));
  List.iter s.pending ~f:(fun p ->
    printf
      "  pending %s %s %s %s\n"
      (Ledger.Hold_id.to_string p.hold)
      (Date.to_string p.business_date)
      (Money.Amount.to_decimal_string p.amount)
      p.annotation.description)
;;

let statement ?(period = september_period) basis =
  Statement.create
    (records (september ()))
    ~account:alice
    ~currency:Money.Currency.eur
    ~period
    ~basis
    ~now
  |> ok_exn
;;

let%expect_test "netted: settlements are lines, open holds are pending" =
  show (statement Netted);
  [%expect
    {|
    ((basis Netted) (as_of "2026-10-01 00:00:00Z") (opening "500.00 EUR")
     (money_in "350.00 EUR") (money_out "145.50 EUR") (closing "704.50 EUR")
     (held_for_pending "-20.00 EUR") (ledger_balance_at_end "684.50 EUR"))
      8.0   2026-09-01 DEPOSIT           IN    250.00    750.00  Top-up
      11.0  2026-09-05 CARD_SETTLEMENT   OUT    45.50    704.50  Coffee Corner held since 2026-09-03 09:00:00.000000000Z
      13.0  2026-09-10 WITHDRAWAL        OUT   100.00    604.50  To savings, "Ltd"
      14.0  2026-09-12 REVERSAL          IN    100.00    704.50   (reversal)
      pending auth-3 2026-09-29 20.00 Bookshop
    |}]
;;

let%expect_test "posting basis: holds are lines when placed and released" =
  show (statement Posting);
  [%expect
    {|
    ((basis Posting) (as_of "2026-10-01 00:00:00Z") (opening "500.00 EUR")
     (money_in "474.50 EUR") (money_out "290.00 EUR") (closing "684.50 EUR")
     (held_for_pending "0.00 EUR") (ledger_balance_at_end "684.50 EUR"))
      8.0   2026-09-01 DEPOSIT           IN    250.00    750.00  Top-up
      9.0   2026-09-03 CARD_AUTH         OUT    50.00    700.00  Coffee Corner
      10.0  2026-09-04 CARD_AUTH         OUT   120.00    580.00  Hotel
      11.0  2026-09-05 CARD_SETTLEMENT   IN      4.50    584.50  Coffee Corner
      12.0  2026-09-06 HOLD              IN    120.00    704.50  Hotel
      13.0  2026-09-10 WITHDRAWAL        OUT   100.00    604.50  To savings, "Ltd"
      14.0  2026-09-12 REVERSAL          IN    100.00    704.50   (reversal)
      16.0  2026-09-29 CARD_AUTH         OUT    20.00    684.50  Bookshop
    |}]
;;

let%expect_test "a period that has not ended runs to now" =
  let s = statement ~period:(Period.of_month { year = 2026; month = Oct }) Netted in
  show s;
  [%expect
    {|
    ((basis Netted) (as_of "2026-10-14 12:00:00Z") (opening "704.50 EUR")
     (money_in "10.00 EUR") (money_out "20.00 EUR") (closing "694.50 EUR")
     (held_for_pending "0.00 EUR") (ledger_balance_at_end "694.50 EUR"))
      17.0  2026-10-02 CARD_SETTLEMENT   OUT    20.00    684.50  Bookshop held since 2026-09-29 09:00:00.000000000Z
      18.0  2026-10-03 DEPOSIT           IN     10.00    694.50  Top-up
    |}]
;;

let%expect_test "errors" =
  let records = records (september ()) in
  let try_ account =
    print_s
      [%sexp
        (Statement.create
           records
           ~account
           ~currency:Money.Currency.eur
           ~period:september_period
           ~basis:Netted
           ~now
         |> Or_error.map ~f:(fun _ -> ())
         : unit Or_error.t)]
  in
  try_ (Ledger.Account_id.pending alice);
  try_ (Ledger.Account_id.of_string "customer:nobody");
  [%expect
    {|
    (Error
     ("statements cover an account, not its pending sub-account"
      (account customer:alice/pending)))
    (Error
     ("account not opened by the end of the period" (account customer:nobody)))
    |}]
;;
