(* A customer account with a deposit, a card payment that settles for less than was
   authorised, and one still pending at the end of the month; its October statement as
   totals, CSV and PDF. *)

open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn
let at s = Time_ns.of_string_with_utc_offset (s ^ " 09:00:00Z")
let customer = Account_id.of_string "customer:42"
let bank = Account_id.of_string "bank:central"
let cards = Account_id.of_string "external:cards"

let journal () =
  let state = ref Balances.empty in
  let records = ref [] in
  let push event =
    let r = { Record.seq = Balances.next_seq !state; event } in
    state := Balances.apply !state r |> ok_exn;
    records := r :: !records
  in
  let post entry = push (Posted (ok_exn entry)) in
  push (Account_opened (Account.create bank Asset ~allow_negative:true));
  push (Account_opened (Account.create cards Asset ~allow_negative:true));
  push (Account_opened (Account.create customer Liability));
  post
    (Entry.create
       ~idempotency_key:(Idempotency_key.of_string "salary-1")
       ~kind:(Transfer "deposit")
       ~code:Deposit
       ~remittance_info:"Salary"
       ~effective_at:(at "2026-10-01")
       ~recorded_at:(at "2026-10-01")
       [ Posting.debit bank (eur "1500.00"); Posting.credit customer (eur "1500.00") ]);
  let hold id amount date =
    post
      (Hold.place
         !state
         ~hold_id:(Hold_id.of_string id)
         ~account:customer
         ~amount:(eur amount)
         ~idempotency_key:(Idempotency_key.of_string id)
         ~code:Card_auth
         ~remittance_info:"Cafe"
         ~effective_at:(at date)
         ~recorded_at:(at date)
         ())
  in
  hold "auth-1" "30.00" "2026-10-05";
  post
    (Hold.capture
       !state
       ~hold_id:(Hold_id.of_string "auth-1")
       ~amount:(eur "27.50")
       ~release_remainder:true
       ~to_:cards
       ~idempotency_key:(Idempotency_key.of_string "settle-1")
       ~code:Card_settlement
       ~effective_at:(at "2026-10-07")
       ~recorded_at:(at "2026-10-07")
       ());
  hold "auth-2" "12.00" "2026-10-30";
  List.rev !records
;;

let () =
  let statement =
    Statements.Statement.create
      (journal ())
      ~account:customer
      ~currency:Money.Currency.eur
      ~period:(Statements.Period.of_month { year = 2026; month = Oct })
      ~basis:Netted
      ~now:(at "2026-11-02")
    |> ok_exn
  in
  let t = statement.totals in
  printf
    "opening %s, in %s, out %s, closing %s, held %s, available %s\n"
    (Money.Amount.to_string t.opening)
    (Money.Amount.to_string t.money_in)
    (Money.Amount.to_string t.money_out)
    (Money.Amount.to_string t.closing)
    (Money.Amount.to_string statement.held_for_pending)
    (Money.Amount.to_string statement.ledger_balance_at_end);
  let csv = Statements.Csv.render statement in
  print_string (String.substr_replace_all csv ~pattern:"\r\n" ~with_:"\n");
  let pdf =
    Statements.Pdf.render
      ~catalogue:(Statements.Catalogue.builtin De)
      ~header:(Statements.Pdf.Header.create ~account_name:"Girokonto" ())
      statement
  in
  printf
    "%s: %d bytes\n"
    (Statements.Statement.file_name statement ~prefix:"bank" ~extension:"pdf")
    (String.length pdf)
;;
