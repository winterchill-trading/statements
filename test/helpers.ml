open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn
let id = Account_id.of_string
let key = Idempotency_key.of_string
let bank = id "bank:central"
let network = id "external:sepa"
let cards = id "external:cards"
let alice = id "customer:alice"
let bob = id "customer:bob"

(* Fixed instants in UTC so expect output does not depend on the machine's zone. *)
let at ?(hour = 9) date =
  Time_ns.of_string_with_utc_offset (sprintf "%s %02d:00:00Z" date hour)
;;

(* A journal under construction: the records so far and the state they fold to. *)
type t =
  { mutable state : Balances.t
  ; mutable records_rev : Record.t list
  }

let push t event =
  let r = { Record.seq = Balances.next_seq t.state; event } in
  t.state <- Balances.apply t.state r |> ok_exn;
  t.records_rev <- r :: t.records_rev
;;

let records t = List.rev t.records_rev

let create () =
  let t = { state = Balances.empty; records_rev = [] } in
  List.iter
    [ Account.create bank Asset ~allow_negative:true
    ; Account.create network Asset ~allow_negative:true
    ; Account.create cards Asset ~allow_negative:true
    ; Account.create alice Liability
    ; Account.create bob Liability
    ]
    ~f:(fun a -> push t (Account_opened a));
  t
;;

let post t entry = push t (Posted (ok_exn entry))

let transfer
  t
  ~k
  ~when_
  ?(code = Entry.Code.Transfer_intraledger)
  ?(info = "")
  ~from
  ~to_
  a
  =
  post
    t
    (Entry.create
       ~idempotency_key:(key k)
       ~kind:(Transfer "transfer")
       ~code
       ~reference:k
       ~remittance_info:info
       ~effective_at:when_
       ~recorded_at:when_
       [ Posting.debit from (eur a); Posting.credit to_ (eur a) ])
;;

let deposit t ~k ~when_ ?(info = "") ?(to_ = alice) a =
  transfer t ~k ~when_ ~code:Deposit ~info ~from:bank ~to_ a
;;

let pay_out t ~k ~when_ ?(info = "") ?(from = alice) a =
  transfer t ~k ~when_ ~code:Withdrawal ~info ~from ~to_:network a
;;

let authorise t ~hold ~k ~when_ ?(info = "") ?(account = alice) a =
  post
    t
    (Hold.place
       t.state
       ~hold_id:(Hold_id.of_string hold)
       ~account
       ~amount:(eur a)
       ~idempotency_key:(key k)
       ~code:Card_auth
       ~reference:hold
       ~remittance_info:info
       ~effective_at:when_
       ~recorded_at:when_
       ())
;;

let settle t ~hold ~k ~when_ ?amount ?(release_remainder = false) ?(info = "") () =
  post
    t
    (Hold.capture
       t.state
       ~hold_id:(Hold_id.of_string hold)
       ?amount:(Option.map amount ~f:eur)
       ~release_remainder
       ~to_:cards
       ~idempotency_key:(key k)
       ~code:Card_settlement
       ~reference:hold
       ~remittance_info:info
       ~effective_at:when_
       ~recorded_at:when_
       ())
;;

let release t ~hold ~k ~when_ =
  post
    t
    (Hold.release
       t.state
       ~hold_id:(Hold_id.of_string hold)
       ~idempotency_key:(key k)
       ~effective_at:when_
       ~recorded_at:when_
       ())
;;

let reverse t ~of_ ~k ~when_ =
  post
    t
    (Reversal.create
       t.state
       ~reverses:(key of_)
       ~idempotency_key:(key k)
       ~effective_at:when_
       ~recorded_at:when_
       ())
;;

(* A month with every kind of line: deposits, a card payment settled for less than
   authorised, a cancelled authorisation, one still open at the end of the month, a
   transfer out that came back, and activity before and after the month. *)
let september () =
  let t = create () in
  deposit t ~k:"dep-1" ~when_:(at "2026-08-28") ~info:"Salary August" "1200.00";
  pay_out t ~k:"rent-aug" ~when_:(at "2026-08-31") ~info:"Rent" "700.00";
  deposit t ~k:"dep-2" ~when_:(at "2026-09-01" ~hour:8) ~info:"Top-up" "250.00";
  authorise
    t
    ~hold:"auth-1"
    ~k:"auth-1"
    ~when_:(at "2026-09-03")
    ~info:"Coffee Corner"
    "50.00";
  authorise t ~hold:"auth-2" ~k:"auth-2" ~when_:(at "2026-09-04") ~info:"Hotel" "120.00";
  settle
    t
    ~hold:"auth-1"
    ~k:"settle-1"
    ~when_:(at "2026-09-05")
    ~amount:"45.50"
    ~release_remainder:true
    ~info:"Coffee Corner"
    ();
  release t ~hold:"auth-2" ~k:"release-2" ~when_:(at "2026-09-06");
  pay_out t ~k:"out-1" ~when_:(at "2026-09-10") ~info:"To savings, \"Ltd\"" "100.00";
  reverse t ~of_:"out-1" ~k:"out-1-returned" ~when_:(at "2026-09-12");
  deposit t ~k:"dep-bob" ~to_:bob ~when_:(at "2026-09-15") ~info:"Other customer" "5.00";
  authorise t ~hold:"auth-3" ~k:"auth-3" ~when_:(at "2026-09-29") ~info:"Bookshop" "20.00";
  settle t ~hold:"auth-3" ~k:"settle-3" ~when_:(at "2026-10-02") ~info:"Bookshop" ();
  deposit t ~k:"dep-3" ~when_:(at "2026-10-03") ~info:"Top-up" "10.00";
  t
;;

let september_period = Statements.Period.of_month { year = 2026; month = Sep }
let now = at "2026-10-14" ~hour:12

(* Prints a statement: totals, one row per line, then the pending items. *)
let show (s : Statements.Statement.t) =
  print_s
    [%message
      ""
        ~basis:(s.basis : Statements.Basis.t)
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
      (Statements.Statement.Line.entry_ref l)
      (Date.to_string l.business_date)
      (Ledger.Entry.Code.to_string l.code)
      (Statements.Statement.Direction.to_string l.direction)
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
