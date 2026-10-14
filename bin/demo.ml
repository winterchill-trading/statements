open! Core
open Ledger

let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn
let id = Account_id.of_string
let bank = id "bank:central"
let network = id "external:sepa"
let cards = id "external:cards"
let account = id "customer:demo"
let at s = Time_ns.of_string_with_utc_offset (s ^ ":00Z")

let records () =
  let state = ref Balances.empty in
  let out = ref [] in
  let push event =
    let r = { Record.seq = Balances.next_seq !state; event } in
    state := Balances.apply !state r |> ok_exn;
    out := r :: !out
  in
  List.iter
    [ Account.create bank Asset ~allow_negative:true
    ; Account.create network Asset ~allow_negative:true
    ; Account.create cards Asset ~allow_negative:true
    ; Account.create account Liability
    ]
    ~f:(fun a -> push (Account_opened a));
  let transfer ~k ~when_ ~code ~info ~from ~to_ a =
    push
      (Posted
         (Entry.create
            ~idempotency_key:(Idempotency_key.of_string k)
            ~kind:(Transfer "transfer")
            ~code
            ~reference:k
            ~remittance_info:info
            ~effective_at:(at when_)
            ~recorded_at:(at when_)
            [ Posting.debit from (eur a); Posting.credit to_ (eur a) ]
          |> ok_exn))
  in
  let hold ~k ~when_ ~info a =
    push
      (Posted
         (Hold.place
            !state
            ~hold_id:(Hold_id.of_string k)
            ~account
            ~amount:(eur a)
            ~idempotency_key:(Idempotency_key.of_string k)
            ~code:Card_auth
            ~reference:k
            ~remittance_info:info
            ~effective_at:(at when_)
            ~recorded_at:(at when_)
            ()
          |> ok_exn))
  in
  let settle ~hold ~k ~when_ ?amount () =
    push
      (Posted
         (Hold.capture
            !state
            ~hold_id:(Hold_id.of_string hold)
            ?amount:(Option.map amount ~f:eur)
            ~release_remainder:true
            ~to_:cards
            ~idempotency_key:(Idempotency_key.of_string k)
            ~code:Card_settlement
            ~reference:hold
            ~effective_at:(at when_)
            ~recorded_at:(at when_)
            ()
          |> ok_exn))
  in
  let release ~hold ~k ~when_ =
    push
      (Posted
         (Hold.release
            !state
            ~hold_id:(Hold_id.of_string hold)
            ~idempotency_key:(Idempotency_key.of_string k)
            ~effective_at:(at when_)
            ~recorded_at:(at when_)
            ()
          |> ok_exn))
  in
  transfer
    ~k:"salary-08"
    ~when_:"2026-08-28 08:00"
    ~code:Deposit
    ~info:"Salary"
    ~from:bank
    ~to_:account
    "2450.00";
  transfer
    ~k:"rent-09"
    ~when_:"2026-09-01 07:30"
    ~code:Withdrawal
    ~info:"Rent September"
    ~from:account
    ~to_:network
    "980.00";
  hold ~k:"card-0001" ~when_:"2026-09-02 12:14" ~info:"Coffee Corner" "4.80";
  settle ~hold:"card-0001" ~k:"settle-0001" ~when_:"2026-09-03 02:00" ();
  hold ~k:"card-0002" ~when_:"2026-09-05 18:40" ~info:"Grand Hotel" "300.00";
  settle ~hold:"card-0002" ~k:"settle-0002" ~when_:"2026-09-08 02:00" ~amount:"262.40" ();
  hold ~k:"card-0003" ~when_:"2026-09-09 10:02" ~info:"Fuel station" "120.00";
  release ~hold:"card-0003" ~k:"release-0003" ~when_:"2026-09-09 10:20";
  transfer
    ~k:"pay-0004"
    ~when_:"2026-09-12 09:00"
    ~code:Withdrawal
    ~info:"Invoice 2026-118"
    ~from:account
    ~to_:network
    "150.00";
  transfer
    ~k:"pay-0004-return"
    ~when_:"2026-09-15 09:00"
    ~code:Return
    ~info:"Invoice 2026-118"
    ~from:network
    ~to_:account
    "150.00";
  transfer
    ~k:"p2p-0005"
    ~when_:"2026-09-18 19:30"
    ~code:Deposit
    ~info:"Dinner"
    ~from:bank
    ~to_:account
    "42.50";
  for i = 1 to 24 do
    let k = sprintf "card-1%03d" i in
    hold
      ~k
      ~when_:(sprintf "2026-09-%02d 12:%02d" (19 + (i / 4)) (i * 2))
      ~info:"Grocer"
      (sprintf "%d.%02d" (5 + i) (i * 7 mod 100));
    settle
      ~hold:k
      ~k:("settle-" ^ k)
      ~when_:(sprintf "2026-09-%02d 12:%02d" (19 + (i / 4)) ((i * 2) + 1))
      ()
  done;
  transfer
    ~k:"fee-09"
    ~when_:"2026-09-30 23:00"
    ~code:Fee
    ~info:""
    ~from:account
    ~to_:bank
    "2.00";
  hold ~k:"card-0006" ~when_:"2026-09-30 23:15" ~info:"Bookshop" "18.90";
  transfer
    ~k:"salary-10"
    ~when_:"2026-10-01 08:00"
    ~code:Deposit
    ~info:"Salary"
    ~from:bank
    ~to_:account
    "2450.00";
  List.rev !out
;;

let annotate (e : Entry.t) : Statements.Annotation.t =
  let base = Statements.Annotation.default e in
  match Idempotency_key.to_string e.idempotency_key with
  | "p2p-0005" ->
    { base with counterparty = Some "Айгүл Асанова"; description = "Ужин / Dinner" }
  | k when String.is_prefix k ~prefix:"settle-card-1" ->
    { base with description = "Grocer"; category = Some "groceries" }
  | _ -> base
;;
