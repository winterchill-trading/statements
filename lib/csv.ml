open! Core
module Amount = Money.Amount

let columns =
  [ "entry_ref"
  ; "booking_date"
  ; "value_time"
  ; "type"
  ; "status"
  ; "direction"
  ; "amount"
  ; "currency"
  ; "balance"
  ; "reversal"
  ; "reference"
  ; "description"
  ; "counterparty"
  ; "category"
  ; "hold"
  ; "authorised_at"
  ]
;;

let field s =
  if String.exists s ~f:(function
       | ',' | '"' | '\r' | '\n' -> true
       | _ -> false)
  then "\"" ^ String.substr_replace_all s ~pattern:"\"" ~with_:"\"\"" ^ "\""
  else s
;;

let record fields = String.concat ~sep:"," (List.map fields ~f:field) ^ "\r\n"

(* OWASP's advice for CSV injection: neutralise a leading formula character in text a
   third party controls. *)
let text s =
  match String.get s 0 with
  | '=' | '+' | '-' | '@' | '\t' | '\r' -> "'" ^ s
  | _ -> s
  | exception _ -> s
;;

(* [to_string_iso8601_basic] prints nanoseconds and "Z"; trim to whole seconds when there
   is no fraction, which is the common case and easier to read. *)
let time t =
  let s = Time_ns.to_string_iso8601_basic t ~zone:Time_float.Zone.utc in
  match String.chop_suffix s ~suffix:".000000000Z" with
  | Some whole -> whole ^ "Z"
  | None -> s
;;

let amount a = Amount.to_decimal_string a
let opt f = Option.value_map ~default:"" ~f

let line_row (s : Statement.t) (l : Statement.Line.t) =
  [ Statement.Line.entry_ref l
  ; Date.to_string l.business_date
  ; time l.effective_at
  ; Ledger.Entry.Code.to_string l.code
  ; "BOOKED"
  ; Statement.Direction.to_string l.direction
  ; amount (Statement.Line.signed l)
  ; Money.Currency.to_string s.currency
  ; amount l.balance
  ; Bool.to_string l.reversal
  ; text l.reference
  ; text l.annotation.description
  ; opt text l.annotation.counterparty
  ; opt text l.annotation.category
  ; opt Ledger.Hold_id.to_string l.hold
  ; opt time l.held_since
  ]
;;

let pending_row (s : Statement.t) (p : Statement.Pending.t) =
  [ ""
  ; Date.to_string p.business_date
  ; time p.placed_at
  ; Ledger.Entry.Code.to_string p.code
  ; "PENDING"
  ; "OUT"
  ; amount (Amount.neg p.amount)
  ; Money.Currency.to_string s.currency
  ; ""
  ; "false"
  ; text p.reference
  ; text p.annotation.description
  ; opt text p.annotation.counterparty
  ; opt text p.annotation.category
  ; Ledger.Hold_id.to_string p.hold
  ; time p.placed_at
  ]
;;

let preamble_rows (c : Catalogue.t) (s : Statement.t) =
  let t = Catalogue.text c in
  [ [ t Statement_title ]
  ; [ t Account; Ledger.Account_id.to_string s.account ]
  ; [ t Currency; Money.Currency.to_string s.currency ]
  ; [ t Period; Date.to_string s.period.from_date; Date.to_string s.period.to_date ]
  ; [ t As_of; time s.as_of ]
  ; [ t Opening_balance; amount s.totals.opening ]
  ; [ t Money_in; amount s.totals.money_in ]
  ; [ t Money_out; amount s.totals.money_out ]
  ; [ t Closing_balance; amount s.totals.closing ]
  ]
  @
  match s.basis with
  | Netted ->
    [ [ t Held_for_pending; amount s.held_for_pending ]
    ; [ t Available_at_end; amount s.ledger_balance_at_end ]
    ; [ t Basis_netted ]
    ]
  | Posting -> [ [ t Basis_posting ] ]
;;

let pad row = row @ List.init (List.length columns - List.length row) ~f:(fun _ -> "")

let render ?preamble (s : Statement.t) =
  let buf = Buffer.create 4096 in
  (* UTF-8 byte order mark *)
  Buffer.add_string buf "\xEF\xBB\xBF";
  Option.iter preamble ~f:(fun c ->
    List.iter (preamble_rows c s) ~f:(fun row -> Buffer.add_string buf (record (pad row))));
  Buffer.add_string buf (record columns);
  List.iter s.lines ~f:(fun l -> Buffer.add_string buf (record (line_row s l)));
  List.iter s.pending ~f:(fun p -> Buffer.add_string buf (record (pending_row s p)));
  Buffer.contents buf
;;
