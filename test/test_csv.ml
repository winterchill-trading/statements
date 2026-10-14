open! Core
open Statements
open Helpers

let statement basis =
  Statement.create
    (records (september ()))
    ~account:alice
    ~currency:Money.Currency.eur
    ~period:september_period
    ~basis
    ~now
    ~annotate:(fun e ->
      { (Annotation.default e) with
        category =
          (match e.code with
           | Card_settlement | Card_auth -> Some "shopping"
           | _ -> None)
      ; counterparty =
          (if String.equal e.remittance_info "Top-up"
           then Some "=HYPERLINK(\"x\")"
           else None)
      })
  |> ok_exn
;;

(* CRLF shown as "\r\n" markers; the BOM as <BOM>. *)
let show csv =
  let csv =
    match String.chop_prefix csv ~prefix:"\xEF\xBB\xBF" with
    | Some rest -> "<BOM>" ^ rest
    | None -> csv
  in
  print_string (String.substr_replace_all csv ~pattern:"\r\n" ~with_:"\\r\\n\n")
;;

let%expect_test "netted statement" =
  show (Csv.render (statement Netted));
  [%expect
    {|
    <BOM>entry_ref,booking_date,value_time,type,status,direction,amount,currency,balance,reversal,reference,description,counterparty,category,hold,authorised_at\r\n
    8.0,2026-09-01,2026-09-01T08:00:00Z,DEPOSIT,BOOKED,IN,250.00,EUR,750.00,false,dep-2,Top-up,"'=HYPERLINK(""x"")",,,\r\n
    11.0,2026-09-05,2026-09-05T09:00:00Z,CARD_SETTLEMENT,BOOKED,OUT,-45.50,EUR,704.50,false,auth-1,Coffee Corner,,shopping,auth-1,2026-09-03T09:00:00Z\r\n
    13.0,2026-09-10,2026-09-10T09:00:00Z,WITHDRAWAL,BOOKED,OUT,-100.00,EUR,604.50,false,out-1,"To savings, ""Ltd""",,,,\r\n
    14.0,2026-09-12,2026-09-12T09:00:00Z,REVERSAL,BOOKED,IN,100.00,EUR,704.50,true,out-1,,,,,\r\n
    ,2026-09-29,2026-09-29T09:00:00Z,CARD_AUTH,PENDING,OUT,-20.00,EUR,,false,auth-3,Bookshop,,shopping,auth-3,2026-09-29T09:00:00Z\r\n
    |}]
;;

let%expect_test "posting basis with a Russian preamble" =
  show (Csv.render ~preamble:(Catalogue.builtin Ru) (statement Posting));
  [%expect
    {|
    <BOM>Выписка по счёту,,,,,,,,,,,,,,,\r\n
    Счёт,customer:alice,,,,,,,,,,,,,,\r\n
    Валюта,EUR,,,,,,,,,,,,,,\r\n
    Период,2026-09-01,2026-09-30,,,,,,,,,,,,,\r\n
    По состоянию на,2026-10-01T00:00:00Z,,,,,,,,,,,,,,\r\n
    Входящий остаток,500.00,,,,,,,,,,,,,,\r\n
    Поступления,474.50,,,,,,,,,,,,,,\r\n
    Списания,290.00,,,,,,,,,,,,,,\r\n
    Исходящий остаток,684.50,,,,,,,,,,,,,,\r\n
    "Указаны все проводки, включая блокировки и их снятие.",,,,,,,,,,,,,,,\r\n
    entry_ref,booking_date,value_time,type,status,direction,amount,currency,balance,reversal,reference,description,counterparty,category,hold,authorised_at\r\n
    8.0,2026-09-01,2026-09-01T08:00:00Z,DEPOSIT,BOOKED,IN,250.00,EUR,750.00,false,dep-2,Top-up,"'=HYPERLINK(""x"")",,,\r\n
    9.0,2026-09-03,2026-09-03T09:00:00Z,CARD_AUTH,BOOKED,OUT,-50.00,EUR,700.00,false,auth-1,Coffee Corner,,shopping,auth-1,\r\n
    10.0,2026-09-04,2026-09-04T09:00:00Z,CARD_AUTH,BOOKED,OUT,-120.00,EUR,580.00,false,auth-2,Hotel,,shopping,auth-2,\r\n
    11.0,2026-09-05,2026-09-05T09:00:00Z,CARD_SETTLEMENT,BOOKED,IN,4.50,EUR,584.50,false,auth-1,Coffee Corner,,shopping,auth-1,\r\n
    12.0,2026-09-06,2026-09-06T09:00:00Z,HOLD,BOOKED,IN,120.00,EUR,704.50,false,,Hotel,,shopping,auth-2,\r\n
    13.0,2026-09-10,2026-09-10T09:00:00Z,WITHDRAWAL,BOOKED,OUT,-100.00,EUR,604.50,false,out-1,"To savings, ""Ltd""",,,,\r\n
    14.0,2026-09-12,2026-09-12T09:00:00Z,REVERSAL,BOOKED,IN,100.00,EUR,704.50,true,out-1,,,,,\r\n
    16.0,2026-09-29,2026-09-29T09:00:00Z,CARD_AUTH,BOOKED,OUT,-20.00,EUR,684.50,false,auth-3,Bookshop,,shopping,auth-3,\r\n
    |}]
;;

(* A reader for the subset of RFC 4180 the writer produces. *)
let parse s =
  let records = ref []
  and fields = ref []
  and buf = Buffer.create 16 in
  let n = String.length s in
  let end_field () =
    fields := Buffer.contents buf :: !fields;
    Buffer.clear buf
  in
  let end_record () =
    end_field ();
    records := List.rev !fields :: !records;
    fields := []
  in
  let rec plain i =
    if i >= n
    then ()
    else (
      match s.[i] with
      | ',' ->
        end_field ();
        plain (i + 1)
      | '\r' when i + 1 < n && Char.equal s.[i + 1] '\n' ->
        end_record ();
        plain (i + 2)
      | '"' when Buffer.length buf = 0 -> quoted (i + 1)
      | c ->
        Buffer.add_char buf c;
        plain (i + 1))
  and quoted i =
    match s.[i] with
    | '"' when i + 1 < n && Char.equal s.[i + 1] '"' ->
      Buffer.add_char buf '"';
      quoted (i + 2)
    | '"' -> plain (i + 1)
    | c ->
      Buffer.add_char buf c;
      quoted (i + 1)
  in
  plain 0;
  List.rev !records
;;

let%test_unit "fields round-trip through an RFC 4180 reader" =
  Quickcheck.test
    (List.quickcheck_generator String.quickcheck_generator)
    ~sexp_of:[%sexp_of: string list]
    ~f:(fun fields ->
      let fields = if List.is_empty fields then [ "" ] else fields in
      [%test_result: string list list] (parse (Csv.record fields)) ~expect:[ fields ])
;;

let%test_unit "every record has one field per column" =
  List.iter Basis.all ~f:(fun basis ->
    List.iter Locale.all ~f:(fun locale ->
      let csv = Csv.render ~preamble:(Catalogue.builtin locale) (statement basis) in
      let body = String.chop_prefix_exn csv ~prefix:"\xEF\xBB\xBF" in
      List.iter (parse body) ~f:(fun r ->
        [%test_result: int] (List.length r) ~expect:(List.length Csv.columns))))
;;
