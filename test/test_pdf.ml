open! Core
open Statements
open Helpers

let september_statement ?(records = records (september ())) basis =
  Statement.create
    records
    ~account:alice
    ~currency:Money.Currency.eur
    ~period:september_period
    ~basis
    ~now
  |> ok_exn
;;

let header =
  Pdf.Header.create
    ~institution:"Example Bank"
    ~holder:"Alex Example"
    ~account_name:"Main account"
    ()
;;

(* Structural checks a PDF reader relies on: every xref entry points at its object, the
   trailer's startxref points at the table, and every stream's /Length is exact. *)
let check_structure pdf =
  let fail msg = raise_s [%message "bad PDF structure" msg] in
  if not (String.is_prefix pdf ~prefix:"%PDF-1.4\n") then fail "header";
  if not (String.is_suffix pdf ~suffix:"%%EOF\n") then fail "trailer end";
  let startxref =
    let i =
      String.substr_index_all pdf ~may_overlap:false ~pattern:"startxref\n"
      |> List.last_exn
    in
    let rest = String.drop_prefix pdf (i + 10) in
    Int.of_string (String.take_while rest ~f:Char.is_digit)
  in
  if not (String.is_substring_at pdf ~pos:startxref ~substring:"xref\n0 ")
  then fail "startxref";
  let after = String.drop_prefix pdf (startxref + 7) in
  let count = Int.of_string (String.take_while after ~f:Char.is_digit) in
  let table = startxref + 7 + String.length (Int.to_string count) + 1 in
  for i = 1 to count - 1 do
    let entry = String.sub pdf ~pos:(table + (20 * i)) ~len:20 in
    if not (String.is_suffix entry ~suffix:" 00000 n \n") then fail "xref entry";
    let offset = Int.of_string (String.prefix entry 10) in
    if not (String.is_substring_at pdf ~pos:offset ~substring:(sprintf "%d 0 obj\n" i))
    then fail (sprintf "object %d not at its offset" i)
  done;
  List.iter
    (String.substr_index_all pdf ~may_overlap:false ~pattern:"<< /Length ")
    ~f:(fun i ->
      let rest = String.drop_prefix pdf (i + 11) in
      let length = Int.of_string (String.take_while rest ~f:Char.is_digit) in
      let start = String.substr_index_exn ~pos:i pdf ~pattern:">>\nstream\n" + 10 in
      if not (String.is_substring_at pdf ~pos:(start + length) ~substring:"\nendstream")
      then fail "stream length");
  count - 1
;;

let count_pages pdf =
  let i = String.substr_index_exn pdf ~pattern:"/Type /Pages" in
  let rest =
    String.drop_prefix pdf (String.substr_index_exn ~pos:i pdf ~pattern:"/Count " + 7)
  in
  Int.of_string (String.take_while rest ~f:Char.is_digit)
;;

let has pdf s = String.is_substring pdf ~substring:s

let%expect_test "English on the standard fonts" =
  let pdf = Pdf.render ~header (september_statement Netted) in
  let objects = check_structure pdf in
  print_s
    [%message
      ""
        (objects : int)
        ~pages:(count_pages pdf : int)
        ~helvetica:(has pdf "/BaseFont /Helvetica " : bool)
        ~embedded:(has pdf "FontFile2" : bool)
        ~title:(has pdf "(Account statement)" : bool)
        ~pending:(has pdf "(Pending, not included in the balances)" : bool)
        ~mark:(has pdf "(Simulated statement, not a real bank document)" : bool)
        ~footer:(has pdf "(Page 1 of 1)" : bool)
        ~deterministic:
          (String.equal pdf (Pdf.render ~header (september_statement Netted)) : bool)];
  [%expect
    {|
    ((objects 8) (pages 1) (helvetica true) (embedded false) (title true)
     (pending true) (mark true) (footer true) (deterministic true))
    |}]
;;

let%expect_test "Russian embeds a subset of the bundled font" =
  let pdf =
    Pdf.render ~catalogue:(Catalogue.builtin Ru) ~header (september_statement Netted)
  in
  let objects = check_structure pdf in
  print_s
    [%message
      ""
        (objects : int)
        ~helvetica:(has pdf "/BaseFont /Helvetica" : bool)
        ~embedded:(has pdf "/Subtype /CIDFontType2" : bool)
        ~to_unicode:(has pdf "/ToUnicode" : bool)
        ~small:(String.length pdf < 120_000 : bool)];
  [%expect
    {|
    ((objects 11) (helvetica false) (embedded true) (to_unicode true)
     (small true))
    |}]
;;

let%expect_test "marks" =
  let s = september_statement Posting in
  let no_mark = Pdf.render ~mark:No_mark ~header s in
  let custom = Pdf.render ~mark:(Text "SPECIMEN") ~header s in
  ignore (check_structure no_mark : int);
  print_s
    [%message
      ""
        ~default_mark_absent:(not (has no_mark "Simulated statement") : bool)
        ~custom:(has custom "(SPECIMEN)" : bool)
        ~posting_note:
          (has custom "(Every posting is listed, including funds reserved and released.)"
           : bool)
        ~no_pending_section:(not (has custom "(Pending, not included") : bool)];
  [%expect
    {|
    ((default_mark_absent true) (custom true) (posting_note true)
     (no_pending_section true))
    |}]
;;

let%expect_test "long statements break across pages with the header repeated" =
  let t = create () in
  deposit t ~k:"start" ~when_:(at "2026-08-31") "10000.00";
  for i = 1 to 120 do
    pay_out
      t
      ~k:(sprintf "pay-%03d" i)
      ~when_:(Time_ns.add (at "2026-09-01") (Time_ns.Span.of_hr (Float.of_int (i * 5))))
      ~info:
        (sprintf
           "Payment %d with a description long enough to be cut at the column edge"
           i)
      "12.34"
  done;
  let pdf = Pdf.render ~header (september_statement ~records:(records t) Netted) in
  ignore (check_structure pdf : int);
  let pages = count_pages pdf in
  print_s
    [%message
      ""
        (pages : int)
        ~last_footer:(has pdf (sprintf "(Page %d of %d)" pages pages) : bool)
        ~headers:
          (List.length
             (String.substr_index_all pdf ~may_overlap:false ~pattern:"(Money out) Tj")
           : int)
        ~ellipsis:(has pdf "\\205) Tj" : bool)];
  [%expect {| ((pages 5) (last_footer true) (headers 6) (ellipsis true)) |}]
;;
