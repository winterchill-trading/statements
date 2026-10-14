open! Core
open Statements

let amounts = [ "0.05"; "-12.00"; "1234.56"; "-12345.67"; "1234567.89" ]

let%expect_test "numbers, dates and periods in every language" =
  let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> ok_exn in
  let month = Period.of_month { year = 2026; month = Oct } in
  let days =
    Period.of_days
      ~from_date:(Date.of_string "2026-01-05")
      ~to_date:(Date.of_string "2026-03-01")
    |> ok_exn
  in
  List.iter Locale.all ~f:(fun locale ->
    let c = Catalogue.builtin locale in
    (* Show no-break spaces as '_' so they are visible. *)
    let v s = String.substr_replace_all s ~pattern:"\u{00A0}" ~with_:"_" in
    printf
      "%s | %s | %s\n   %s | %s | %s | %s\n"
      (Locale.to_string locale)
      (String.concat
         ~sep:"  "
         (List.map amounts ~f:(fun a -> v (Catalogue.format_amount c (eur a)))))
      (v (Catalogue.format_amount ~currency:true c (eur "1234.5")))
      (Catalogue.format_date c (Date.of_string "2026-10-04"))
      (Catalogue.format_time c (Time_ns.of_string_with_utc_offset "2026-10-14 09:05:59Z"))
      (Catalogue.format_period c month)
      (Catalogue.format_period c days));
  [%expect
    {|
    en | 0.05  -12.00  1,234.56  -12,345.67  1,234,567.89 | EUR 1,234.50
       4 Oct 2026 | 14 Oct 2026, 09:05 UTC | October 2026 | 5 Jan 2026 – 1 Mar 2026
    de | 0,05  -12,00  1.234,56  -12.345,67  1.234.567,89 | 1.234,50 EUR
       04.10.2026 | 14.10.2026, 09:05 UTC | Oktober 2026 | 05.01.2026 – 01.03.2026
    fr | 0,05  -12,00  1_234,56  -12_345,67  1_234_567,89 | 1_234,50 EUR
       04/10/2026 | 14/10/2026, 09:05 UTC | octobre 2026 | 05/01/2026 – 01/03/2026
    es | 0,05  -12,00  1234,56  -12.345,67  1.234.567,89 | 1234,50 EUR
       04/10/2026 | 14/10/2026, 09:05 UTC | octubre de 2026 | 05/01/2026 – 01/03/2026
    ru | 0,05  -12,00  1_234,56  -12_345,67  1_234_567,89 | 1_234,50 EUR
       04.10.2026 | 14.10.2026, 09:05 UTC | октябрь 2026 | 05.01.2026 – 01.03.2026
    ky | 0,05  -12,00  1_234,56  -12_345,67  1_234_567,89 | 1_234,50 EUR
       04.10.2026 | 14.10.2026, 09:05 UTC | 2026-ж., октябрь | 05.01.2026 – 01.03.2026
    |}]
;;

let%expect_test "templates and code labels" =
  List.iter Locale.all ~f:(fun locale ->
    let c = Catalogue.builtin locale in
    printf
      "%s: %s / %s / %s\n"
      (Locale.to_string locale)
      (Catalogue.render c Page [ "page", "2"; "pages", "3" ])
      (Catalogue.render c Authorised_on [ "date", "03.09.2026" ])
      (Catalogue.code_label c Card_settlement));
  [%expect
    {|
    en: Page 2 of 3 / authorised 03.09.2026 / Card payment
    de: Seite 2 von 3 / autorisiert am 03.09.2026 / Kartenzahlung
    fr: Page 2 sur 3 / autorisé le 03.09.2026 / Paiement par carte
    es: Página 2 de 3 / autorizado el 03.09.2026 / Pago con tarjeta
    ru: Страница 2 из 3 / авторизовано 03.09.2026 / Оплата картой
    ky: Барак 2 / 3 / 03.09.2026 уруксат берилген / Карта менен төлөм
    |}]
;;

let%expect_test "overrides keep placeholders" =
  let c = Catalogue.builtin En in
  let show r =
    print_s
      [%sexp (Or_error.map r ~f:(fun c -> Catalogue.text c Page) : string Or_error.t)]
  in
  show (Catalogue.with_text c Page "p. {page}/{pages}");
  show (Catalogue.with_text c Page "Page {page}");
  show (Catalogue.with_text c Statement_title "Statement {date}" |> Or_error.map ~f:Fn.id);
  [%expect
    {|
    (Ok "p. {page}/{pages}")
    (Error
     ("text does not fit its label" (label Page) (text "Page {page}")
      (expected (page pages))))
    (Error
     ("text does not fit its label" (label Statement_title)
      (text "Statement {date}") (expected ())))
    |}]
;;

let%expect_test "every built-in text is distinct from English where the language differs" =
  (* A guard against a row pasted into the wrong column: count labels whose text is the
     same as English. Some are legitimately equal (Date in French, Fee loanwords). *)
  let en = Catalogue.builtin En in
  List.iter Locale.all ~f:(fun locale ->
    let c = Catalogue.builtin locale in
    let same =
      List.filter Catalogue.Label.all ~f:(fun l ->
        String.equal (Catalogue.text c l) (Catalogue.text en l))
    in
    if not (Locale.equal locale En)
    then print_s [%sexp (locale : Locale.t), (same : Catalogue.Label.t list)]);
  [%expect
    {|
    (De ())
    (Fr (Date Code_adjustment))
    (Es ())
    (Ru ())
    (Ky ())
    |}]
;;
