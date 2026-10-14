open! Core
open Statements

let%expect_test "codes round-trip and subtags are ignored" =
  List.iter Locale.all ~f:(fun l ->
    assert (Locale.equal l (Locale.of_string_or_error (Locale.to_string l) |> ok_exn)));
  List.iter [ "EN"; "de-AT"; "ru_KG"; "ky"; "pt"; "" ] ~f:(fun s ->
    print_s [%sexp (s : string), (Locale.of_string_or_error s : Locale.t Or_error.t)]);
  [%expect
    {|
    (EN (Ok En))
    (de-AT (Ok De))
    (ru_KG (Ok Ru))
    (ky (Ok Ky))
    (pt (Error ("unsupported locale" pt)))
    ("" (Error ("unsupported locale" "")))
    |}]
;;

let%expect_test "resolve takes the first supported candidate, else English" =
  print_s [%sexp (Locale.resolve [ Some "it"; None; Some "fr-CH"; Some "de" ] : Locale.t)];
  print_s [%sexp (Locale.resolve [ None; Some "zz" ] : Locale.t)];
  [%expect {|
    Fr
    En
    |}]
;;
