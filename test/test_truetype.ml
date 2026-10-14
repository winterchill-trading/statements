open! Core
open Statements

let font = Truetype.parse Noto_sans_data.data |> ok_exn
let glyph c = Truetype.glyph font (Uchar.of_scalar_exn c)

let%expect_test "metrics and code points of the bundled font" =
  print_s
    [%message
      ""
        ~units_per_em:(Truetype.units_per_em font : int)
        ~glyphs:(Truetype.num_glyphs font : int)
        ~ascender:(Truetype.ascender font : int)
        ~descender:(Truetype.descender font : int)
        ~cap_height:(Truetype.cap_height font : int)
        ~bbox:(Truetype.bbox font : int * int * int * int)];
  List.iter [ 'A'; ' ' ] ~f:(fun c ->
    let g = glyph (Char.to_int c) |> Option.value_exn in
    printf "%C glyph %d advance %d\n" c g (Truetype.advance font g));
  (* Kyrgyz letters beyond the Russian alphabet, the euro and a code point the font lacks *)
  List.iter [ 0x04A3; 0x04E9; 0x04AF; 0x20AC; 0x1F600 ] ~f:(fun c ->
    printf "U+%04X %s\n" c (Option.value_map (glyph c) ~default:"none" ~f:Int.to_string));
  [%expect
    {|
    ((units_per_em 1000) (glyphs 4515) (ascender 1069) (descender -293)
     (cap_height 714) (bbox (-621 -508 2800 1067)))
    'A' glyph 36 advance 639
    ' ' glyph 3 advance 260
    U+04A3 645
    U+04E9 715
    U+04AF 657
    U+20AC 538
    U+1F600 none
    |}]
;;

(* The sum of a font file's 32-bit words, which a valid head adjustment makes 0xB1B0AFBA. *)
let file_checksum s =
  let s = s ^ String.make ((4 - (String.length s mod 4)) mod 4) '\000' in
  let sum = ref 0 in
  for i = 0 to (String.length s / 4) - 1 do
    let w = ref 0 in
    for j = 0 to 3 do
      w := (!w lsl 8) lor Char.to_int s.[(4 * i) + j]
    done;
    sum := (!sum + !w) land 0xFFFF_FFFF
  done;
  !sum
;;

let%expect_test "a subset keeps glyph ids and only the glyphs asked for" =
  let wanted = List.filter_map [ 0x41; 0x0416; 0x04E9; 0x00E9 ] ~f:glyph in
  let sub = Truetype.subset font wanted in
  let parsed = Truetype.parse sub |> ok_exn in
  print_s
    [%message
      ""
        ~bytes:(String.length sub < 60_000 : bool)
        ~same_glyph_count:(Truetype.num_glyphs parsed = Truetype.num_glyphs font : bool)
        ~same_advance:
          (List.for_all wanted ~f:(fun g ->
             Truetype.advance parsed g = Truetype.advance font g)
           : bool)
        ~checksum:(sprintf "%08X" (file_checksum sub))];
  (* é may be a composite of e and the accent: its parts survive too. *)
  print_s [%sexp (Truetype.non_empty_glyphs parsed : int list), (wanted : int list)];
  [%expect
    {|
    ((bytes true) (same_glyph_count true) (same_advance true)
     (checksum B1B0AFBA))
    ((0 36 72 118 171 430 604 715) (36 430 715 171))
    |}]
;;
