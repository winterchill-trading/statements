open! Core

(* Code points of bytes 0x80 to 0x9F, where cp1252 differs from Latin-1. *)
let high =
  [ 0x20AC, 0x80
  ; 0x201A, 0x82
  ; 0x0192, 0x83
  ; 0x201E, 0x84
  ; 0x2026, 0x85
  ; 0x2020, 0x86
  ; 0x2021, 0x87
  ; 0x02C6, 0x88
  ; 0x2030, 0x89
  ; 0x0160, 0x8A
  ; 0x2039, 0x8B
  ; 0x0152, 0x8C
  ; 0x017D, 0x8E
  ; 0x2018, 0x91
  ; 0x2019, 0x92
  ; 0x201C, 0x93
  ; 0x201D, 0x94
  ; 0x2022, 0x95
  ; 0x2013, 0x96
  ; 0x2014, 0x97
  ; 0x02DC, 0x98
  ; 0x2122, 0x99
  ; 0x0161, 0x9A
  ; 0x203A, 0x9B
  ; 0x0153, 0x9C
  ; 0x017E, 0x9E
  ; 0x0178, 0x9F
  ]
  |> Int.Map.of_alist_exn
;;

let encode u =
  match Uchar.to_scalar u with
  | c when (c >= 0x20 && c < 0x7F) || (c >= 0xA0 && c <= 0xFF) -> Some (Char.of_int_exn c)
  | 0x202F -> Some '\xA0'
  | c -> Option.map (Map.find high c) ~f:Char.of_int_exn
;;

let can_encode s = List.for_all (Utf8.uchars s) ~f:(fun u -> Option.is_some (encode u))

let encode_string s =
  String.of_list
    (List.map (Utf8.uchars s) ~f:(fun u -> Option.value (encode u) ~default:'?'))
;;

let width ~bold s =
  let table = if bold then Helvetica_widths.bold else Helvetica_widths.regular in
  List.sum
    (module Float)
    (String.to_list (encode_string s))
    ~f:(fun c -> Float.of_int table.(Char.to_int c - 32) /. 1000.)
;;
