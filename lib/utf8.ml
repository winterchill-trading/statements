open! Core

let uchars s =
  let rec go i acc =
    if i >= String.length s
    then List.rev acc
    else (
      let d = Stdlib.String.get_utf_8_uchar s i in
      go (i + Stdlib.Uchar.utf_decode_length d) (Stdlib.Uchar.utf_decode_uchar d :: acc))
  in
  go 0 []
;;

let of_uchars us =
  let b = Buffer.create (List.length us) in
  List.iter us ~f:(Stdlib.Buffer.add_utf_8_uchar b);
  Buffer.contents b
;;
