(** Decoding UTF-8 text into code points. *)

open! Core

(** The code points of [s]; a malformed sequence becomes U+FFFD. *)
val uchars : string -> Uchar.t list
