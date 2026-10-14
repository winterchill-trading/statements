(** WinAnsiEncoding (Windows code page 1252), the encoding PDF's standard fonts use for
    Latin text without embedding a font. *)

open! Core

(** The byte for a code point, if the encoding has one. U+202F (narrow no-break space)
    maps to the no-break space. *)
val encode : Uchar.t -> char option

(** Whether every code point of a UTF-8 string can be encoded. *)
val can_encode : string -> bool

(** The string's bytes in WinAnsiEncoding; a code point the encoding lacks becomes [?]. *)
val encode_string : string -> string

(** Advance width of a UTF-8 string in Helvetica (or Helvetica-Bold) at size 1. *)
val width : bold:bool -> string -> float
