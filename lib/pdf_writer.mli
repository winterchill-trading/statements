(** A minimal PDF 1.4 file writer: numbered objects, streams, the cross-reference table
    and the trailer.

    Objects are given as PDF source text (["<< /Type /Page ... >>"]); the writer numbers
    them, records their byte offsets and writes a classic [xref] table. Streams are stored
    uncompressed. Output is deterministic: no dates or ids are added. *)

open! Core

type t

val create : unit -> t

(** A fresh object number, to refer to an object before its body is known (a page's
    parent, say). Every reserved number must be {!define}d before {!to_string}. *)
val reserve : t -> int

(** Gives the body of a reserved object. *)
val define : t -> int -> string -> unit

(** [reserve] then [define]. *)
val add : t -> string -> int

(** A stream object; [dict] holds extra dictionary entries (["/Length1 1234"]). [/Length]
    is added. *)
val add_stream : ?dict:string list -> t -> string -> int

(** ["12 0 R"]. *)
val ref_ : int -> string

(** The file. [root] is the catalog object, [info] the document information dictionary.
    Raises if a reserved object was never defined. *)
val to_string : t -> root:int -> ?info:int -> unit -> string

(** {2 Values} *)

(** A literal string, [(...)], with [\\], [(] and [)] escaped and bytes outside
    printable ASCII written as octal escapes. *)
val literal : string -> string

(** A hex string, [<...>]. *)
val hex : string -> string

(** A text string for the information dictionary or outlines: a literal for printable
    ASCII, otherwise UTF-16BE with a byte order mark, as a hex string. *)
val text_string : string -> string

(** A number with at most three decimals and no exponent: ["12"], ["0.5"], ["-3.125"]. *)
val number : float -> string
