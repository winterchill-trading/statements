(** The fonts of one PDF document: the standard Helvetica pair, or the bundled Noto Sans
    embedded as a subset.

    The standard fonts need no embedding but only cover WinAnsiEncoding (Western European
    Latin). Text in any other script, such as Cyrillic, uses the embedded font; {!choose}
    picks the standard fonts when every string fits them. The embedded font has no bold
    face, so bold text is drawn with a thin outline over the fill.

    Text is measured with the font's own advance widths and kerning is not applied. *)

open! Core

type t

(** Helvetica and Helvetica-Bold, WinAnsiEncoding. *)
val standard : unit -> t

(** The bundled Noto Sans, as a CID-keyed TrueType font with a [ToUnicode] map so text can
    be searched and copied. *)
val embedded : unit -> t

(** [standard] when every string can be encoded in WinAnsiEncoding, else [embedded]. *)
val choose : string list -> t

(** Whether this is the embedded font. *)
val is_embedded : t -> bool

(** Width of a UTF-8 string at [size] points. *)
val width : t -> bold:bool -> size:float -> string -> float

(** Content-stream operators selecting the font: [/F1 9 Tf], plus the text render mode and
    line width for the drawn bold. *)
val select : t -> bold:bool -> size:float -> string

(** The [Tj] operand for a UTF-8 string, recording the glyphs used. Characters the font
    lacks show as [?] (standard) or the empty glyph (embedded). *)
val show : t -> string -> string

(** Writes the font objects (after every {!show}) and returns the [/Font] resource
    dictionary entries, e.g. [["/F1 5 0 R"; "/F2 6 0 R"]]. *)
val write : t -> Pdf_writer.t -> string list
