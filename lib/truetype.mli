(** Reading a TrueType font and cutting it down to the glyphs a document uses.

    Enough of the format for embedding in PDF: the table directory, [head], [hhea],
    [hmtx], [maxp], [loca], [glyf], the Unicode [cmap] (formats 4 and 12) and the [OS/2]
    and [post] fields a font descriptor needs.

    {!subset} keeps every glyph id and empties the glyphs not asked for (and not used by a
    kept composite glyph), so a PDF can address glyphs by their original ids with an
    identity mapping. The result keeps only the tables a PDF viewer reads for a CID-keyed
    TrueType font ([glyf], [head], [hhea], [hmtx], [loca], [maxp], and the hinting tables
    [cvt ], [fpgm], [prep] when present). *)

open! Core

type t

(** Fails on a file that is not a TrueType ([glyf]-based) font or lacks a table above. A
    font without a [cmap] (such as a {!subset}) parses with no code points mapped. *)
val parse : string -> t Or_error.t

val units_per_em : t -> int
val num_glyphs : t -> int

(** Typographic ascender and descender (descender negative), in font units. *)
val ascender : t -> int

val descender : t -> int

(** [OS/2] capital height in font units, or the ascender when the table is too old. *)
val cap_height : t -> int

(** [xMin, yMin, xMax, yMax] from [head]. *)
val bbox : t -> int * int * int * int

(** [post] italic angle in degrees. *)
val italic_angle : t -> float

(** The glyph for a code point, if the font has one. *)
val glyph : t -> Uchar.t -> int option

(** Advance width of a glyph in font units. *)
val advance : t -> int -> int

(** The font with only these glyphs (plus glyph 0 and composite components) left. *)
val subset : t -> int list -> string

(** Glyph ids whose outlines are not empty in a font file: for tests. *)
val non_empty_glyphs : t -> int list

(** The bundled Noto Sans Regular (Latin, Greek, Cyrillic; SIL Open Font License 1.1),
    parsed once. *)
val bundled : unit -> t
