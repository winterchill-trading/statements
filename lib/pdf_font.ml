open! Core

type embedded =
  { font : Truetype.t
  ; used : Uchar.t Int.Table.t (** glyph id -> the first code point shown with it *)
  }

type t =
  | Standard
  | Embedded of embedded

let standard () = Standard

(* Parsed once per process: the bundled file is constant. *)
let noto = lazy (Truetype.parse Noto_sans_data.data |> ok_exn)
let embedded () = Embedded { font = force noto; used = Int.Table.create () }

let choose strings =
  if List.for_all strings ~f:Win_ansi.can_encode then standard () else embedded ()
;;

let is_embedded = function
  | Standard -> false
  | Embedded _ -> true
;;

let width t ~bold ~size s =
  match t with
  | Standard -> size *. Win_ansi.width ~bold s
  | Embedded { font; _ } ->
    let units =
      List.sum (module Int) (Utf8.uchars s) ~f:(fun u ->
        Option.value_map (Truetype.glyph font u) ~default:0 ~f:(Truetype.advance font))
    in
    size *. Float.of_int units /. Float.of_int (Truetype.units_per_em font)
;;

let select t ~bold ~size =
  match t with
  | Standard -> sprintf "/%s %s Tf" (if bold then "F2" else "F1") (Pdf_writer.number size)
  | Embedded _ ->
    (* Fill and stroke (render mode 2) with a line a thirtieth of the size wide thickens
       the strokes enough to read as bold. *)
    if bold
    then
      sprintf
        "/F1 %s Tf 2 Tr %s w"
        (Pdf_writer.number size)
        (Pdf_writer.number (size /. 30.))
    else sprintf "/F1 %s Tf 0 Tr" (Pdf_writer.number size)
;;

let show t s =
  match t with
  | Standard -> Pdf_writer.literal (Win_ansi.encode_string s)
  | Embedded { font; used } ->
    let gids =
      List.map (Utf8.uchars s) ~f:(fun u ->
        let g = Option.value (Truetype.glyph font u) ~default:0 in
        if g <> 0
        then Hashtbl.add used ~key:g ~data:u |> (ignore : [ `Ok | `Duplicate ] -> unit);
        g)
    in
    "<" ^ String.concat (List.map gids ~f:(sprintf "%04X")) ^ ">"
;;

(* Six capital letters derived from the glyph set, as PDF requires for a subset's name:
   the same document always gets the same tag. *)
let subset_tag gids =
  let digest =
    Md5.digest_string (String.concat ~sep:"," (List.map gids ~f:Int.to_string))
    |> Md5.to_binary
  in
  String.init 6 ~f:(fun i ->
    Char.of_int_exn (Char.to_int 'A' + (Char.to_int digest.[i] mod 26)))
;;

let utf16be u =
  let c = Uchar.to_scalar u in
  if c < 0x10000
  then sprintf "%04X" c
  else (
    let c = c - 0x10000 in
    sprintf "%04X%04X" (0xD800 lor (c lsr 10)) (0xDC00 lor (c land 0x3FF)))
;;

let to_unicode_cmap (pairs : (int * Uchar.t) list) =
  let chunks = List.chunks_of pairs ~length:100 in
  String.concat
    ~sep:"\n"
    ([ "/CIDInit /ProcSet findresource begin"
     ; "12 dict begin"
     ; "begincmap"
     ; "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def"
     ; "/CMapName /Adobe-Identity-UCS def"
     ; "/CMapType 2 def"
     ; "1 begincodespacerange"
     ; "<0000> <FFFF>"
     ; "endcodespacerange"
     ]
     @ List.concat_map chunks ~f:(fun chunk ->
       (sprintf "%d beginbfchar" (List.length chunk)
        :: List.map chunk ~f:(fun (g, u) -> sprintf "<%04X> <%s>" g (utf16be u)))
       @ [ "endbfchar" ])
     @ [ "endcmap"; "CMapName currentdict /CMap defineresource pop"; "end"; "end" ])
;;

let write t w =
  match t with
  | Standard ->
    let font base =
      Pdf_writer.add
        w
        (sprintf
           "<< /Type /Font /Subtype /Type1 /BaseFont /%s /Encoding /WinAnsiEncoding >>"
           base)
    in
    let regular = font "Helvetica" in
    let bold = font "Helvetica-Bold" in
    [ "/F1 " ^ Pdf_writer.ref_ regular; "/F2 " ^ Pdf_writer.ref_ bold ]
  | Embedded { font; used } ->
    let pairs =
      Hashtbl.to_alist used |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    in
    let gids = List.map pairs ~f:fst in
    let name = subset_tag gids ^ "+NotoSans-Regular" in
    let scale v = Float.of_int v *. 1000. /. Float.of_int (Truetype.units_per_em font) in
    let n v = Pdf_writer.number (Float.round_nearest (scale v)) in
    let program = Truetype.subset font gids in
    let file =
      Pdf_writer.add_stream
        w
        program
        ~dict:[ sprintf "/Length1 %d" (String.length program) ]
    in
    let x0, y0, x1, y1 = Truetype.bbox font in
    let descriptor =
      Pdf_writer.add
        w
        (sprintf
           "<< /Type /FontDescriptor /FontName /%s /Flags 32 /FontBBox [%s %s %s %s] \
            /ItalicAngle %s /Ascent %s /Descent %s /CapHeight %s /StemV 80 /FontFile2 %s \
            >>"
           name
           (n x0)
           (n y0)
           (n x1)
           (n y1)
           (Pdf_writer.number (Truetype.italic_angle font))
           (n (Truetype.ascender font))
           (n (Truetype.descender font))
           (n (Truetype.cap_height font))
           (Pdf_writer.ref_ file))
    in
    let widths =
      String.concat
        ~sep:" "
        (List.map gids ~f:(fun g -> sprintf "%d [%s]" g (n (Truetype.advance font g))))
    in
    let cid =
      Pdf_writer.add
        w
        (sprintf
           "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /%s /CIDSystemInfo << \
            /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor %s \
            /DW 1000 /W [%s] /CIDToGIDMap /Identity >>"
           name
           (Pdf_writer.ref_ descriptor)
           widths)
    in
    let to_unicode = Pdf_writer.add_stream w (to_unicode_cmap pairs) in
    let type0 =
      Pdf_writer.add
        w
        (sprintf
           "<< /Type /Font /Subtype /Type0 /BaseFont /%s /Encoding /Identity-H \
            /DescendantFonts [%s] /ToUnicode %s >>"
           name
           (Pdf_writer.ref_ cid)
           (Pdf_writer.ref_ to_unicode))
    in
    [ "/F1 " ^ Pdf_writer.ref_ type0 ]
;;
