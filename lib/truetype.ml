open! Core

type t =
  { data : string
  ; tables : (int * int) String.Map.t (** tag -> offset, length *)
  ; units_per_em : int
  ; num_glyphs : int
  ; long_loca : bool
  ; num_hmetrics : int
  ; ascender : int
  ; descender : int
  ; cap_height : int
  ; bbox : int * int * int * int
  ; italic_angle : float
  ; cmap : int Int.Table.t
  }

let u8 s o = Char.to_int s.[o]
let u16 s o = (u8 s o lsl 8) lor u8 s (o + 1)

let i16 s o =
  let v = u16 s o in
  if v >= 0x8000 then v - 0x10000 else v
;;

let u32 s o = (u16 s o lsl 16) lor u16 s (o + 2)

let i32 s o =
  let v = u32 s o in
  if v >= 0x8000_0000 then v - 0x1_0000_0000 else v
;;

let table t tag = Map.find_exn t.tables tag
let units_per_em t = t.units_per_em
let num_glyphs t = t.num_glyphs
let ascender t = t.ascender
let descender t = t.descender
let cap_height t = t.cap_height
let bbox t = t.bbox
let italic_angle t = t.italic_angle
let glyph t u = Hashtbl.find t.cmap (Uchar.to_scalar u)

let advance t g =
  let off, _ = table t "hmtx" in
  u16 t.data (off + (4 * Int.min g (t.num_hmetrics - 1)))
;;

(* Format 4: segments of 16-bit code points. *)
let read_format4 s o cmap =
  let seg = u16 s (o + 6) / 2 in
  let ends = o + 14 in
  let starts = ends + (2 * seg) + 2 in
  let deltas = starts + (2 * seg) in
  let ranges = deltas + (2 * seg) in
  for i = 0 to seg - 1 do
    let first = u16 s (starts + (2 * i))
    and last = u16 s (ends + (2 * i))
    and delta = u16 s (deltas + (2 * i))
    and range = u16 s (ranges + (2 * i)) in
    if first <> 0xFFFF
    then
      for c = first to last do
        let g =
          if range = 0
          then (c + delta) land 0xFFFF
          else (
            let g = u16 s (ranges + (2 * i) + range + (2 * (c - first))) in
            if g = 0 then 0 else (g + delta) land 0xFFFF)
        in
        if g <> 0 then Hashtbl.set cmap ~key:c ~data:g
      done
  done
;;

(* Format 12: groups of 32-bit code points. *)
let read_format12 s o cmap =
  let groups = u32 s (o + 12) in
  for i = 0 to groups - 1 do
    let g = o + 16 + (12 * i) in
    let first = u32 s g
    and last = u32 s (g + 4)
    and glyph = u32 s (g + 8) in
    for c = first to last do
      Hashtbl.set cmap ~key:c ~data:(glyph + c - first)
    done
  done
;;

let parse data =
  Or_error.try_with_join (fun () ->
    let open Or_error.Let_syntax in
    let%bind () =
      if String.length data >= 12 && u32 data 0 = 0x0001_0000
      then Ok ()
      else Or_error.error_string "not a TrueType font (sfnt version 1.0)"
    in
    let n = u16 data 4 in
    let tables =
      List.init n ~f:(fun i ->
        let e = 12 + (16 * i) in
        String.sub data ~pos:e ~len:4, (u32 data (e + 8), u32 data (e + 12)))
      |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
    in
    let%bind () =
      List.filter [ "head"; "hhea"; "hmtx"; "maxp"; "loca"; "glyf" ] ~f:(fun tag ->
        not (Map.mem tables tag))
      |> function
      | [] -> Ok ()
      | missing -> Or_error.error_s [%message "missing tables" (missing : string list)]
    in
    let off tag = fst (Map.find_exn tables tag) in
    let head = off "head"
    and hhea = off "hhea" in
    let cmap = Int.Table.create () in
    let c = Option.value_map (Map.find tables "cmap") ~default:0 ~f:fst in
    let subtables =
      if c = 0
      then []
      else
        List.init
          (u16 data (c + 2))
          ~f:(fun i ->
            let e = c + 4 + (8 * i) in
            u16 data e, u16 data (e + 2), c + u32 data (e + 4))
    in
    let find p e = List.find subtables ~f:(fun (p', e', _) -> p = p' && e = e') in
    let with_format f =
      List.find_map ~f:(fun st -> Option.find st ~f:(fun (_, _, o) -> u16 data o = f))
    in
    let%bind () =
      match
        with_format 12 [ find 3 10; find 0 4 ], with_format 4 [ find 3 1; find 0 3 ]
      with
      | Some (_, _, o), _ -> Ok (read_format12 data o cmap)
      | None, Some (_, _, o) -> Ok (read_format4 data o cmap)
      | None, None when c = 0 -> Ok ()
      | None, None -> Or_error.error_string "no Unicode cmap of format 4 or 12"
    in
    let ascender = i16 data (hhea + 4) in
    let cap_height =
      match Map.find tables "OS/2" with
      | Some (o, len) when u16 data o >= 2 && len >= 90 -> i16 data (o + 88)
      | _ -> ascender
    in
    let italic_angle =
      match Map.find tables "post" with
      | Some (o, _) -> Float.of_int (i32 data (o + 4)) /. 65536.
      | None -> 0.
    in
    Ok
      { data
      ; tables
      ; units_per_em = u16 data (head + 18)
      ; num_glyphs = u16 data (off "maxp" + 4)
      ; long_loca = i16 data (head + 50) = 1
      ; num_hmetrics = u16 data (hhea + 34)
      ; ascender
      ; descender = i16 data (hhea + 6)
      ; cap_height
      ; bbox =
          ( i16 data (head + 36)
          , i16 data (head + 38)
          , i16 data (head + 40)
          , i16 data (head + 42) )
      ; italic_angle
      ; cmap
      })
;;

(* Byte range of a glyph's outline inside the font. *)
let glyph_range t g =
  let loca, _ = table t "loca" in
  let at i =
    if t.long_loca then u32 t.data (loca + (4 * i)) else 2 * u16 t.data (loca + (2 * i))
  in
  let glyf, _ = table t "glyf" in
  glyf + at g, at (g + 1) - at g
;;

(* Glyphs a composite glyph is built from. *)
let components t g =
  let off, len = glyph_range t g in
  if len = 0 || i16 t.data off >= 0
  then []
  else (
    let rec go o acc =
      let flags = u16 t.data o in
      let glyph = u16 t.data (o + 2) in
      let args = if flags land 0x0001 <> 0 then 4 else 2 in
      let scale =
        if flags land 0x0008 <> 0
        then 2
        else if flags land 0x0040 <> 0
        then 4
        else if flags land 0x0080 <> 0
        then 8
        else 0
      in
      let acc = glyph :: acc in
      if flags land 0x0020 <> 0 then go (o + 4 + args + scale) acc else acc
    in
    go (off + 10) [])
;;

let non_empty_glyphs t =
  List.filter (List.init t.num_glyphs ~f:Fn.id) ~f:(fun g -> snd (glyph_range t g) > 0)
;;

let checksum s =
  let padded = s ^ String.make ((4 - (String.length s mod 4)) mod 4) '\000' in
  let sum = ref 0 in
  for i = 0 to (String.length padded / 4) - 1 do
    sum := (!sum + u32 padded (4 * i)) land 0xFFFF_FFFF
  done;
  !sum
;;

let be16 v = String.init 2 ~f:(fun i -> Char.of_int_exn ((v lsr (8 * (1 - i))) land 0xFF))
let be32 v = String.init 4 ~f:(fun i -> Char.of_int_exn ((v lsr (8 * (3 - i))) land 0xFF))

let subset t glyphs =
  (* Close over composite components. *)
  let keep = Int.Hash_set.create () in
  let rec add g =
    if g < t.num_glyphs && not (Hash_set.mem keep g)
    then (
      Hash_set.add keep g;
      List.iter (components t g) ~f:add)
  in
  List.iter (0 :: glyphs) ~f:add;
  let glyf = Buffer.create 65536 in
  let loca = Buffer.create ((t.num_glyphs + 1) * 4) in
  for g = 0 to t.num_glyphs - 1 do
    Buffer.add_string loca (be32 (Buffer.length glyf));
    if Hash_set.mem keep g
    then (
      let off, len = glyph_range t g in
      Buffer.add_string glyf (String.sub t.data ~pos:off ~len);
      (* Glyphs start on four-byte boundaries. *)
      Buffer.add_string glyf (String.make ((4 - (len mod 4)) mod 4) '\000'))
  done;
  Buffer.add_string loca (be32 (Buffer.length glyf));
  let copy tag =
    Option.map (Map.find t.tables tag) ~f:(fun (o, len) -> String.sub t.data ~pos:o ~len)
  in
  let head =
    let h = Bytes.of_string (Option.value_exn (copy "head")) in
    (* checkSumAdjustment is filled in below; the new loca is in the long format. *)
    Bytes.From_string.blit ~src:(be32 0) ~src_pos:0 ~dst:h ~dst_pos:8 ~len:4;
    Bytes.From_string.blit ~src:(be16 1) ~src_pos:0 ~dst:h ~dst_pos:50 ~len:2;
    Bytes.to_string h
  in
  let tables =
    [ "cvt ", copy "cvt "
    ; "fpgm", copy "fpgm"
    ; "glyf", Some (Buffer.contents glyf)
    ; "head", Some head
    ; "hhea", copy "hhea"
    ; "hmtx", copy "hmtx"
    ; "loca", Some (Buffer.contents loca)
    ; "maxp", copy "maxp"
    ; "prep", copy "prep"
    ]
    |> List.filter_map ~f:(fun (tag, data) -> Option.map data ~f:(fun d -> tag, d))
  in
  let n = List.length tables in
  let rec log2 x = if x <= 1 then 0 else 1 + log2 (x / 2) in
  let entry_selector = log2 n in
  let search_range = 16 * (1 lsl entry_selector) in
  let header =
    be32 0x0001_0000
    ^ be16 n
    ^ be16 search_range
    ^ be16 entry_selector
    ^ be16 ((16 * n) - search_range)
  in
  let pad s = s ^ String.make ((4 - (String.length s mod 4)) mod 4) '\000' in
  let _, entries, bodies =
    List.fold
      tables
      ~init:(12 + (16 * n), [], [])
      ~f:(fun (offset, entries, bodies) (tag, data) ->
        let entry =
          tag ^ be32 (checksum data) ^ be32 offset ^ be32 (String.length data)
        in
        let body = pad data in
        offset + String.length body, entry :: entries, body :: bodies)
  in
  let font =
    header ^ String.concat (List.rev entries) ^ String.concat (List.rev bodies)
  in
  (* The head table's checkSumAdjustment makes the whole file sum to 0xB1B0AFBA. *)
  let adjustment = (0xB1B0_AFBA - checksum font) land 0xFFFF_FFFF in
  let head_offset =
    let rec find i = function
      | (tag, _) :: _ when String.equal tag "head" -> i
      | _ :: rest -> find (i + 1) rest
      | [] -> assert false
    in
    let index = find 0 tables in
    u32 font (12 + (16 * index) + 8)
  in
  let b = Bytes.of_string font in
  Bytes.From_string.blit
    ~src:(be32 adjustment)
    ~src_pos:0
    ~dst:b
    ~dst_pos:(head_offset + 8)
    ~len:4;
  Bytes.to_string b
;;

let bundled_font = lazy (parse Noto_sans_data.data |> ok_exn)
let bundled () = force bundled_font
