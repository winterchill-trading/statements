open! Core

type t =
  { mutable next : int
  ; bodies : string Int.Table.t
  }

let create () = { next = 1; bodies = Int.Table.create () }

let reserve t =
  let n = t.next in
  t.next <- n + 1;
  n
;;

let define t n body = Hashtbl.set t.bodies ~key:n ~data:body

let add t body =
  let n = reserve t in
  define t n body;
  n
;;

let add_stream ?(dict = []) t data =
  add
    t
    (sprintf
       "<< /Length %d%s >>\nstream\n%s\nendstream"
       (String.length data)
       (String.concat (List.map dict ~f:(fun d -> " " ^ d)))
       data)
;;

let ref_ n = sprintf "%d 0 R" n

let to_string t ~root ?info () =
  let buf = Buffer.create 65536 in
  (* The second line's high bytes tell file tools the content is binary. *)
  Buffer.add_string buf "%PDF-1.4\n%\xE2\xE3\xCF\xD3\n";
  let offsets =
    List.init (t.next - 1) ~f:(fun i ->
      let n = i + 1 in
      match Hashtbl.find t.bodies n with
      | None -> raise_s [%message "PDF object reserved but never defined" (n : int)]
      | Some body ->
        let offset = Buffer.length buf in
        Buffer.add_string buf (sprintf "%d 0 obj\n%s\nendobj\n" n body);
        offset)
  in
  let xref = Buffer.length buf in
  Buffer.add_string buf (sprintf "xref\n0 %d\n" t.next);
  (* Each entry is exactly 20 bytes, ending in space + LF. *)
  Buffer.add_string buf "0000000000 65535 f \n";
  List.iter offsets ~f:(fun o -> Buffer.add_string buf (sprintf "%010d 00000 n \n" o));
  Buffer.add_string
    buf
    (sprintf
       "trailer\n<< /Size %d /Root %s%s >>\nstartxref\n%d\n%%%%EOF\n"
       t.next
       (ref_ root)
       (Option.value_map info ~default:"" ~f:(fun i -> " /Info " ^ ref_ i))
       xref);
  Buffer.contents buf
;;

let literal s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '(';
  String.iter s ~f:(fun c ->
    match c with
    | '\\' | '(' | ')' ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | ' ' .. '~' -> Buffer.add_char buf c
    | c -> Buffer.add_string buf (sprintf "\\%03o" (Char.to_int c)));
  Buffer.add_char buf ')';
  Buffer.contents buf
;;

let hex s = "<" ^ String.concat_map s ~f:(fun c -> sprintf "%02X" (Char.to_int c)) ^ ">"

let text_string s =
  if String.for_all s ~f:(fun c -> Char.( >= ) c ' ' && Char.( <= ) c '~')
  then literal s
  else (
    let utf16 =
      List.concat_map (Utf8.uchars s) ~f:(fun u ->
        let c = Uchar.to_scalar u in
        let unit v = [ Char.of_int_exn (v lsr 8); Char.of_int_exn (v land 0xFF) ] in
        if c < 0x10000
        then unit c
        else (
          let c = c - 0x10000 in
          unit (0xD800 lor (c lsr 10)) @ unit (0xDC00 lor (c land 0x3FF))))
    in
    hex ("\xFE\xFF" ^ String.of_list utf16))
;;

let number f =
  let s = sprintf "%.3f" f in
  let s = String.rstrip s ~drop:(Char.equal '0') in
  let s = String.chop_suffix_if_exists s ~suffix:"." in
  if String.equal s "-0" then "0" else s
;;
