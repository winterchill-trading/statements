(* Prints [let data = "..."] with the bytes of the file named on the command line. *)

let () =
  let path = Sys.argv.(1) in
  let ic = open_in_bin path in
  let data = really_input_string ic (in_channel_length ic) in
  close_in ic;
  print_string "(* Generated from ";
  print_string (Filename.basename path);
  print_string " by gen/embed.exe. *)\n\nlet data = \"";
  String.iter
    (fun c ->
      match c with
      | ' ' .. '~' when c <> '"' && c <> '\\' -> print_char c
      | c -> Printf.printf "\\%03d" (Char.code c))
    data;
  print_string "\"\n"
;;
