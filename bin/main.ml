open Core

let () =
  let file_name = (Sys.get_argv ()).(1) in
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  Printf.printf "%s\n" (Syntax.Exp.to_string exp)
