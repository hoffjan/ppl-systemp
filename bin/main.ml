open Core

let () =
  let file_name = (Sys.get_argv ()).(1) in
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  let _ = Printf.printf "Expression:\n%s\n" (Syntax.Exp.to_string exp) in
  let typ = Statics.type_exp exp in
  Printf.printf "Type:\n%s\n" (Syntax.Typ.to_string typ)
