open Core

let () =
  let file_name = (Sys.get_argv ()).(1) in
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  let () = Printf.printf "Parsing successful.\n" in
  let typ = Statics.type_exp exp in
  let () = Printf.printf "Type checking successful\n  Typ: %s\n" (Syntax.Typ.to_string typ) in
  let () = Printf.printf "Starting evaluation ... \n" in
  let value = Dynamics.Eval.eval exp in
  Dynamics.Pprint.print_value value
