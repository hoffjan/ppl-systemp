open Core

let parse_file file_name =
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  let () = Printf.printf "Parsing successful.\n" in
  exp

let filename_param = Command.Param.(anon ("filename" %: string))

let eval =
  Command.basic ~summary:"Evaluate a System P program"
    (Command.Param.map filename_param ~f:(fun filename () ->
         let exp = parse_file filename in
         let typ = Statics.type_exp exp in
         let () = Printf.printf "Type checking successful\n  Typ: %s\n" (Syntax.Typ.to_string typ) in
         let () = Printf.printf "Starting evaluation ... \n" in
         let value = Dynamics.Eval.eval exp in
         Dynamics.Pprint.print_value value))

let typecheck =
  Command.basic ~summary:"Evaluate a System P program"
    (Command.Param.map filename_param ~f:(fun filename () ->
         let exp = parse_file filename in
         let typ = Statics.type_exp exp in
         Printf.printf "Type checking successful\n  Typ: %s\n" (Syntax.Typ.to_string typ)))

let systemp = Command.group ~summary:("System P v" ^ Version.systemp) [ ("eval", eval); ("typecheck", typecheck) ]
