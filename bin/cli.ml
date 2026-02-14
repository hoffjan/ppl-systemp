open Core

let parse_file file_name =
  let str = In_channel.read_all file_name in
  let exp = Parser.exp str in
  let () = Printf.printf "Parsing successful.\n" in
  exp

let filename_param = Command.Param.(anon ("filename" %: string))

let simulate =
  Command.basic ~summary:"Sample a trace from a System P program"
    (let%map_open.Command seed = flag "-seed" (optional int) ~doc:"INT random seed" and filename = filename_param in
     fun () ->
       let exp = parse_file filename in
       let typ = Statics.type_exp exp in
       Printf.printf "Type checking successful\n  Typ: %s\n" (Syntax.Typ.to_string typ);
       begin match seed with
       | Some s ->
           Printf.printf "Simulate with random seed %d ...\n" s;
           Dynamics.Stat.init s
       | None -> Printf.printf "Simulate with default random state ...\n"
       end;
       let res = Dynamics.Eval.simulate exp in
       Dynamics.Pprint.print_result ~weight:false res)

let assess =
  Command.basic ~summary:"Compute the weight of a complete trace of a System P program"
    (let%map_open.Command trace = flag "-trace" (required string) ~doc:"TRACE complete execution trace"
     and filename = filename_param in
     fun () ->
       let exp = parse_file filename in
       let typ = Statics.type_exp exp in
       let () = Printf.printf "Type checking successful\n  Typ: %s\n" (Syntax.Typ.to_string typ) in
       let exp_trace = Parser.trace trace in
       let trace = Dynamics.Trace.map exp_trace ~f:(fun exp -> Dynamics.Eval.eval exp) in
       let () = Printf.printf "Computing weight ...\n" in
       let res = Dynamics.Eval.assess trace exp in
       Dynamics.Pprint.print_result ~trace:false res)

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

let systemp =
  Command.group ~summary:("System P v" ^ Version.systemp)
    [ ("assess", assess); ("simulate", simulate); ("eval", eval); ("typecheck", typecheck) ]
