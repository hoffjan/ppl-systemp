open MParser
open Utils

let address s =
  let parse =
    let* address = Utils.lexeme (many_chars_until any_char (char ' ')) in
    if String.length address = 0 then fail "address is empty" else return address
  in
  parse s

let trace s =
  let sample s =
    let parse =
      let* name = address in
      let* _ = symbol "=" in
      let* exp = Exp.parse in
      return (name, exp)
    in
    parse s
  in
  let parse =
    let* _ = symbol "{" in
    let* samples = sep_by sample (symbol ",") in
    let* _ = symbol "}" in
    return (Dynamics.Trace.of_list samples)
  in
  parse s

let parse s = (spaces >> trace) s
