open Core
open MParser

exception Syntax_error

let ( let* ) = bind

(* --- 2. Lexing Helpers --- *)

(* Consumes spaces, parses 'p', and then consumes trailing spaces *)
let lexeme p = p >>= fun x -> spaces >> return x

(* Parses a specific string and eats whitespace *)
let symbol str = lexeme (string str) >> return ()

(* Parses identifiers: starts with a letter, followed by alphanumerics *)
let ident case =
  let id = pipe2 case (many alphanum) (fun c cs -> String.of_char_list (c :: cs)) in
  lexeme id <?> "identifier"

let prod_comp s = (ident lowercase) s
let sum_const s = (ident uppercase) s

(* Handles parentheses: ( p ) *)
let parens p = between (symbol "(") (symbol ")") p
