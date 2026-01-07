open Core
open MParser

exception Syntax_error

let ( let* ) = bind

(* --- 2. Lexing Helpers --- *)

(* Consumes spaces, parses 'p', and then consumes trailing spaces *)
let lexeme p = p >>= fun x -> spaces >> return x

(* Parses a specific string and eats whitespace *)
let symbol s = lexeme (string s) >> return ()

(* Parses identifiers: starts with a letter, followed by alphanumerics *)
let ident case : (string, unit) parser =
  let id = pipe2 case (many alphanum) (fun c cs -> String.of_char_list (c :: cs)) in
  lexeme id <?> "identifier"

let prod_comp = ident lowercase
let sum_const = ident uppercase

(* Handles parentheses: ( p ) *)
let parens p = between (symbol "(") (symbol ")") p
