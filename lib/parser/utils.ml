open Core
open MParser

exception Syntax_error

let ( let* ) = bind

(* --- 2. Lexing Helpers --- *)

(* Consumes spaces, parses 'p', and then consumes trailing spaces *)
let lexeme p = p >>= fun x -> spaces >> return x

(* Parses a specific string and eats whitespace *)
let symbol s = lexeme (string s)

(* Parses identifiers: starts with a letter, followed by alphanumerics *)
let ident : (string, unit) parser =
  let id = pipe2 letter (many alphanum) (fun c cs -> String.of_char_list (c :: cs)) in
  lexeme id <?> "identifier"

(* Handles parentheses: ( p ) *)
let parens p = between (symbol "(") (symbol ")") p
