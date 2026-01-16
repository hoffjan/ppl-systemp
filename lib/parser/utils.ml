open Core
open MParser

let keywords = [ "fn"; "let"; "rec"; "in"; "Cons"; "Nil"; "type"; "case"; "int"; "float"; "string"; "list" ]
let ( let* ) = bind

(* --- 2. Lexing Helpers --- *)

(* Consumes spaces, parses 'p', and then consumes trailing spaces *)
let lexeme p = p >>= fun x -> spaces >> return x

(* Parses a specific string and eats whitespace *)
let symbol str = lexeme (string str) >> return ()

(* Parses identifiers: starts with a letter, followed by alphanumerics *)
let ident case =
  let is_keyword str = List.mem keywords ~equal:String.( = ) str in
  let id = pipe2 case (many alphanum) (fun c cs -> String.of_char_list (c :: cs)) in
  let* str = look_ahead id in
  if is_keyword str then fail ("Expecting identifyer but found keyword: " ^ str) else lexeme id <?> "identifier"

(* Handles parentheses: ( p ) *)
let parens p = between (symbol "(") (symbol ")") p
let prod_comp s = (ident lowercase) s
let sum_const s = (ident uppercase) s
let typ_ident s = (ident lowercase) s
