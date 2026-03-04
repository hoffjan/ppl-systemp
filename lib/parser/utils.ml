open Core
open MParser

let keywords =
  [
    "fn";
    "let";
    "rec";
    "in";
    "Cons";
    "Nil";
    "type";
    "case";
    "int";
    "float";
    "string";
    "list";
    "dist";
    "toString";
    "true";
    "false";
    "if";
    "then";
    "else";
    "sample";
    "at";
    "bern";
    "binom";
  ]

let ( let* ) = bind

let comment s =
  let parse =
    let* _ = string "(*" in
    skip_many_until any_char (string "*)")
  in
  parse s

let spaces_or_comment p = (skip_many (choice [ comment; skip space ])) p

(* Consumes spaces, parses 'p', and then consumes trailing spaces *)
let lexeme p = p >>= fun x -> spaces_or_comment >> return x

(* Parses a specific string and eats whitespace *)
let symbol str = lexeme (string str) >> return ()
let keyword str = attempt (string str >> not_followed_by alphanum "expecting keyword") >> spaces_or_comment >> return ()
let is_keyword str = List.mem keywords ~equal:String.( = ) str

(* Parses identifiers: starts with a letter, followed by alphanumerics *)
let ident case =
  let id = pipe2 case (many (choice [ alphanum; any_of "_" ])) (fun c cs -> String.of_char_list (c :: cs)) in
  let* str = look_ahead id in
  if is_keyword str then fail ("Expecting identifyer but found keyword: " ^ str) else lexeme id <?> "identifier"

(* Handles parentheses: ( p ) *)
let parens p = between (symbol "(") (symbol ")") p
let prod_comp s = (ident lowercase) s
let sum_const s = (ident uppercase) s
let typ_ident s = (ident lowercase) s
