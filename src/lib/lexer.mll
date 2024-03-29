{
  open Lexing
  open Parser

  exception LexError of string
}

(* some definitions of character classes *)

let underscore = '_' (* perhaps we could remove this *)
let prime = '\''
let letter = ['a'-'z' 'A'-'Z']
let bang = '!'
let qmark = '?'
let colon = ':'

let digit = ['0'-'9']

let identifier = (letter|digit|underscore|prime|bang|qmark|colon)+

let int_literal = digit+

let string_literal = "\""[^'\"']*"\""

rule line_comment = parse
| '\n' {  new_line lexbuf ; token lexbuf }
| _ { line_comment lexbuf }

and block_comment n = parse
| "(*)" { block_comment n lexbuf }
| "*)" { if (n-1) = 0 then token lexbuf else block_comment (n-1) lexbuf }
| "(*" { block_comment (n+1) lexbuf }
| '\n' { new_line lexbuf ; block_comment n lexbuf }
| _ { block_comment n lexbuf }

and token = parse
(* whitespace *)
| ('\t'|' ')+ { token lexbuf}
| ('\n'|'\r') { new_line lexbuf ; token lexbuf}

(* comments and pragmas *)
| "(*)" { line_comment lexbuf }  (* nuScr extension: ml-style line comments *)
| "(*" { block_comment 1 lexbuf }

(* symbolic literals *)
| '[' { SQLEFT }
| ']' { SQRIGHT }
| '(' { LPARENS }
| ')' { RPARENS }
| '.' { PERIOD }

(* keywords *)

| "token" { TOKEN }
| "place" { PLACE }
| "transition" { TRANSITION }
| "marking" { MARKING }
| ">->" { ARROW }


(* other *)
| eof
    { EOI }

| identifier as str { NAME str }

| string_literal as str { QNAME (String.sub str 1 (String.length str - 2)) }

| _ as unrecog {
  let offset = Lexing.lexeme_start lexbuf in
  let str = Printf.sprintf "At offset %d: unexpected character('%c').\n" offset unrecog in
  LexError str |> raise }
