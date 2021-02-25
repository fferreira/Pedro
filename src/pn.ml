open Graph

type name = string

type token = Token of name * int (* a token has a name and a multiplicity *)

type node = Place of name * token list
          | Transition of name

(* if the label is to a transition it is the tokens it needs to
   trigger or if it is from a transition it is the tokens it provides
   *)
type label = token list

module Node =
  struct
    type t = node
  end

module Label =
  struct
    type t = label

    let default = []

    (* perhaps we want a better comparison
       (labels are name multisets)
     *)
    let compare l1 l2 = if List.for_all2 (=) l1 l2 then 0 else 1
  end

module PersistentPN = Persistent.Digraph.AbstractLabeled(Node)(Label)

module PN = Imperative.Digraph.AbstractLabeled(Node)(Label)


type mark = token list
module PNMark = Map.Make(Int) (* relates the node mark (which is an integer) with the mark *)

let _ = Parsing.peek_val


let show_position pos =
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_source_loc (startp, endp) : string =
  Printf.sprintf "%s to %s in: %s" (show_position startp) (show_position endp)
    startp.Lexing.pos_fname

let set_filename (fname : string) (lexbuf : Lexing.lexbuf) =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname= fname} ;
  lexbuf

let parse_from_lexbuf lexbuf =
  try Parser.petri_net Lexer.token lexbuf with
  | Lexer.LexError msg -> failwith msg
  | Parser.Error ->
      let err_interval =
        (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
      in
      failwith @@ "Parse error: " ^ show_source_loc err_interval
  | e -> failwith @@ "Found a problem: " ^ Printexc.to_string_default e

let parse fname (ch : in_channel) =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  parse_from_lexbuf lexbuf

let parse_string string = parse_from_lexbuf @@ Lexing.from_string string



let () = print_endline "Vote for Pedro!" ;
         print_endline @@ "Current working directory: " ^ Sys.getcwd ()  ;
         let fn = "examples/read-short.pdr" in
         let _ = parse fn (Stdlib.open_in fn) in
         ()
