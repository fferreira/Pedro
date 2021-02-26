

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
  | e -> failwith @@ "Found a problem: " ^ Printexc.to_string e

let parse fname (ch : in_channel) =
  let lexbuf = set_filename fname (Lexing.from_channel ch) in
  parse_from_lexbuf lexbuf

let parse_string string = parse_from_lexbuf @@ Lexing.from_string string

let () = print_endline "//Vote for Pedro!" ;
         print_endline @@ "//Current working directory: " ^ Sys.getcwd ()  ;
         let fn =
           if Array.length Sys.argv = 2 then
             Sys.argv.(1)
           else
             "examples/proto.pdr"
         in
         print_endline @@ "//Reading: " ^ fn ;
         let exprs = parse fn (Stdlib.open_in fn) in
         match Syntax.validate_net exprs with
         | Monad.Yes net -> let s = Pn.generate_ppn net |> Pn.generate_dot in print_endline s
         | Monad.No err -> "//Alles kaput!: " ^ err |> print_endline
