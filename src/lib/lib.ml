let show_position pos =
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_source_loc (startp, endp) : string =
  Printf.sprintf "%s to %s in: %s" (show_position startp)
    (show_position endp) startp.Lexing.pos_fname

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

let validate_exprs_to_net_to_exprs_to_net exprs =
  match Syntax.validate_net exprs with
  | Ok net -> (
    match Syntax.expr_list_of_net net |> Syntax.validate_net with
    | Ok net -> net
    | _ -> failwith "Violation: this cannot happen" )
  | _ -> failwith "Violation: this cannot happen"

let main_scr fn =
  try
    let scr = Nuscrlib.Lib.parse fn (Stdlib.open_in fn) in
    let proto = Nuscrlib.Names.ProtocolName.of_string "TwoBuyer" in
    let gtype = Nuscrlib.Lib.get_global_type scr ~protocol:proto in
    let net = Global.net_of_global_type gtype |> Result.value ~default: Syntax.empty_net in
    Syntax.expr_list_of_net net |> Pretty.pp_expr_list |> print_endline
  with
  | Nuscrlib.Err.UserError ue -> Nuscrlib.Err.show_user_error ue |> print_endline


let main_pdr fn =
  print_endline @@ "//Reading: " ^ fn ;
  let exprs = parse fn (Stdlib.open_in fn) in
  match Syntax.validate_net exprs with
  | Ok net ->
      print_endline "----Graphviz----" ;
      Pn.generate_ppn net |> Pn.generate_dot |> print_endline ;
      print_endline "----SExp----" ;
      Syntax.sexp_of_net net |> Sexplib.Sexp.to_string_hum |> print_endline ;
      print_endline "----Pedro----" ;
      Pretty.pp_expr_list exprs |> print_endline ;
      print_endline "----Pedro from net----" ;
      let net' = net |> Syntax.expr_list_of_net |> Pretty.pp_expr_list in
      print_endline net' ;
      print_endline "----Information----" ;
      "Enabled transitions: "
      ^ (Opsem.enabled_transitions net |> String.concat " ")
      |> print_endline ;
      "Enabled transitions (with silent): "
      ^ (Opsem.enabled_transitions_with_silent net |> String.concat " ")
      |> print_endline ;
      "Enabled transitions (with silent, and pretty print back): "
      ^ ( Opsem.enabled_transitions_with_silent
          @@ validate_exprs_to_net_to_exprs_to_net exprs
        |> String.concat " " )
      |> print_endline ;
      print_endline "----After first transition----" ;
      if List.length (Opsem.enabled_transitions net) > 0 then (
        let net =
          Opsem.enabled_transitions net
          |> List.hd |> Opsem.do_transition net
          |> Option.value ~default:Syntax.empty_net
        in
        net |> Pn.generate_ppn |> Pn.generate_dot |> print_endline ;
        print_endline "----Information after the first transition ----" ;
        "Enabled transitions: "
        ^ (net |> Opsem.enabled_transitions |> String.concat " ")
        |> print_endline ;
        "Enabled transitions: "
        ^ (net |> Opsem.enabled_transitions_with_silent |> String.concat " ")
        |> print_endline )
      else "No first transition." |> print_endline
  | Error err -> "//Alles kaputt!: " ^ err |> print_endline

let main fn =
  if Filename.check_suffix fn ".pdr" then
    main_pdr fn
  else
    main_scr fn
