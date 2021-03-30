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


type fmt = Nuscr | Pedro | Dot | Sexp | Info

type cmd = Empty | Exit | Pwd | Load | Extra

let interact () =
  let _prots = ref ["default", Syntax.empty_net] in
  let rec loop () =
    let parse_cmd cmd =
      match String.split_on_char ' ' cmd with
      | [] -> Ok (Empty, [])
      | "bye"::_ -> Ok (Exit, [])
      | "pwd"::_ -> Ok (Pwd, [])
      | "load"::pars -> Ok (Load, pars)
      | _ -> Error ("Wrong command: " ^ cmd)
    in

    let do_cmd = function
      | Empty, _ -> Ok ("", false)
      | Exit, _ -> Ok ("Bye!", true)
      | Pwd, _ -> Ok (Sys.getcwd (), false)
      | Load, [] ->  Error "Load command with wrong number of parameters"
      | Load, pars ->
         let _fn = String.concat " " pars in
         Ok ("Not done.", false)
      | _ -> Error "Something is not implemented"
    in

    let print_res = function
      | Ok (str, _) -> print_endline str
      | Error msg -> print_endline @@ "Error: " ^ msg
    in
    let finished = function
        | Ok (_, res) -> res
        | _ -> false
    in

    print_string "> " ;
    let cmd_line = read_line () in
    let res = Result.bind
                (parse_cmd cmd_line)
              (fun cmd ->
                do_cmd cmd)
    in
    print_res res ;
    if finished res then () else loop ()
  in
  print_endline "Welcome to Pedro (Vote for Pedro!)" ;
  loop ()


let convert (fmt_in : fmt) (fmt_out : fmt) fn =
  let inp : (string * Syntax.net) list =
    match fmt_in with
    | Pedro -> (
        let exprs = parse fn (Stdlib.open_in fn) in
        match Syntax.validate_net exprs with
        | Ok net -> [(fn, net)]
        | Error err -> failwith err )
    | Nuscr -> (
      try
        let scr = Nuscrlib.Lib.parse fn (Stdlib.open_in fn) in
        let protocol_names = List.map fst @@ Nuscrlib.Lib.enumerate scr in
        let gtypes =
          let scr_to_net proto =
            match
              Nuscrlib.Lib.get_global_type scr ~protocol:proto
              |> Global.net_of_global_type
            with
            | Ok net -> net
            | Error err -> failwith err
          in
          List.map
            (fun proto ->
              (Global.N.ProtocolName.user proto, scr_to_net proto) )
            protocol_names
        in
        gtypes
      with Nuscrlib.Err.UserError ue ->
        Nuscrlib.Err.show_user_error ue |> failwith )
    | _ -> failwith "input format not supported"
  in
  let out (nm, net) =
    "**** " ^ nm ^ "****\n"
    ^
    match fmt_out with
    | Nuscr -> failwith "output format not supported"
    | Pedro -> net |> Syntax.expr_list_of_net |> Pretty.pp_expr_list
    | Dot -> Pn.generate_ppn net |> Pn.generate_dot
    | Sexp -> Syntax.sexp_of_net net |> Sexplib.Sexp.to_string
    | Info ->
        "----Information----\n" ^ "Enabled transitions: "
        ^ (Opsem.enabled_transitions net |> String.concat " ")
        ^ "\n" ^ "Enabled transitions (with silent): "
        ^ (Opsem.enabled_transitions_with_silent net |> String.concat " ")
  in
  let str : string = String.concat "\n" @@ List.map out inp in
  print_endline str
