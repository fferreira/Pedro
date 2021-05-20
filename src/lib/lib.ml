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

type cmd =
  | Empty
  | Exit
  | Pwd
  | Load
  | List
  | Set
  | Trx
  | Print
  | Done
  | Proj
  | Help

let interact () =
  let prots = ref [("default", Syntax.empty_net)] in
  let current : string ref = ref "default" in
  let rec loop () =
    let parse_cmd cmd =
      let cmd' = String.trim cmd in
      let split =
        String.index_opt cmd' ' '
        |> Option.value ~default:(String.length cmd')
      in
      let verb, pars =
        ( String.sub cmd' 0 split |> String.trim
        , String.sub cmd' split (String.length cmd' - split) |> String.trim
        )
      in
      match verb with
      | "" -> Ok (Empty, "")
      | "bye" -> Ok (Exit, "")
      | "pwd" -> Ok (Pwd, "")
      | "load" -> Ok (Load, pars)
      | "list" -> Ok (List, pars)
      | "set" -> Ok (Set, pars)
      | "trx" -> Ok (Trx, pars)
      | "print" -> Ok (Print, pars)
      | "done" -> Ok (Done, pars)
      | "project" -> Ok (Proj, pars)
      | "help" -> Ok (Help, pars)
      | _ -> Error ("Wrong command: " ^ cmd)
    in
    let do_cmd = function
      | Empty, _ -> Ok ("", false)
      | Exit, _ -> Ok ("Bye!", true)
      | Pwd, _ -> Ok (Sys.getcwd (), false)
      | Load, "" -> Error "Load command with wrong number of parameters"
      | Load, fn ->
          if Filename.extension fn = ".pdr" then
            let exprs = parse fn (Stdlib.open_in fn) in
            match Syntax.validate_net exprs with
            | Ok net ->
                prots := [("default", net)] ;
                current := "default" ;
                Ok (fn ^ " loaded.", false)
            | Error err -> Error err
          else if
            Filename.extension fn = ".nuscr"
            || Filename.extension fn = ".scr"
          then
            try
              let scr = Nuscrlib.Lib.parse fn (Stdlib.open_in fn) in
              let protocol_names =
                List.map fst @@ Nuscrlib.Lib.enumerate scr |> Util.uniq
              in
              let gtypes =
                let scr_to_net proto =
                  match
                    Result.bind
                      (Nuscrlib.Lib.get_global_type scr ~protocol:proto |> Wf.wf)
                      Global.net_of_global_type
                  with
                  | Ok net -> net
                  | Error err -> failwith err
                in
                List.map
                  (fun proto ->
                    (Global.N.ProtocolName.user proto, scr_to_net proto) )
                  protocol_names
              in
              prots := gtypes ;
              current := List.hd gtypes |> fst ;
              Ok (fn ^ " loaded.", false)
            with Nuscrlib.Err.UserError ue ->
              Error (Nuscrlib.Err.show_user_error ue)
          else Error ("Unknown file extension: " ^ Filename.extension fn)
      | List, _ ->
          let pns = String.concat " " @@ List.map fst !prots in
          Ok (pns, false)
      | Set, prot when String.length prot = 0 -> Ok (!current, false)
      | Set, prot ->
          if List.mem prot (List.map fst !prots) then (
            current := prot ;
            Ok ("Ok.", false) )
          else Error (prot ^ " not found.")
      | Trx, pars when String.length pars = 0 ->
          let net = List.assoc !current !prots in
          Ok
            ( Opsem.enabled_transitions_with_silent net |> String.concat " "
            , false )
      | Trx, tn -> (
          let net = List.assoc !current !prots in
          match Opsem.do_transition_with_silent net tn with
          | None -> Error (tn ^ " can't be taken.")
          | Some net ->
              prots := (!current, net) :: List.remove_assoc !current !prots ;
              Ok ("Ok.", false) )
      | Print, fmt ->
          let net = List.assoc !current !prots in
          let str =
            match fmt with
            | "sexp" -> Syntax.sexp_of_net net |> Sexplib.Sexp.to_string
            | "dot" -> Pn.generate_ppn net |> Pn.generate_dot
            | "pedro" | _ ->
                net |> Syntax.expr_list_of_net |> Pretty.pp_expr_list
          in
          Ok (str, false)
      | Done, _ ->
          let net = List.assoc !current !prots in
          let final_set = Opsem.get_markings_by_tag "final" net in
          if List.exists (Opsem.net_matches_marking net) final_set then
            Ok ("Ok.", false)
          else Error "Net is not done"
      | Proj, tok ->
          let net = Proj.project (List.assoc !current !prots) tok in
          prots := (!current, net) :: List.remove_assoc !current !prots ;
          Ok ("Ok.", false)
      | Help, _ ->
          let msg =
            "Enter: cmd pars\n"
            ^ "load <name>.nuscr : load a scribble file.\n"
            ^ "load <name>.scr : load a scribble file.\n"
            ^ "load <name>.pdr : load a pedro file.\n"
            ^ "list : list the currently loaded protocos.\n"
            ^ "set : with no parameters prints the current protocol.\n"
            ^ "set <name> : sets <name> as the current protocol.\n"
            ^ "trx : with no parameters prints the enabled transitions.\n"
            ^ "trx <name> : tries to execute <name> in the current protocl.\n"
            ^ "print : prints the pedro syntax for the current state of the \
               protocol.\n"
            ^ "print pedro : prints the pedro syntax for the current state \
               of the protocol.\n"
            ^ "print sexp : prints the s-expression for the current state \
               of the protocol.\n"
            ^ "print dot : prints the graphviz representation for the \
               current state of the protocol.\n"
            ^ "done : prints Ok when the protocol has finished.\n"
            ^ "project <role> : projects the net to the information that \
               role sees.\n" ^ "help : prints this message.\n"
            ^ "pwd : prints working directory.\n" ^ "bye : quits.\n"
          in
          Ok (msg, false)
    in
    let print_res = function
      | Ok (str, _) -> print_endline str
      | Error msg -> print_endline @@ "Error: " ^ msg
    in
    let finished = function Ok (_, res) -> res | _ -> false in
    print_string "> " ;
    let cmd_line = try read_line () with _ -> "bye" in
    let res = Result.bind (parse_cmd cmd_line) (fun cmd -> do_cmd cmd) in
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
        let protocol_names =
          List.map fst @@ Nuscrlib.Lib.enumerate scr |> Util.uniq
        in
        let gtypes =
          let scr_to_net proto =
            match
              Result.bind
                (Nuscrlib.Lib.get_global_type scr ~protocol:proto |> Wf.wf)
                Global.net_of_global_type
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
