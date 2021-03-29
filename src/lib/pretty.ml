open Syntax
open Util

(* pretty printer *)

let pp_name nm =
  let is_identifier =
    try
      match Lexing.from_string nm |> Lexer.token with
      | Parser.NAME nm' ->
          nm = nm' (* if the whole string is an identifier *)
      | _ -> false
    with _ -> false
  in
  if is_identifier then nm else "\"" ^ nm ^ "\""

let pp_tokens tks =
  let toks =
    List.map
      (function Token nm -> nm | _ -> failwith "violation can't be this")
      tks
  in
  if Util.is_empty toks then "" else "token " ^ String.concat " " toks ^ "."

let pp_places pcs =
  let pcs' =
    List.map
      (function
        | Place (nm, tks) -> (nm, tks)
        | _ -> failwith "violation can't be this" )
      pcs
  in
  let print_place (nm, tks) =
    pp_name nm ^ "[" ^ (String.concat " " @@ List.map pp_name tks) ^ "]"
  in
  if Util.is_empty pcs then ""
  else "place " ^ String.concat " " (List.map print_place pcs') ^ "."

let pp_transitions trs =
  let trs' =
    List.map
      (function
        | Transition (nm, vis) -> (nm, vis)
        | _ -> failwith "violation can't be this" )
      trs
  in
  let print_transition (nm, vis) =
    match vis with
    | Labelled -> pp_name nm
    | Silent -> "(" ^ pp_name nm ^ ")"
  in
  if Util.is_empty trs then ""
  else
    "transition "
    ^ (String.concat " " @@ List.map print_transition trs')
    ^ "."

let pp_arcs arcs =
  let arcs' =
    List.map
      (function
        | Arc (src, dst, tks) -> (src, dst, tks)
        | _ -> failwith "violation can't be this" )
      arcs
  in
  let rec pp arcs =
    let pp_tokens ts =
      "[" ^ (String.concat " " @@ List.map pp_name ts) ^ "]"
    in
    let rec continue_arc_from p arcs =
      match find_and_remove_fst (fun (a, _, _) -> a = p) arcs with
      | Some ((_, dst', tks'), arcs') ->
          let rest, more_arcs = continue_arc_from dst' arcs' in
          (">->" ^ pp_name dst' ^ pp_tokens tks' ^ rest, more_arcs)
      | None -> ("", arcs)
    in
    match arcs with
    | (s, d, ts) :: arcs' ->
        let this = pp_name s ^ ">->" ^ pp_name d ^ pp_tokens ts in
        let that, arcs'' = continue_arc_from d arcs' in
        this ^ that ^ ".\n" ^ pp arcs''
    | [] -> ""
  in
  pp arcs'

let pp_marking (nm, tag, pcs) =
  let print_tkns m = List.map pp_name m |> String.concat " " in
  let print_place (nm, m) = nm ^ "[" ^ print_tkns m ^ "]" in
  let str = List.map print_place pcs |> String.concat " " in
  match tag with
  | None -> "marking " ^ pp_name nm ^ str ^ "."
  | Some t -> "marking[" ^ t ^ "]" ^ " " ^ pp_name nm ^ " " ^ str ^ "."

let pp_markings markings =
  let markings' =
    List.map
      (function
        | Marking (nm, tag, mks) -> (nm, tag, mks)
        | _ -> failwith "violation can't be this" )
      markings
  in
  List.map pp_marking markings' |> String.concat "\n"

let pp_expr_list exprs =
  let tokens = List.filter (function Token _ -> true | _ -> false) exprs in
  let places = List.filter (function Place _ -> true | _ -> false) exprs in
  let transitions =
    List.filter (function Transition _ -> true | _ -> false) exprs
  in
  let arcs = List.filter (function Arc _ -> true | _ -> false) exprs in
  let markings =
    List.filter (function Marking _ -> true | _ -> false) exprs
  in
  String.concat "\n"
    [ pp_tokens tokens
    ; pp_places places
    ; pp_transitions transitions
    ; pp_arcs arcs
    ; pp_markings markings ]
