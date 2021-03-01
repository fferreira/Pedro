open Syntax

(* some utility function *)

let rec uniq = function
  | x::xs -> if List.mem x xs then uniq xs else x::uniq xs
  | [] -> []

let rec find_and_remove_fst (f : 'a -> bool) : 'a list -> ('a * 'a list) option =
  function
  | x :: xs when f x -> Some (x, xs)
  | x :: xs ->
     begin match find_and_remove_fst f xs with
     | Some (y, ys) -> Some (y, x::ys)
     | None -> None
     end
  | [] -> None

(* pretty printer *)

let pp_name nm =
  let is_identifier =
    match Lexing.from_string nm |> Lexer.token with
    | Parser.NAME nm' -> nm = nm' (* if the whole string is an identifier *)
    | _ -> false
  in
  if is_identifier then nm  else "\"" ^ nm ^ "\""

let pp_name_with_multiplicity (nm, n) =
  if n = 1 then pp_name nm
  else pp_name nm ^ "^" ^ string_of_int n

let pp_tokens tks =
  let toks = List.map (function |Token (nm, s) -> (nm, s) |_ -> failwith "violation can't be this") tks in
  let sorts = List.map snd toks |> uniq in
  let print_per_sort s =
    let toks = List.filter (fun (_, s') -> s' = s) toks |> List.map fst in
    if List.length toks = 0 then failwith "this cannot happen" ;
    if List.length toks = 1 then
      let tok = List.hd toks in
      if tok = s then
        "token " ^ pp_name tok ^ "."
      else
        "token " ^ pp_name tok ^ ":" ^ pp_name s ^ "."
    else
      "token:" ^ pp_name s ^ " " ^ String.concat " " toks ^ "."
  in
  List.map print_per_sort sorts |> String.concat "\n"

let pp_places pcs =
  let pcs' = List.map (function |Place (nm, tks) -> (nm, tks) |_ -> failwith "violation can't be this") pcs in
  let print_place (nm, tks) =
    pp_name nm ^ "[" ^ (String.concat " " @@ List.map pp_name_with_multiplicity tks) ^ "]"
  in
  "place " ^ String.concat " " (List.map print_place pcs') ^ "."

let pp_transitions trs =
  let trs' = List.map (function |Transition (nm, vis) -> (nm, vis) |_ -> failwith "violation can't be this") trs in
  let print_transition (nm, vis) =
    match vis with
    | Labelled ->  pp_name nm
    | Silent -> "(" ^ pp_name nm ^ ")"
  in
  "transition " ^ (String.concat " " @@ List.map print_transition trs') ^ "."

let pp_arcs arcs =
  let arcs' = List.map (function |Arc (src, dst, tks) -> (src, dst, tks) |_ -> failwith "violation can't be this") arcs in
  let rec pp arcs =
    let pp_tokens ts = "[" ^ (String.concat " " @@ List.map pp_name_with_multiplicity ts) ^ "]" in
    let rec continue_arc_from p arcs =
      match find_and_remove_fst (fun (a, _, _)-> a = p) arcs with
      | Some ((_, dst', tks'), arcs')->
         let rest, more_arcs = continue_arc_from dst' arcs' in
         ">->" ^ pp_name dst' ^ pp_tokens tks' ^ rest, more_arcs
      | None ->  "", arcs
    in
    match arcs with
    | (s, d, ts)::arcs' ->
       let this = pp_name s ^ ">->" ^ pp_name d ^ pp_tokens ts in
       let that, arcs'' = continue_arc_from d arcs' in
       this ^ that ^ ".\n" ^ pp arcs''
    | [] -> ""
  in
  pp arcs'

let pp_expr_list exprs =
  let tokens = List.filter (function Token _ -> true |_ -> false) exprs in
  let places = List.filter (function Place _ -> true |_ -> false) exprs in
  let transitions = List.filter (function Transition _ -> true |_ -> false) exprs in
  let arcs = List.filter (function Arc _ -> true |_ -> false) exprs in
  String.concat "\n" [pp_tokens tokens ; pp_places places ; pp_transitions transitions ; pp_arcs arcs]