open Syntax

let rec remove_one x = function
| [] -> []
| y::ys when x = y -> ys
| y::ys -> y::remove_one x ys

(* consumes mreq resources from mavail and returns the new mavail *)
let rec consume_from_marking (mavail : entity_marking) (mreq: entity_marking) : entity_marking option =
  match mreq with
    [] -> Some mavail
  | (_, [])::mreq' -> consume_from_marking mavail mreq'
  | (s, tkn::tkns)::mreq' ->
     begin match List.assoc_opt s mavail with
     | None
     | Some [] -> None

     | Some tkns' when List.mem tkn tkns' ->
        let mavail' = List.remove_assoc s mavail in
        let tkns'' = remove_one tkn tkns' in
        consume_from_marking ((s, tkns'')::mavail') ((s, tkns)::mreq')

     | Some (_::_) -> None
     end

open List

let rec add_to_marking (mavail : entity_marking) (mprov : entity_marking) : entity_marking =
  match mprov with
| [] -> mavail
| (_, [])::mprov' -> add_to_marking mavail mprov'
| (s, tkns)::mprov' ->
   let av_tkns = List.assoc_opt s mavail |> Option.value ~default: [] in
   add_to_marking ((s, av_tkns @ tkns) :: (List.remove_assoc s mavail)) mprov'


(* checks if a transition is enabled *)
let is_transition_enabled (n : net) (nm : name) : bool =
  let collect_arcs = List.filter (fun (_, dst, _, _) -> nm = dst) n.arcs in
  if List.length collect_arcs = 0 then false else
    let has_enough pl mreq =
      let mavail = List.assoc pl n.places in
      match consume_from_marking mavail mreq with | Some _ -> true | None -> false
    in
    List.for_all (fun (src,_,_,m) -> has_enough src m) collect_arcs

(* lists enabled transitions *)
let enabled_transitions n =
  List.map fst n.transitions |> List.filter (is_transition_enabled n)




let do_transition (n : net) (t : name) =
  let consume n : net option =
    let collect_arcs = List.filter (fun (_, dst, _, _) -> t = dst) n.arcs in
    if List.length collect_arcs = 0 then None else
      let update_for_arc (src, _dst, _dir, mreq) n =
        let mavail = List.assoc src n.places in
        Option.bind (consume_from_marking mavail mreq)
          (fun mavail ->
            Some {n with places = (src, mavail)::(List.remove_assoc src n.places)})
      in
      let rec update_all arcs n =
        match arcs with
        | [] -> Some n
        | arc'::arcs' ->
           Option.bind (update_for_arc arc' n) (fun n' -> update_all arcs' n')
      in
      update_all collect_arcs n
  in
  let provide n : net option =
    let collect_arcs = List.filter (fun (src, _, _, _) -> t = src) n.arcs in
    if List.length collect_arcs = 0 then None else
      let add_arc  (_src, dst, _dir, mprov) n =
        let mdest = List.assoc dst n.places in
        let places = (dst, add_to_marking mdest mprov)::List.remove_assoc dst n.places in
        { n with places }
      in
      let rec add_all arcs n =
        match arcs with
        | [] -> n
        | arc'::arcs' ->
           let n' = add_arc arc' n in
           add_all arcs' n'
      in
      Some (add_all collect_arcs n)
  in
  Option.bind (consume n) (fun n' -> provide n')
