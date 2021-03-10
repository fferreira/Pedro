open Syntax

(* consumes mreq resources from mavail and returns the new mavail *)
let rec consume (mavail : entity_marking) (mreq: entity_marking) : entity_marking option =
  match mreq with
    [] -> Some mavail
  | (_, [])::mreq' -> consume mavail mreq'
  | (s, tkn::tkns)::mreq' ->
     begin match List.assoc_opt s mavail with
     | None
     | Some [] -> None

     | Some (tkn'::tkns') when tkn = tkn' ->
        let mavail' = List.remove_assoc s mavail in
        consume ((s, tkns')::mavail') ((s, tkns)::mreq')

     | Some (_::_) -> None
     end

(* checks if a transition is enabled *)
let is_transition_enabled (n : net) (nm : name) : bool =
  let collect_arcs = List.filter (fun (_, dst, _, _) -> nm = dst) n.arcs in
  if List.length collect_arcs = 0 then false else
    let has_enough pl mreq =
      let mavail = List.assoc pl n.places in
      match consume mavail mreq with | Some _ -> true | None -> false
    in
    List.for_all (fun (src,_,_,m) -> has_enough src m) collect_arcs

let enabled_transitions n =
  List.map fst n.transitions |> List.filter (is_transition_enabled n)
