open Syntax

let rec remove_one x = function
| [] -> []
| y::ys when x = y -> ys
| y::ys -> y::remove_one x ys

(* consumes mreq resources from mavail and returns the new mavail if enough resources are present*)
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

(* (\* consumes mreq resources from mavail and returns the new mavail and the remaining resources to consume *\)
 * let rec partial_consume_from_marking (mavail : entity_marking) (mreq: entity_marking) : entity_marking * entity_marking =
 *   match mreq with
 *     [] -> mavail, []
 *   | (_, [])::mreq' -> partial_consume_from_marking mavail mreq'
 *   | (s, tkn::tkns)::mreq' ->
 *      begin match List.assoc_opt s mavail with
 *      | None
 *      | Some [] -> (\* there are no s resources in mavail, continue *\)
 *         let mavail', mreq' = partial_consume_from_marking mavail mreq' in
 *         mavail', (s, tkn::tkns)::mreq'
 *
 *      | Some (tkn'::tkns') when tkn = tkn' ->
 *         let mavail' = List.remove_assoc s mavail in
 *         partial_consume_from_marking ((s, tkns')::mavail') ((s, tkns)::mreq')
 *
 *      | Some (_::_) -> mavail, mreq
 *      end *)

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



(* do named transition if available *)
let do_transition (n : net) (t : name) : net option =
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

(* do named transition, gather resources from silent transitions if needed *)
let do_transition_with_silent (n : net) (t: name) : net =
  let remove_token_from_marking (t : name) (m : entity_marking) : entity_marking option =
    let sort = List.assoc t n.tokens in
    let av_tkns = List.assoc_opt sort m |> Option.value ~default:[] in
    if List.length av_tkns = 0 then None
    else if List.hd av_tkns = t then Some ((sort, List.tl av_tkns)::List.remove_assoc sort m)
    else None
  in

  (* use silent transitions to bring to place pl token tkn from net n *)
  let (* rec *) _use_silent pl tkn n : net option =
    let is_silent tr = List.assoc tr n.transitions = Silent in
    let has_token tkn m = remove_token_from_marking tkn m <> None in
    let silent_arcs = (* these are silent arcs that bring the resource we need *)
      List.filter
        (fun (src, dst, _, m) -> pl = dst && is_silent src && has_token tkn m)
        n.arcs
    in
    if List.length silent_arcs = 0 then None
    else
      (* there's at least one arc that could bring the token *)
      let (* rec *)  _find_first = function
        | [] -> None
        | (_src, _dst, _dir, _mreq)::_arcs ->
           (* if has_token ? src *)
           assert false
      in
      assert false
    and _bring_to_silent _tr _tkn _n = None

  in

  let _consume_one (n : net) src t : net =
    let m = List.assoc src n.places in
    let m_opt = remove_token_from_marking t m in
    match m_opt with
    | None -> n (* here go fishing for the resource *)
    | Some m' -> {n with places = (src, m')::List.remove_assoc src n.places }
  in

  let req_arcs = List.filter (fun (_, dst, _, _) -> t = dst) n.arcs in
  if List.length req_arcs = 0 then n
  else
    assert false
