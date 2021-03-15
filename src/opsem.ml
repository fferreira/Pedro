open Syntax
open Util

let consume_one_from_marking (m : entity_marking) (t: name) : entity_marking option =
  if List.mem t m then Some (remove_one ((=) t) m) else None

(* consumes mreq resources from mavail and returns the new mavail if enough resources are present*)
let rec consume_from_marking (mavail : entity_marking) (mreq: entity_marking) : entity_marking option =
match mreq with
| [] -> Some mavail
| tkn::tkns ->
   Option.bind (consume_one_from_marking mavail tkn) (fun mavail' ->
       consume_from_marking mavail' tkns)

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
        let places = (dst, mdest @ mprov)::List.remove_assoc dst n.places in
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

(* get place pl to have resource t by firing silent transitions, return None if it is impossible *)
let rec get_silent_resource (pl : name) (t : name) (n : net) fuel : net option =
  if fuel = 0 then None else begin
  "visiting: " ^ pl ^ " asking for: " ^ t |> print_endline ;
  let is_silent tr = List.assoc tr n.transitions = Silent in
  let has_token tkn m = consume_one_from_marking m tkn <> None in
  (* get the silent arcs that bring at least token t to place pl *)
  let arcs_provide pl t n =
    List.filter
      (fun (src, dst, _, m) -> pl = dst && is_silent src && has_token t m)
      n.arcs
  in
  (* get the arcs that are required by the transition *)
  let arcs_require tr n =
    List.filter
      (fun (_, dst, _, _m) -> tr = dst (* && has_token t m *))
      n.arcs
  in
  (* try to fire this arc and return the new net if possible *)
  let try_fire_arc (_, dst, _, _)  n  =
    (* all these arcs to transition have to be satisfied *)
    let rarcs = arcs_require dst n in
    let get_arc_requirements (_, dst, _, m) n : net option =
         match get_silent_resource_many dst m n with (* replace with option bind *)
         | None -> None
         | Some n' ->
            Some n' (* this is an unnecesary eta-long version REMOVE *)
    in
    (* try and get all the requirements for the arcs to transaction *)
    let rec get_all_requirements rarcs n : net option =
      match rarcs with
      | [] -> Some n
      | arc :: rarcs' ->
         Option.bind (get_arc_requirements arc n) (fun n' ->
           get_all_requirements rarcs' n')
    in
    Option.bind (get_all_requirements rarcs n) (fun n' ->
        (* this violation is just for sanity checking, because this phase cannot fail if we got this far *)
        Some(Option.value
               (do_transition n' dst)
               ~default:(failwith "VIOLATION: all resources must have been collected by now")))
  in
  (* find the first source for the resource *)
  let rec check_all_arcs n arcs =
      match arcs with
      | [] -> None (* no candidate arcs can be triggered *)

      | arc :: arcs' ->
         match try_fire_arc arc n with
         | Some n' -> Some n'
         | None -> check_all_arcs n arcs'
  in
  if List.mem t (List.assoc pl n.places) then Some n
  else
    let arcs = arcs_provide pl t n in
    if List.length arcs = 0 then None (* No arcs can bring the requried resource *)
    else
      let r = check_all_arcs n arcs in
      "visiting: " ^ pl ^ " asking for: " ^ t |> print_endline ;
      r
end

and get_silent_resource_many (pl : name) (m : entity_marking) (n : net) : net option =
  match m with
  | [] -> Some n
  | tk::m' ->
       Option.bind (get_silent_resource pl tk n 15) (fun n' ->
           get_silent_resource_many pl m' n')



(* do named transition, gather resources from silent transitions if needed *)
let do_transition_with_silent (n : net) (t: name) : net option =
  if is_transition_enabled n t then
    do_transition n t
  else
    let rarcs = (* these are the arcs that bring the required resources *)
      List.filter (fun (_, dst, _, _) -> t = dst) n.arcs
    in

   let rec bring_resources n = function
     | (src, _, _, m) :: rarcs ->
        Option.bind (get_silent_resource_many src m n) (fun n' ->
            bring_resources n' rarcs)
     | [] -> Some n
   in
   Option.bind (bring_resources n rarcs) (fun n' ->
       Some(Option.value
              (do_transition n' t)
              ~default:(failwith "VIOLATION: all resources must have been collected by now")))


let is_transition_enabled_with_silent (n : net) (nm : name) : bool =
  match do_transition_with_silent n nm with
  | Some _ -> true
  | None -> false

let enabled_transitions_with_silent n =
  List.map fst n.transitions |> List.filter (is_transition_enabled_with_silent n)
