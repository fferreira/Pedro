open Syntax

let project (n : net) (tok : name) : net =
  let arc_by_tok tok (src, dst, dir, m) =
    if List.mem tok m then Some (src, dst, dir, List.filter (( = ) tok) m)
    else None
  in
  let place_by_tok tok (nm, m) = (nm, List.filter (( = ) tok) m) in
  let place_by_tok' tok (nm, m) =
    if List.mem tok m then Some (nm, List.filter (( = ) tok) m) else None
  in
  let marking_by_tok tok (nm, (otag, ms)) =
    let ms' = List.filter_map (place_by_tok' tok) ms in
    (nm, (otag, ms'))
  in
  (* remove orphan transitions and places *)
  let gc n =
    (* return the place first, and transitio secon *)
    let separate = function
      | src, dst, PlaceToTransition, _ -> (src, dst)
      | src, dst, TransitionToPlace, _ -> (dst, src)
    in
    let plcs, trxs = List.map separate n.arcs |> List.split in
    let places = List.filter (fun (nm, _) -> List.mem nm plcs) n.places in
    let transitions =
      List.filter (fun (nm, _) -> List.mem nm trxs) n.transitions
    in
    {n with places; transitions}
  in
  let arcs = List.filter_map (arc_by_tok tok) n.arcs in
  let markings = List.map (marking_by_tok tok) n.markings in
  let places = List.map (place_by_tok tok) n.places in
  gc {n with arcs; markings; places}
