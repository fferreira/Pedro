(* internal state *)

let pn : Syntax.net ref = ref Syntax.empty_net

(* External API *)

let load_from_file (fn : string) : string option =
  try
    match Lib.parse fn (Stdlib.open_in fn) |> Syntax.validate_net with
    | Ok n ->
        pn := n ;
        None
    | Error err -> Some err
  with e -> Some (Printexc.to_string e)

let save_to_file (fn : string) =
  try
    let ch = Stdlib.open_out fn in
    Syntax.expr_list_of_net !pn
    |> Pretty.pp_expr_list |> Stdlib.output_string ch ;
    true
  with _ -> false

let get_enabled_transitions () : string list =
  Opsem.enabled_transitions_with_silent !pn

let do_transition (tr : string) : bool =
  match Opsem.do_transition_with_silent !pn tr with
  | Some pn' ->
      pn := pn' ;
      true
  | None -> false

let has_finished () : bool =
  let final_set = Opsem.get_markings_by_tag "final" !pn in
  List.exists (Opsem.net_matches_marking !pn) final_set

include Version
