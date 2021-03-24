(* this module generates a petri net representation from the global type *)

open Syntax

module G = Nuscrlib.Gtype
module N = Nuscrlib.Names

type state =
  { gen_sym_st : int (* state for the gen_sym function *)
  ; gamma : (N.RoleName.t * name) list (* maps role names to their last known position *)

  ; net : net (* the net we are building *)
  }

module Translation = struct
  include Monad.State (struct
    type s = state
  end)

  let gen_sym () : name t =
    let* st = get in
    let* _ = set {st with gen_sym_st = st.gen_sym_st + 1}  in
    "_" ^ (string_of_int st.gen_sym_st) |> return

  let lookup_gamma (r : N.RoleName.t) : name option t =
    let* st = get in
    List.assoc_opt r st.gamma |> return


  let assert_place pl =
    let* st = get in
    let n = st.net in
    match List.assoc_opt pl n.places with
    | None -> fail (pl ^ " is not a place")
    | Some _ -> return ()

  let create_silent_tr src dst =
    let* _ = assert_place src in
    let* _ = assert_place dst in
    let* st = get in
    let n = st.net in
    let* nm = gen_sym () in
    let net = {n with
                transitions = (nm, Silent)::n.transitions
              ; arcs = (src, nm, PlaceToTransition, [])::(nm, dst, TransitionToPlace, []):: n.arcs
              }
    in
    set {st with net}
end

module Monadic = struct
  open Translation

  (* base operations *)


  let bring (r : N.RoleName.t) (dst : name) : unit t =
    let* from = lookup_gamma r in
    match from with
    | None -> return ()
    | Some src -> create_silent_tr src dst



  (* translation *)

  let translate : G.t ->  Syntax.net t = function
    | G.MessageG (_m, _src, _dst, _cont) -> assert false
    | G.MuG (_x, _vars, _cont) -> assert false
    | G.TVarG (_x, exprs, _cont) when Util.is_empty exprs-> fail "Unsupported: TVarG cannot have refinements."
    | G.TVarG (_x, _, _cont) -> assert false
    | G.ChoiceG (_, _) -> assert false
    | EndG -> assert false
    | G.CallG _ -> fail "Unsopported: cannot call sub protocols."

end
