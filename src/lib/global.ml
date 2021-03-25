(* this module generates a petri net representation from the global type *)

open Syntax
module G = Nuscrlib.Gtype
module N = Nuscrlib.Names

(* monadic state *)
type state =
  { gen_sym_st: int (* state for the gen_sym function *)
  ; gamma: (N.RoleName.t * name) list
        (* maps role names to their last known position *)
  ; net: net (* the net we are building *) }

(* message definition to be added to the net *)
type message =
  { p1: name (* place that sends *)
  ; label: name (* token that is the message label *)
  ; r_from: name (* token that is the sending participant *)
  ; r_to: name (* token that is the receiving participant *)
  ; tr_send: name (* transition that is sending transition *)
  ; tr_recv: name (* transition that is the receiving transition *) }

let transition_name src dst dir msg =
  let act =
    match dir with PlaceToTransition -> "!" | TransitionToPlace -> "?"
  in
  src ^ act ^ dst ^ "<" ^ msg ^ ">"

module Translation = struct
  include Monad.State (struct
    type s = state
  end)

  let get_net =
    let* st = get in
    st.net |> return

  let set_net net =
    let* st = get in
    set {st with net}

  (* generates a fresh name *)
  let gen_sym : name t =
    let* st = get in
    let* _ = set {st with gen_sym_st= st.gen_sym_st + 1} in
    "_" ^ string_of_int st.gen_sym_st |> return

  (* looks up a role in gamma *)
  let lookup_gamma (r : N.RoleName.t) : name option t =
    let* st = get in
    List.assoc_opt r st.gamma |> return

  let update_gamma (r : N.RoleName.t) (nm : name) : unit t =
    let* st = get in
    set {st with gamma= (r, nm) :: List.remove_assoc r st.gamma}

  (* Operations on the net *)

  (* asserts that pl is a place in the current net *)
  let assert_place pl =
    let* n = get_net in
    match List.assoc_opt pl n.places with
    | None -> fail (pl ^ " is not a place")
    | Some _ -> return ()

  (* asserts that pl is not a place in the current net *)
  let assert_not_place pl =
    let* st = get in
    let n = st.net in
    match List.assoc_opt pl n.places with
    | None -> return ()
    | Some _ -> fail (pl ^ " already exists")

  (* asserts that tkn is a token in the current net *)
  let assert_token tkn =
    let* st = get in
    let n = st.net in
    if List.mem tkn n.tokens then return ()
    else fail (tkn ^ " is not a token")

  (* asserts that tr is not a transition in the current net *)
  let assert_not_transition tr =
    let* st = get in
    let n = st.net in
    match List.assoc_opt tr n.transitions with
    | None -> return ()
    | Some _ -> fail (tr ^ " transition already exists")

  let add_place pl m =
    let* _ = assert_not_place pl in
    let* n = get_net in
    set_net {n with places= (pl, m) :: n.places}

  let add_tokens_to_place pl m =
    let* n = get_net in
    let* places =
      match List.assoc_opt pl n.places with
      | None -> fail @@ pl ^ ": unknown place."
      | Some m' -> (pl, m @ m') :: List.remove_assoc pl n.places |> return
    in
    set_net {n with places}

  (* create a silent transition between two places *)
  let create_silent_tr src dst tkn =
    let* _ = assert_place src in
    let* _ = assert_place dst in
    let* n = get_net in
    let* nm = gen_sym in
    let net =
      { n with
        transitions= (nm, Silent) :: n.transitions
      ; arcs=
          (src, nm, PlaceToTransition, [tkn])
          :: (nm, dst, TransitionToPlace, [tkn]) :: n.arcs }
    in
    set_net net

  (* creates a new message in the net and returns the buffer place, and the
     final place *)
  let create_messge (msg : message) : (name * name) t =
    let* _ = assert_place msg.p1 in
    let* _ = assert_not_transition msg.tr_send in
    let* _ = assert_not_transition msg.tr_recv in
    let* _ = assert_token msg.r_from in
    let* _ = assert_token msg.r_to in
    let* _ = assert_token msg.label in
    let* p2 = gen_sym in
    let* p3 = gen_sym in
    let* n = get_net in
    let net =
      { n with
        places= (p2, []) :: (p3, []) :: n.places
      ; arcs=
          (msg.p1, msg.tr_send, PlaceToTransition, [msg.r_from])
          ::
          (msg.tr_send, p2, TransitionToPlace, [msg.r_from; msg.label])
          ::
          (p2, msg.tr_recv, PlaceToTransition, [msg.r_to; msg.label])
          :: (msg.tr_recv, p3, TransitionToPlace, [msg.r_to]) :: n.arcs }
    in
    let* _ = set_net net in
    return (p2, p3)

  (* token from role, it adds it if it is new *)
  let tkr (r : N.RoleName.t) : name t =
    let* st = get in
    let n = st.net in
    let nm = N.RoleName.show r in
    if List.mem nm n.tokens then return nm
    else
      let net = {n with tokens= nm :: n.tokens} in
      let* _ = set {st with net} in
      return nm

  (* token from role, it adds it if it is new *)
  let tkm (r : Nuscrlib.Gtype.message) : name t =
    let* st = get in
    let n = st.net in
    let nm = N.LabelName.show r.label in
    if List.mem nm n.tokens then return nm
    else
      let net = {n with tokens= nm :: n.tokens} in
      let* _ = set {st with net} in
      return nm
end

module Monadic = struct
  open Translation

  (* base operations *)

  let bring (r : N.RoleName.t) (dst : name) : bool t =
    let* from = lookup_gamma r in
    let* tk_r = tkr r in
    (* token for the role *)
    match from with
    | None -> return false (* no bringing possible *)
    | Some src ->
        let* _ = create_silent_tr src dst tk_r in
        return true
  (* brought the token *)

  (* translation *)

  let rec translate : G.t -> Syntax.net t = function
    | G.MessageG (m, src, dst, cont) ->
        let* t_src = tkr src in
        (* translate source, as token *)
        let* t_dst = tkr dst in
        (* translate destination, as token *)
        let* l = tkm m in
        (* translate label *)
        let* p1_opt = lookup_gamma src in
        let* p1 =
          match p1_opt with
          | None ->
              let* p1 = gen_sym in
              let* _ = add_place p1 [t_src] in
              return p1
          | Some p1 -> return p1
        in
        let msg =
          { p1
          ; label= l
          ; r_from= t_src
          ; r_to= t_dst
          ; tr_send= transition_name t_src t_dst PlaceToTransition l
          ; tr_recv= transition_name t_src t_dst TransitionToPlace l }
        in
        let* p2, p3 = create_messge msg in
        let* t_src_exists = bring src p2 in
        let* _ =
          if not t_src_exists then add_tokens_to_place p2 [t_dst]
          else return ()
        in
        let* _ = update_gamma src p2 in
        let* _ = update_gamma dst p3 in
        translate cont
    | G.MuG (_x, _vars, _cont) -> assert false
    | G.TVarG (_, exprs, _) when Util.is_empty exprs ->
        fail "Unsupported: TVarG cannot have refinements."
    | G.TVarG (_x, _, _cont) -> assert false
    | G.ChoiceG (_, _) -> assert false
    | EndG -> assert false
    | G.CallG _ -> fail "Unsopported: cannot call sub protocols."
end
