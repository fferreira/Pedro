(* this module generates a petri net representation from the global type *)

open Syntax
module G = Nuscrlib.Gtype
module N = Nuscrlib.Names

(* utilities *)

let rec participants (n : G.t) : N.RoleName.t list =
  let uc = Util.uniq_cons in
  match n with
  | G.MessageG (_, src, dst, cont) -> uc src @@ uc dst @@ participants cont
  | G.MuG (_, vars, _) when not (Util.is_empty vars) ->
      failwith "Error: Unsupported refined protocol"
  | G.MuG (_, _, cont) -> participants cont
  | G.TVarG (_, exprs, _) when not (Util.is_empty exprs) ->
      failwith "Error: Unsupported refined protocol"
  | G.TVarG (_, _, _) -> []
  | G.ChoiceG (r, conts) ->
      uc r (Util.uniq @@ List.concat @@ List.map participants conts)
  | G.EndG -> []
  | G.CallG (src, _, roles, cont) ->
      uc src (Util.uniq @@ roles @ participants cont)
  | G.ParG conts ->
      (* TODO potential perforamnce problem *)
      Util.uniq @@ List.concat_map participants conts

(* monadic state *)
type state =
  { gen_sym_st: int (* state for the gen_sym function *)
  ; gamma: (N.RoleName.t * name) list
        (* maps role names to their last known position *)
  ; delta: (N.TypeVariableName.t * (N.RoleName.t * name) list) list
        (* maps type variables to a map of roles to places to go when the
           variable is found *)
  ; net: net (* the net we are building *) }

let initial = {gen_sym_st= 0; gamma= []; delta= []; net= empty_net}

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

  let get_gamma =
    let* st = get in
    st.gamma |> return

  let set_gamma gamma =
    let* st = get in
    set {st with gamma}

  let get_delta =
    let* st = get in
    st.delta |> return

  let set_delta delta =
    let* st = get in
    set {st with delta}

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

  (* generates 'n' fresh names *)
  let rec gen_sym_n (n : int) : name list t =
    match n with
    | 0 -> return []
    | n ->
        let* nm = gen_sym in
        let* nms = gen_sym_n (n - 1) in
        return @@ nm :: nms

  (* looks up a role in gamma *)
  let lookup_gamma (r : N.RoleName.t) : name option t =
    let* st = get in
    List.find_map
      (fun (r', pl) -> if N.RoleName.equal r r' then Some pl else None)
      st.gamma
    |> return

  let update_gamma (r : N.RoleName.t) (nm : name) : unit t =
    let* st = get in
    let gamma' =
      List.filter
        (fun (r', _) -> if N.RoleName.equal r r' then false else true)
        st.gamma
    in
    set {st with gamma= (r, nm) :: gamma'}

  let update_gamma_n (rs : N.RoleName.t list) (pl : name) : unit t =
    let* _ = map (fun r -> update_gamma r pl) rs in
    return ()

  let lookup_delta (r : N.TypeVariableName.t) :
      (N.RoleName.t * name) list option t =
    let* st = get in
    List.find_map
      (fun (r', pl) ->
        if N.TypeVariableName.equal r r' then Some pl else None )
      st.delta
    |> return

  let update_delta (r : N.TypeVariableName.t)
      (dict : (N.RoleName.t * name) list) : unit t =
    let* st = get in
    let delta' =
      List.filter
        (fun (r', _) -> if N.TypeVariableName.equal r r' then false else true)
        st.delta
    in
    set {st with delta= (r, dict) :: delta'}

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

  let update_marking nm ?(tag = None) m =
    let* n = get_net in
    let markings = (nm, (tag, m)) :: List.remove_assoc nm n.markings in
    set_net {n with markings}

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
  let create_message (msg : message) : (name * name) t =
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
      ; transitions=
          (msg.tr_send, Labelled) :: (msg.tr_recv, Labelled) :: n.transitions
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

  (* takes a list of roles, and generates an 'n' split for them to implement
     par. It returns the split point and all the resulting branches. *)
  let create_par (p : name) (rns : name list) (n : int) : name list t =
    let* ps = gen_sym_n n in
    let for_each_part (rn : name) : unit t =
      let* trn = gen_sym in
      let trs_split =
        List.map (fun p -> (trn, p, TransitionToPlace, [rn])) ps
      in
      let* n = get_net in
      let net =
        { n with
          transitions= (trn, Silent) :: n.transitions
        ; arcs= (p, trn, PlaceToTransition, [rn]) :: trs_split @ n.arcs }
      in
      set_net net
    in
    let* n = get_net in
    let net = {n with places= List.map (fun p' -> (p', [])) ps @ n.places} in
    let* _ = set_net net in
    let* _ = map for_each_part rns in
    return ps

  (* token from role, it adds it if it is new *)
  let tkr (r : N.RoleName.t) : name t =
    let* st = get in
    let n = st.net in
    let nm = N.RoleName.user r in
    if List.mem nm n.tokens then return nm
    else
      let net = {n with tokens= nm :: n.tokens} in
      let* _ = set {st with net} in
      return nm

  (* token from role, it adds it if it is new *)
  let tkm (r : Nuscrlib.Gtype.message) : name t =
    let* st = get in
    let n = st.net in
    let nm = N.LabelName.user r.label in
    if List.mem nm n.tokens then return nm
    else
      let net = {n with tokens= nm :: n.tokens} in
      let* _ = set {st with net} in
      return nm

  (* looks up a place or creates a new place and it puts it *)
  let lookup_gamma_or_create r =
    let* r_from = tkr r in
    let* p1_opt = lookup_gamma r in
    let* p1 =
      match p1_opt with
      | None ->
          let* p1 = gen_sym in
          let* _ = add_place p1 [r_from] in
          let* _ = update_gamma r p1 in
          return p1
      | Some p1 -> return p1
    in
    return p1
end

module Monadic = struct
  open Translation

  (* base operations *)

  (* brings r to dst and return bool if it could be done *)
  let bring (r : N.RoleName.t) (dst : name) : bool t =
    let trx_exists (r : name) (src : name) (dst : name) : bool t =
      let* n = get_net in
      let is_silent trx =
        try List.assoc trx n.transitions = Silent with _ -> false
      in
      let trx_ex =
        Option.bind
          (List.find_opt
             (fun (s, _, _, m) -> s = src && List.mem r m)
             n.arcs )
          (fun (_, trx, _, _) ->
            if is_silent trx then
              List.find_opt
                (fun (s, d, _, m) -> s = trx && d = dst && List.mem r m)
                n.arcs
            else None )
        |> Option.is_some
      in
      return trx_ex
    in
    let* from = lookup_gamma r in
    let* tk_r = tkr r in
    (* token for the role *)
    match from with
    | None -> return false (* no bringing possible *)
    | Some src ->
        let* trx_ex = trx_exists tk_r src dst in
        (* check if there is a single transtion that does this *)
        if src = dst || trx_ex then return true
        else
          let* _ = create_silent_tr src dst tk_r in
          return true
  (* brought the token *)

  (* bring a participant to a place (or add it if it's new) and update gamma *)
  let bring_or_create (part : N.RoleName.t) (pl : name) : unit t =
    let* part_exists = bring part pl in
    let* _ = update_gamma part pl in
    let* tok = tkr part in
    if not part_exists then add_tokens_to_place pl [tok] else return ()

  (* translation *)

  let rec translate : G.t -> unit t = function
    | G.MessageG (m, src, dst, cont) ->
        let* r_from = tkr src in
        (* translate source, as token *)
        let* r_to = tkr dst in
        (* translate destination, as token *)
        let* label = tkm m in
        (* translate label *)
        let* p1 = lookup_gamma_or_create src in
        let msg =
          { p1
          ; label
          ; r_from
          ; r_to
          ; tr_send= transition_name r_from r_to PlaceToTransition label
          ; tr_recv= transition_name r_from r_to TransitionToPlace label }
        in
        let* p2, p3 = create_message msg in
        let* _ = bring_or_create dst p2 in
        let* _ = update_gamma src p2 in
        let* _ = update_gamma dst p3 in
        (* after this, dst has to move to p3 *)
        translate cont
    | G.MuG (_, vars, _) when not (Util.is_empty vars) ->
        fail "Unsupported: MuG cannot have refinements."
    | G.MuG (x, _, cont) ->
        let parts = participants cont in
        let* pl =
          match parts with
          | [] ->
              let* pl = gen_sym in
              let* _ = add_place pl [] in
              return pl
          | p :: _ -> lookup_gamma_or_create p
        in
        let* _ = map (fun p -> bring_or_create p pl) parts in
        let* _ = update_delta x (List.map (fun p -> (p, pl)) parts) in
        translate cont
    | G.TVarG (_, exprs, _) when not (Util.is_empty exprs) ->
        fail "Unsupported: TVarG cannot have refinements."
    | G.TVarG (x, _, _) ->
        let* part_pls = lookup_delta x in
        let* part_pls' =
          match part_pls with
          | Some ppls -> return ppls
          | None ->
              fail @@ "Variable: " ^ N.TypeVariableName.user x
              ^ " not found! (" ^ N.TypeVariableName.show x ^ ")"
        in
        let* _ = map (fun (p, pl) -> bring p pl) part_pls' in
        return ()
    | G.ChoiceG (r, conts) ->
        let add_cont cont =
          let* gamma = get_gamma in
          let* pl = gen_sym in
          let* _ = add_place pl [] in
          let* _ = bring_or_create r pl in
          let* _ = translate cont in
          set_gamma gamma
          (* We could use a reader monad to avoid this, but this is simpler
             for now *)
        in
        let* _ = lookup_gamma_or_create r in
        let* _ = map add_cont conts in
        return ()
    | EndG ->
        let* st = get in
        let parts = List.map fst st.gamma in
        let f p =
          let* tk = tkr p in
          let* plopt = lookup_gamma p in
          let pl =
            match plopt with
            | None -> failwith "Violation: p has to be in gamma"
            | Some pl -> pl
          in
          return (pl, [tk])
        in
        let* m = map f parts in
        let* nm = gen_sym in
        update_marking nm ~tag:(Some "final") m
    | G.CallG _ ->
        fail
          "Unsopported: cannot call sub protocols. (In the future we could \
           inline them)"
    | G.ParG conts ->
        let rs =
          Util.uniq_eq N.RoleName.( = ) @@ List.concat_map participants conts
        in
        let* rnms = map tkr rs in
        let n = List.length conts in
        let* p = lookup_gamma_or_create (List.hd rs) in
        (* create the net to split *)
        let* ps = create_par p rnms n in
        (* bring the participants *)
        let* _ = map (fun r -> bring_or_create r p) rs in
        let for_each_cont (p', c) =
          let* _ = update_gamma_n rs p' in
          translate c
        in
        let* _ = map for_each_cont @@ List.combine ps conts in
        return ()
end

let net_of_global_type n =
  let n', res = Translation.run (Monadic.translate n) initial in
  match res with Ok _ -> Ok n'.net | Error s -> Error s
