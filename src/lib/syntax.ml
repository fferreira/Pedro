open Sexplib

type name = string

type entity_marking = name list (* a list of tokens *)

type dir = PlaceToTransition | TransitionToPlace

type vis = Silent | Labelled

type expr =
  | Token of name
  | Place of name * entity_marking
  | Transition of name * vis
  | Arc of name * name * entity_marking
  | Marking of name * name option * (name * entity_marking) list

(* lists a named list of places and their states *)

(* check the scope of pedro expressions *)

type net =
  { tokens: name list
  ; places: (name * entity_marking) list
  ; transitions: (name * vis) list
  ; arcs: (name * name * dir * entity_marking) list
  ; markings: (string * (name option * (name * entity_marking) list)) list
        (* a name and a list of states and their markings *) }

let empty_net =
  {tokens= []; places= []; transitions= []; arcs= []; markings= []}

module Scoped = struct
  include Monad.State (struct
    type s = net
  end)

  let get_tokens =
    let* st = get in
    return st.tokens

  let set_tokens tokens =
    let* st = get in
    set {st with tokens}

  let exists_token x : bool t =
    let* ctx = get_tokens in
    List.mem x ctx |> return

  let add_token nm =
    let* ctx = get_tokens in
    nm :: ctx |> set_tokens

  let get_places =
    let* st = get in
    return st.places

  let set_places places =
    let* st = get in
    set {st with places}

  let exists_place x : bool t =
    let* ctx = get_places in
    try
      let _ = List.assoc x ctx in
      return true
    with Not_found -> return false

  let add_place nm tks =
    let* ctx = get_places in
    (nm, tks) :: ctx |> set_places

  let get_transitions =
    let* st = get in
    return st.transitions

  let set_transitions transitions =
    let* st = get in
    set {st with transitions}

  let exists_transition x : bool t =
    let* trs = get_transitions in
    List.exists (fun (nm, _) -> nm = x) trs |> return

  let add_transition tr =
    let* ctx = get_transitions in
    tr :: ctx |> set_transitions

  let get_arcs =
    let* st = get in
    return st.arcs

  let set_arcs arcs =
    let* st = get in
    set {st with arcs}

  let add_arc src dst d tks =
    let* ctx = get_arcs in
    (src, dst, d, tks) :: ctx |> set_arcs

  let get_markings =
    let* st = get in
    return st.markings

  let set_markings markings =
    let* st = get in
    set {st with markings}

  let add_marking mk =
    let* mks = get_markings in
    mk :: mks |> set_markings
end

module Monadic = struct
  open Scoped

  let rec process_tkn_list : entity_marking -> entity_marking t = function
    | nm :: nms ->
        let* ex = exists_token nm in
        if ex then
          let* nms' = process_tkn_list nms in
          nm :: nms' |> return
        else "Token: " ^ nm ^ " unknown." |> fail
    | [] -> return []

  let check_expr (e : expr) : unit t =
    match e with
    | Token nm ->
        let* ex = exists_token nm in
        if ex then "Token: " ^ nm ^ " defined more than once." |> fail
        else add_token nm
    | Place (nm, tks) ->
        let* ex = exists_place nm in
        if ex then "Place: " ^ nm ^ " defined more than once." |> fail
        else
          let* tks' = process_tkn_list tks in
          add_place nm tks'
    | Transition (nm, vis) ->
        let* ex = exists_transition nm in
        if ex then "Transition: " ^ nm ^ " defined more than once." |> fail
        else add_transition (nm, vis)
    | Arc (src, dst, tks) ->
        let* tks' = process_tkn_list tks in
        (* check if src and dst exist and if they are of different kind
           (place/transition) *)
        let* src_place = exists_place src in
        let* src_trans = exists_transition src in
        if src_place then
          let* et = exists_transition dst in
          if et then add_arc src dst PlaceToTransition tks'
          else dst ^ " is not a transition!" |> fail
        else if src_trans then
          let* ep = exists_place dst in
          if ep then add_arc src dst TransitionToPlace tks'
          else dst ^ " is not a place!" |> fail
        else src ^ " is neither a place nor a transition." |> fail
    | Marking (nm, tag, mks) ->
        let f (pl, tks) =
          let* ep = exists_place pl in
          let* tks' = process_tkn_list tks in
          if ep then return (pl, tks') else pl ^ " is not a state." |> fail
        in
        let* mks' = map f mks in
        add_marking (nm, (tag, mks'))

  let check (es : expr list) : unit t =
    let* _ = Scoped.map check_expr es in
    return ()
end

let validate_net (es : expr list) : net Monad.res =
  let n', res = Scoped.run (Monadic.check es) empty_net in
  match res with Ok _ -> Ok n' | Error s -> Error s

let expr_list_of_net (pn : net) : expr list =
  let tkns = List.map (fun n -> Token n) pn.tokens in
  let places = List.map (fun (n, m) -> Place (n, m)) pn.places in
  let trs = List.map (fun (n, vis) -> Transition (n, vis)) pn.transitions in
  let arcs = List.map (fun (src, dst, _, m) -> Arc (src, dst, m)) pn.arcs in
  let ms =
    List.map (fun (nm, (tag, mks)) -> Marking (nm, tag, mks)) pn.markings
  in
  tkns @ places @ trs @ arcs @ ms

let sexp_of_net (pn : net) : Sexp.t =
  let sexp_of_markings (markings : entity_marking) =
    Sexp.List (List.map (fun x -> Sexp.Atom x) markings)
  in
  let tokens =
    let tokens = pn.tokens in
    let tokens =
      List.map
        (fun token_name ->
          Sexp.List [Sexp.Atom "token"; Sexp.Atom token_name] )
        tokens
    in
    Sexp.List (Sexp.Atom "tokens" :: tokens)
  in
  let places =
    let places = pn.places in
    let places =
      List.map
        (fun (place_name, markings) ->
          Sexp.List
            [ Sexp.Atom "place"
            ; Sexp.Atom place_name
            ; sexp_of_markings markings ] )
        places
    in
    Sexp.List (Sexp.Atom "places" :: places)
  in
  let transitions =
    let transitions = pn.transitions in
    let transitions =
      List.map
        (fun (trn_name, vis) ->
          let vis =
            match vis with
            | Silent -> Sexp.Atom "silent"
            | Labelled -> Sexp.Atom "labelled"
          in
          Sexp.List [Sexp.Atom "transition"; Sexp.Atom trn_name; vis] )
        transitions
    in
    Sexp.List (Sexp.Atom "transitions" :: transitions)
  in
  let arcs =
    let arcs = pn.arcs in
    let arcs =
      List.map
        (fun (frm, dst, dir, markings) ->
          let dir =
            match dir with
            | PlaceToTransition -> Sexp.Atom "place-to-transition"
            | TransitionToPlace -> Sexp.Atom "transition-to-place"
          in
          Sexp.List
            [ Sexp.Atom "arc"
            ; Sexp.Atom frm
            ; Sexp.Atom dst
            ; dir
            ; sexp_of_markings markings ] )
        arcs
    in
    Sexp.List (Sexp.Atom "arcs" :: arcs)
  in
  Sexp.List [Sexp.Atom "petri-net"; tokens; places; transitions; arcs]
