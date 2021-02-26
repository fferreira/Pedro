open Sexplib

type name = string

type entity_marking = (name * int) list (* a list of names with multiplicity *)

type dir = PlaceToTransition | TransitionToPlace

type vis = Silent | Labelled

type expr
  = Token of name * name (* token name and its sort *)
  | Place of name * entity_marking
  | Transition of name * vis
  | Arc of name * name * entity_marking

(* check the scope of pedro expressions *)



type net =
  { tokens : (name * name) list
  ; places : (name * entity_marking) list
  ; transitions : (name * vis) list
  ; arcs : (name * name * dir * entity_marking) list
  }

let empty_net =
  { tokens = []
  ; places  = []
  ; transitions = []
  ; arcs = []
  }

module Scoped =
  struct
    include Monad.State(struct type s = net end)

    let get_tokens =
      let* st = get in
      return st.tokens

    let set_tokens tokens =
      let* st = get in
      set {st with tokens}

    let exists_token x : bool t =
      let* ctx = get_tokens in
      try
        let _ = List.assoc x ctx in return true
      with
        Not_found -> return false

    let lookup_token x : name t =
      let* ctx = get_tokens in
      try
        List.assoc x ctx |> return
      with
        Not_found -> failwith @@ "Unknown token " ^ x

    let add_token nm sort =
      let* ctx = get_tokens in
      (nm, sort):: ctx |> set_tokens

    let get_places =
      let* st = get in
      return st.places

    let set_places places =
      let* st = get in
      set {st with places}

    let exists_place x : bool t =
      let* ctx = get_places in
      try
        let _ = List.assoc x ctx in return true
      with
        Not_found -> return false

    let add_place nm tks =
      let* ctx = get_places in
      (nm, tks):: ctx |> set_places

    let get_transitions =
      let* st = get in
      return st.transitions

    let set_transitions transitions =
      let* st = get in
      set {st with transitions}

    let exists_transition x : bool t =
      let* trs = get_transitions in
      List.exists (fun (nm, _) -> nm = x) trs |> return

    let add_transition tr  =
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
      (src, dst, d, tks):: ctx |> set_arcs
  end

module Monadic =
  struct
    open Scoped

    let validate_tkn_list (tl : entity_marking) : unit t =
      let val_one (nm, n) =
        let* te = exists_token nm in
        if te && n > 0
        then return ()
        else
          "Token: " ^ nm ^ " does not exist!" |> fail
      in
      let* _ = Scoped.map val_one tl in
      return ()


    let check_expr (e : expr) : unit t =
      match e with
      | Token (nm, sort) ->
         let* ex = exists_token nm in
         if ex then
           "Token: " ^ nm ^ " defined more than once" |> fail
         else
           add_token nm sort

      | Place (nm, tks) ->
         let* ex = exists_place nm in
         if ex then
           "Place: " ^ nm ^ " defined more than once" |> fail
         else
           let* _ = validate_tkn_list tks in
           add_place nm tks

      | Transition (nm, vis) ->
         let* ex = exists_transition nm in
         if ex then
           "Transition: " ^ nm ^ " defined more than once" |> fail
         else add_transition (nm, vis)

      | Arc (src, dst, tks) ->
         let*  _ = validate_tkn_list tks in

         (* check if src and dst exist and if they are of different kind (place/transition) *)
         let* src_place = exists_place src in
         let* src_trans = exists_transition src in
         if src_place then
           let* et = exists_transition dst in
           if et then add_arc src dst PlaceToTransition tks
           else dst ^ " is not a transition!" |> fail
         else
           if src_trans then
             let* ep = exists_place dst in
             if ep then add_arc src dst TransitionToPlace tks
             else dst ^ " is not a place!" |> fail
           else
             src ^ " is neither a place nor a transition." |> fail

    let check (es : expr list) : unit t =
      let* _ = Scoped.map check_expr es in
      return ()

  end

let validate_net
      (es : expr list)
    : net Monad.res =
  let (n', res) = Scoped.run (Monadic.check es) empty_net in
  match res with
  | Monad.Yes _ -> Yes n'
  | Monad.No s -> No s

let sexp_of_net (pn : net) : Sexp.t =
  let sexp_of_markings markings =
    Sexp.List (
      List.map (
        fun (name, mult) ->
          Sexp.List [Sexp.Atom name; Sexp.Atom (Int.to_string mult)]
      ) markings
    )
  in
  let tokens =
    let tokens = pn.tokens in
    let tokens = List.map (fun (token_name, kind) ->
      Sexp.List
      [ Sexp.Atom "token"
      ; Sexp.Atom token_name
      ; Sexp.Atom kind
      ]
    ) tokens
    in
    Sexp.List (Sexp.Atom "tokens" :: tokens)
  in
  let places =
    let places = pn.places in
    let places = List.map (fun (place_name, markings) ->
      Sexp.List
      [ Sexp.Atom "place"
      ; Sexp.Atom place_name
      ; sexp_of_markings markings
      ]
      ) places
    in
    Sexp.List (Sexp.Atom "places" :: places)
  in
  let transitions =
    let transitions = pn.transitions in
    let transitions = List.map (fun (trn_name, vis) ->
      let vis = match vis with
        | Silent -> Sexp.Atom "silent"
        | Labelled -> Sexp.Atom "labelled"
      in
      Sexp.List
      [ Sexp.Atom "transition"
      ; Sexp.Atom trn_name
      ; vis
      ]
      ) transitions
    in
    Sexp.List (Sexp.Atom "transitions" :: transitions)
  in
  let arcs =
    let arcs = pn.arcs in
    let arcs = List.map (fun (frm, dst, dir, markings) ->
      let dir = match dir with
        | PlaceToTransition -> Sexp.Atom "place-to-transition"
        | TransitionToPlace -> Sexp.Atom "transition-to-place"
      in
      Sexp.List
      [ Sexp.Atom "arc"
      ; Sexp.Atom frm
      ; Sexp.Atom dst
      ; dir
      ; sexp_of_markings markings
      ]
      ) arcs
    in
    Sexp.List (Sexp.Atom "arcs" :: arcs)
  in
  Sexp.List [
    Sexp.Atom "petri-net"
  ; tokens
  ; places
  ; transitions
  ; arcs
  ]
