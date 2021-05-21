module G = Nuscrlib.Gtype
module N = Nuscrlib.Names

type state =
  { delta: N.TypeVariableName.t list (* ungarded recursion variables *)
  ; epsilon: N.TypeVariableName.t list (* guarded recursion variables *)
  ; psi: N.RoleName.t list (* enabled roles *)
  ; phi: (N.RoleName.t * N.RoleName.t list) list
        (* sets of participants that have to be informed of a choice *)
  ; labels: N.LabelName.t list }

let is_enabled r st =
  List.find_opt (N.RoleName.equal r) st.psi |> Option.is_some

module WellFormed = struct
  include Monad.State (struct
    type s = state
  end)

  (* delta operations *)
  let new_var x =
    let* st = get in
    set {st with delta= x :: st.delta}

  (* epsilon operations *)
  let assert_var_guarded x =
    let* st = get in
    if List.exists (fun y -> N.TypeVariableName.equal x y) st.epsilon then
      return ()
    else fail @@ N.TypeVariableName.show x ^ " is ungarded."

  (* delta/epsilon operations *)

  let forget_recursion =
    let* st = get in
    set {st with delta= []; epsilon= []}

  let guard_recursion =
    let* st = get in
    set {st with delta= []; epsilon= st.delta @ st.epsilon}

  (* psi operations *)

  let enable r =
    let* st = get in
    if is_enabled r st then return () else set {st with psi= r :: st.psi}

  let assert_enabled r =
    let* st = get in
    if List.exists (fun y -> N.RoleName.equal r y) st.psi then return ()
    else fail @@ "Role:" ^ N.RoleName.show r ^ "was not enabled."

  (* phi operations *)

  let to_be_informed r rs =
    let* st = get in
    set
      { st with
        phi=
          (r, List.filter (fun r' -> N.RoleName.equal r r' |> not) rs)
          :: st.phi }

  let inform r =
    let* st = get in
    let inform_choice (rch, rs) =
      let rs' = List.filter (fun r' -> N.RoleName.equal r r' |> not) rs in
      return (rch, rs')
    in
    let* phi = map inform_choice st.phi in
    set {st with phi}

  (* phi is well informed: meaning there are no roles that still need to be
     informed *)
  let wi =
    let* st = get in
    let error_string () =
      let phi = st.phi in
      String.concat "\n"
      @@ List.map
           (fun (r, rs) ->
             "Choice by role: " ^ N.RoleName.show r ^ " is not informed to: "
             ^ String.concat ", "
             @@ List.map N.RoleName.user rs )
           phi
    in
    if List.for_all (fun (_, rs) -> Util.is_empty rs) st.phi then return ()
    else fail @@ "Phi is not well informed.\n" ^ error_string ()

  (* label operations *)
  let validate_and_add l =
    let* st = get in
    match List.find_opt (N.LabelName.equal l) st.labels with
    | Some _ -> fail @@ "Duplicated label name:" ^ N.LabelName.show l
    | None -> set {st with labels= l :: st.labels}
end

module Monadic = struct
  open WellFormed

  let rec wf (g : G.t) : unit t =
    let* st = get in
    match g with
    | G.MessageG (msg, src, dst, cont) when is_enabled src st ->
        let* _ = enable dst in
        let* _ = validate_and_add msg.label in
        let* _ = guard_recursion in
        let* _ = inform dst in
        wf cont
    | G.MessageG (msg, _, dst, cont) when is_enabled dst st ->
        let* _ = validate_and_add msg.label in
        let* _ = guard_recursion in
        let* _ = inform dst in
        wf cont
    | G.MessageG (msg, src, dst, _) ->
        fail @@ "Neither the sender (" ^ N.RoleName.show src
        ^ "), nor the receiver (" ^ N.RoleName.show dst
        ^ ") are enabled for label:"
        ^ N.LabelName.show msg.label
    | G.MuG (_, vars, _) when not (Util.is_empty vars) ->
        fail "Error: Unsupported refined protocol"
    | G.MuG (x, _, cont) ->
        let* _ = new_var x in
        wf cont
    | G.TVarG (_, exprs, _) when not (Util.is_empty exprs) ->
        fail "Error: Unsupported refined protocol"
    | G.TVarG (x, _, _) ->
        let* _ = assert_var_guarded x in
        wi
    | G.ChoiceG (r, conts) ->
        let* _ = assert_enabled r in
        let* st = get in
        let validate_branch g =
          let* _ = to_be_informed r (Global.participants g) in
          let* _ = wf g in
          set st
          (* TODO: this is a reader monad more than a state monad *)
        in
        let* _ = map validate_branch conts in
        return ()
    | G.EndG -> return ()
    | G.CallG (_src, _, _roles, _cont) ->
        fail "Calling sub protocols is not yet supported."
    | G.ParG conts ->
        let* st = get in
        let validate_branch g =
          let* _ = forget_recursion in
          let* _ = wf g in
          set st
        in
        let* _ = map validate_branch conts in
        return ()
end

let wf g =
  let initial g =
    {delta= []; epsilon= []; psi= Global.participants g; phi= []; labels= []}
  in
  let _, res = WellFormed.run (Monadic.wf g) (initial g) in
  match res with Ok _ -> Ok g | Error err -> Error err
