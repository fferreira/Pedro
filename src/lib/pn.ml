open Graph
open Syntax

(* type name = string
 *
 * type token = Token of name * int (\* a token has a name and a multiplicity *\) *)

type node = Place of name * name list | Transition of name * vis

(* if the label is to a transition it is the tokens it needs to trigger or if
   it is from a transition it is the tokens it provides *)
(* type label = token list *)

module Node = struct
  type t = node

  let equal = ( = )

  let hash = Hashtbl.hash

  let compare = compare
end

module Label = struct
  type t = entity_marking

  let default = []

  (* This comparison is broken because the lists are not ordered *)
  let compare l1 l2 = if List.for_all2 ( = ) l1 l2 then 0 else 1
end

module PPN = Persistent.Digraph.ConcreteLabeled (Node) (Label)

(* module PN = Imperative.Digraph.AbstractLabeled(Node)(Label) *)

(* markings *)
module PNMark = Map.Make (String)

(* relates the node mark (which is an name/string) with the mark *)

let _ = Parsing.peek_val (* just for the compilation of this file *)

(* Generating graphs from the nets produced by pedro expressions *)

let generate_ppn (n : net) : PPN.t =
  let start = PPN.empty in
  let rec add (arcs : (name * name * dir * entity_marking) list) g =
    match arcs with
    | (src, dst, dir, tks) :: rest -> (
      match dir with
      | PlaceToTransition ->
          let vis = List.assoc dst n.transitions in
          (* this should not fail for well formed nets *)
          let srcm = List.assoc src n.places in
          add rest
            (PPN.add_edge_e g
               (Place (src, srcm), tks, Transition (dst, vis)) )
      | TransitionToPlace ->
          let vis = List.assoc src n.transitions in
          (* this should not fail for well formed nets *)
          let dstm = List.assoc dst n.places in
          add rest
            (PPN.add_edge_e g
               (Transition (src, vis), tks, Place (dst, dstm)) ) )
    | [] -> g
  in
  add n.arcs start

module Display = struct
  include PPN

  let vertex_name = function
    | Place (nm, _) -> "\"" ^ nm ^ "\""
    | Transition (nm, Labelled) -> "\"" ^ nm ^ "\""
    | Transition (nm, Silent) -> "\"(" ^ nm ^ ")\""

  let graph_attributes _ = [`Rankdir `LeftToRight]

  let default_vertex_attributes _ = []

  let vertex_attributes = function
    | Place (_, []) -> [`Shape `Circle; `Label ("")]
    | Place (_, tks) ->
        [`Shape `Circle; `Label (String.concat " " tks)]
    (* | Place (nm, []) -> [`Shape `Circle; `Label ("{" ^ nm ^ "}")]
     * | Place (nm, tks) ->
     *     [`Shape `Circle; `Label ("{" ^ nm ^ "}" ^ String.concat " " tks)] *)
    | Transition (_, Labelled) -> [`Shape `Box]
    | Transition (_, Silent) -> [`Shape `Diamond; `Label ""]

  let default_edge_attributes _ = []

  let edge_attributes ((_, a, _) : PPN.edge) = [`Label (String.concat " " a)]

  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot (Display)

let generate_dot g =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
