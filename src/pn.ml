open Graph
open Syntax

(* type name = string
 *
 * type token = Token of name * int (\* a token has a name and a multiplicity *\) *)

type node = Place of name
          | Transition of name

(* if the label is to a transition it is the tokens it needs to
   trigger or if it is from a transition it is the tokens it provides
   *)
(* type label = token list *)

module Node =
  struct
    type t = node

    let equal = (=)
    let hash = Hashtbl.hash
    let compare = compare
  end

module Label =
  struct
    type t = entity_marking

    let default = []

    (* This comparison is broken because the lists are not ordered *)
    let compare l1 l2 = if List.for_all2 (=) l1 l2 then 0 else 1
  end

module PPN = Persistent.Digraph.ConcreteLabeled(Node)(Label)

(* module PN = Imperative.Digraph.AbstractLabeled(Node)(Label) *)

(* markings *)
module PNMark = Map.Make(String) (* relates the node mark (which is an name/string) with the mark *)

let _ = Parsing.peek_val (* just for the compilation of this file *)


(* Generating graphs from the nets produced by pedro expressions *)

let generate_ppn (n : net) : PPN.t =
  let start = PPN.empty in
  let rec add (arcs : (name * name * dir * entity_marking) list) g =
    match arcs with
    | (src, dst, dir, tks)::rest ->
       begin match dir with
       | PlaceToTransition ->
          add rest (PPN.add_edge_e g (Place src, tks, Transition dst))
       | TransitionToPlace ->
          add rest (PPN.add_edge_e g (Transition src, tks, Place dst))
       end
    | [] -> g
  in
  add n.arcs start

module Display = struct
  include PPN

  let vertex_name = function
  | Place nm -> "\"" ^ nm ^ "\""
  | Transition nm -> "\"" ^ nm ^ "\""

  let graph_attributes _ = [`Rankdir `LeftToRight]

  let default_vertex_attributes _ = []

  let vertex_attributes = function
    | Place _nm -> [`Shape `Circle]
    | Transition _nm -> [`Shape `Box]

  let default_edge_attributes _ = []

  let edge_attributes (_, a, _) =
    let rec render_attr = function
      | (nm, n) :: rest ->
         if n > 1 then
           nm ^ "^" ^ string_of_int n ^ " " ^ render_attr rest
         else
           nm ^ " " ^ render_attr rest
      | [] -> ""
    in
    [`Label (render_attr a)]

  let get_subgraph _ = None
end

module DotOutput = Graphviz.Dot (Display)

let generate_dot g =
  let buffer = Buffer.create 4196 in
  let formatter = Format.formatter_of_buffer buffer in
  DotOutput.fprint_graph formatter g ;
  Format.pp_print_flush formatter () ;
  Buffer.contents buffer
