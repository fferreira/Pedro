open Graph

type name = string

type token = Token of name * int (* a token has a name and a multiplicity *)

type node = Place of name * token list
          | Transition of name

(* if the label is to a transition it is the tokens it needs to
   trigger or if it is from a transition it is the tokens it provides
   *)
type label = token list

module Node =
  struct
    type t = node
  end

module Label =
  struct
    type t = label

    let default = []

    (* perhaps we want a better comparison
       (labels are name multisets)
     *)
    let compare l1 l2 = if List.for_all2 (=) l1 l2 then 0 else 1
  end

module PersistentPN = Persistent.Digraph.AbstractLabeled(Node)(Label)

module PN = Imperative.Digraph.AbstractLabeled(Node)(Label)


type mark = token list
module PNMark = Map.Make(Int) (* relates the node mark (which is an integer) with the mark *)


let () = print_endline "Vote for Pedro!"
