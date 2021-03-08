%token EOI

%token TOKEN PLACE TRANSITION ARROW

%token SQLEFT SQRIGHT PERIOD COLON LPARENS RPARENS

%token <string>NAME
%token <string>QNAME

%{
    open Syntax

let rm_option_arc_cont = function
| Some x ->  x
| None -> []

let rec build_arcs nm arc_conts =
match arc_conts with
| (nm', tks)::rest -> Arc (nm, nm', tks) :: build_arcs nm' rest
| [] -> []

let token_with_optional_sort nm = function
| Some sort -> Token (nm, sort)
| None -> Token (nm, nm) (* the sort has the same name as the token *)

%}

%start <expr list> petri_net

%%

let petri_net := ess = exprs+ ; EOI ; { List.concat ess }

let exprs :=
  | TOKEN ; COLON ; sort = name ; nms = name+ ; PERIOD ; { List.map (fun x -> Token (x, sort)) nms }
  | TOKEN ; tks = token_with_sort+ ; PERIOD ; < >
  | TRANSITION ; trs = transition+ ; PERIOD ; { List.map (fun (x, y) -> Transition (x, y)) trs }
  | PLACE ; pcs = place+ ; PERIOD ; < >
  | arcs = arc ; PERIOD ; < >

let transition :=
 | nm = name ; { (nm, Labelled) }
 | LPARENS ; nm = name ; RPARENS ; { (nm, Silent) }

let place :=
  | nm = name ; SQLEFT ; tks = token* ; SQRIGHT ; { Place (nm, tks) }

let arc_continuation :=
  ARROW ; nm = name ; SQLEFT ; tks = token* ; SQRIGHT ; arcs = arc_continuation? ;  { (nm, tks) :: rm_option_arc_cont arcs }
let arc :=
  | nm = name ; ac = arc_continuation ;  { build_arcs nm ac }

let token :=
  | nm = name ; < >

let token_with_sort :=
  | nm = name ; s = sort_decl? ; { token_with_optional_sort nm s }

let sort_decl := COLON ; sort = name ; < >

let name :=
  | nm = NAME ; <>
  | nm = QNAME ; <>
