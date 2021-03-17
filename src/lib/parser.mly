%token EOI

%token TOKEN PLACE TRANSITION ARROW MARKING

%token SQLEFT SQRIGHT PERIOD LPARENS RPARENS

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

%}

%start <expr list> petri_net

%%

let petri_net := ess = exprs+ ; EOI ; { List.concat ess }

let exprs :=
  | TOKEN ; tks = token+ ; PERIOD ; { List.map (fun x -> Token x) tks }
  | TRANSITION ; trs = transition+ ; PERIOD ; { List.map (fun (x, y) -> Transition (x, y)) trs }
  | PLACE ; pcs = place+ ; PERIOD ; < >
  | MARKING ; nm = name ; mks = entity_with_tokens+ ; PERIOD ; { [ Marking (nm,  mks) ] }
  | arcs = arc ; PERIOD ; < >

let transition :=
 | nm = name ; { (nm, Labelled) }
 | LPARENS ; nm = name ; RPARENS ; { (nm, Silent) }

let entity_with_tokens :=
  | nm = name ; SQLEFT ; tks = token* ; SQRIGHT ; < >

let place :=
  | nm = name ; SQLEFT ; tks = token* ; SQRIGHT ; { Place (nm, tks) }

let arc_continuation :=
  ARROW ; nm = name ; SQLEFT ; tks = token* ; SQRIGHT ; arcs = arc_continuation? ;  { (nm, tks) :: rm_option_arc_cont arcs }
let arc :=
  | nm = name ; ac = arc_continuation ;  { build_arcs nm ac }

let token :=
  | nm = name ; < >

let name :=
  | nm = NAME ; <>
  | nm = QNAME ; <>
