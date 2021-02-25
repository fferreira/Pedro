%token EOI

%token TOKEN PLACE TRANSITION ARROW

%token SQLEFT SQRIGHT EXP PERIOD

%token <int>MULTIPLICITY
%token <string>NAME

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
  | TOKEN ; nms = NAME+ ; PERIOD ; { List.map (fun x -> Token x) nms }
  | TRANSITION ; nms = NAME+ ; PERIOD ; { List.map (fun x -> Token x) nms }
  | PLACE ; pcs = place+ ; PERIOD ; < >
  | arcs = arc ; PERIOD ; < >

let place :=
  | nm = NAME ; SQLEFT ; tks = token* ; SQRIGHT ; { Place (nm, tks) }

let arc_continuation :=
  ARROW ; nm = NAME ; SQLEFT ; tks = token* ; SQRIGHT ; arcs = arc_continuation? ;  { (nm, tks) :: rm_option_arc_cont arcs }
let arc :=
  | nm = NAME ; ac = arc_continuation ;  { build_arcs nm ac }

let token :=
  | nm = NAME ; EXP ; n = MULTIPLICITY ; < TokenMult >
  | nm = NAME ; { TokenMult (nm, 1) }
