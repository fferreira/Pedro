%token EOI

%token TOKEN PLACE TRANSITION ARROW

%token SQLEFT SQRIGHT EXP PERIOD COLON

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

let token_with_optional_sort nm = function
| Some sort -> Token (nm, sort)
| None -> Token (nm, nm) (* the sort has the same name as the token *)

%}

%start <expr list> petri_net

%%

let petri_net := ess = exprs+ ; EOI ; { List.concat ess }

let exprs :=
  | TOKEN ; COLON ; sort = NAME ; nms = NAME+ ; PERIOD ; { List.map (fun x -> Token (x, sort)) nms }
  | TOKEN ; tks = token_with_sort+ ; PERIOD ; < >
  | TRANSITION ; nms = NAME+ ; PERIOD ; { List.map (fun x -> Transition x) nms }
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

let token_with_sort :=
  | nm = NAME ; s = sort_decl? ; { token_with_optional_sort nm s }

let sort_decl := COLON ; sort = NAME ; < >
