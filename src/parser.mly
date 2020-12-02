%token EOI

%token TOKEN PLACE TRANSITION ARC

%token SQLEFT SQRIGHT EXP

%token <int>MULTIPLICITY
%token <string>NAME

%{
    open Syntax
%}

%start <expr list> petri_net

%%

let petri_net := ess = exprs+ ; EOI ; { List.concat ess }


let exprs :=
  | TOKEN ; nms = NAME+ ; { List.map (fun x -> Token x) nms }
  | TRANSITION ; nms = NAME+ ; { List.map (fun x -> Token x) nms }
  | PLACE ; pcs = place+ ; < >
  | arcs = arc+ ; < >

let place :=
  | nm = NAME ; SQLEFT ; tks = token* ; SQRIGHT ; { Place (nm, tks) }

let arc :=
  | nm = NAME ; ARC ; NAME ; SQLEFT ; tks = token* ; SQRIGHT ; { Place (nm, tks) }

let token :=
  | nm = NAME ; EXP ; n = MULTIPLICITY ; < TokenMult >
  | nm = NAME ; { TokenMult (nm, 1) }


(*
let compilation_unit := ds = declaration * ; EOI ; < >


let declaration := located(raw_declaration)
let raw_declaration :=
  | NAMESPACE ; nm = IDENT ; ds = declaration*  ; END ; < Namespace >
  | ds = raw_small_declaration ; < >

let small_declaration := located(raw_small_declaration)
let raw_small_declaration :=
  | DATA ; nm = IDENT ; pars = data_params  ; WHERE
  ; cs = data_constructor+ ; < DataType >

  | DEF ; nm = IDENT ;
    tp = option(COLON ; f = full_type ; < > ) ;
    ASSIGN ; e = expr ; < Definition >

  | DEF ; nm = IDENT ;
    tp = option(COLON ; f = full_type ; < > ) ;
    WHERE ; bs = branch+ ; < PatternMatch >
  | HAVING ; qnm = qident ; IN ; ds = small_declaration* ; END ; < LocalOpenNamespace >
  | OPEN ; qnm = qident ; < OpenNamespace >


let data_params :=
  { [] }
| LANGLE ; nms = separated_nonempty_list(COMMA, TYPE_VAR) ; RANGLE ; { nms }

let def_data_params :=
| { [] }
| LANGLE ; nms = separated_nonempty_list(COMMA, located(simple_type)) ; RANGLE ; { nms }

let branch :=
  MID ; ps = simple_expr+ ; DOUBLE_ARROW ; e = expr ; < >

(* types *)

let data_constructor :=
  MID ; nm = IDENT ; COLON ; tp = full_type ; { (nm, tp) }

let simple_type :=
  | UNIT ; { T_unit }
  | INT ; { T_int }
  | BOOL ; { T_bool }
  | STRING ; { T_string }
  | nm = TYPE_VAR ; < T_var >
  | nms = qident ; pars = def_data_params ; { T_tp (nms, pars) }
  | LPARENS ; tp = raw_full_type ; RPARENS ; < >


let full_type := located(raw_full_type)
let raw_full_type :=
  | t1 = located(simple_type) ; ARROW ; t2 = full_type ; { T_arrow (t1, t2) }
  | t = simple_type ; { t }

(* expressions *)

let expr := located(raw_expr)
let raw_expr :=
  | FN ; vs = var_bound_site+ ; DOUBLE_ARROW ; e = expr ; < Fn >
  | e1 = simple_expr ; e2s = simple_expr+ ; < App >
  | OPEN ; qnm = qident ; IN ; e = expr ; < Open >
  | LET ; nm = IDENT ; EQ ; e1 = expr ; IN ; e2 = expr ; < Let >
  | CASE ; es = simple_expr+ ; OF ; bs = branch+ ; END ; < Case >
  | e = raw_simple_expr ; < >


let simple_expr := located(raw_simple_expr)
let raw_simple_expr :=
  | UNIT_V ; { Unit }
  | s = STRING_V ; < String >
  | b = BOOL_V ; < Bool >
  | n = INT_V ; < Integer >
  | nms = qident ; < Var >
  | LPARENS ; e = raw_expr ; RPARENS ; < >
  | e1 = simple_expr ; o = op ; e2 = simple_expr ; < Op >


%inline op:
| EQ { Eq }
| PLUS { Plus }
| MINUS { Minus}
| TIMES { Times }
| DIV { Div }
| EXP { Exp }
| AND { And }
| OR { Or }

let var_bound_site :=
  | LPARENS ; nm = IDENT ; tp = option(COLON ; f = full_type ; < >) ; RPARENS ; < >
  | nm = IDENT ; { (nm, None) }

let qident :=
  | nms = separated_nonempty_list(DOT, IDENT) ; < >


(* utilities *)
let located(x) ==
  ~ = x; { { loc = build $loc; value = x } }
*)
