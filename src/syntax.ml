
type name = string

type token = TokenMult of name * int

type expr
  = Token of name
  | Place of name * token list
  | Transition of name
  | Arc of name * name * token list
