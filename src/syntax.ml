
type name = string

type tkn = TokenMult of name * int

type expr
  = Token of name
  | Place of name * tkn list
  | Transition of name
  | Arc of name * name * tkn list
