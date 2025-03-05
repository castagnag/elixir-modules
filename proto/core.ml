type purity = I | P

type t
  = Expr of e | Sig of d list | FTy of string * t * t * purity | Type
  | Equ of e | Inter of t * t | TAtom of string
and d = string * t
and e
  = Var of string | Cst of string | If of e * e * e | Struct of n list
  | Dot of e * string | Fun of string * t * e | App of e * e
  | Rei of t | Seal of e * t | EAtom of string
and n = string * e
