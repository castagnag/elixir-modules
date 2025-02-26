type purity = I | P

type t
  = Expr of e | Sig of d list | FTy of string * t * t * purity | Type
  | Equ of e | Inter of t * t
and d = string * t
and e
  = Var of string | Cst of string | If of e * e * e | Struct of n list
  | Dot of e * string | Fun of string * t * e * purity | App of e * e
  | Rei of t | Seal of e * t
and n = string * e
