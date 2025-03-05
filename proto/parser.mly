%{
  open Core
  open Formal1

  let atoms = ref Utils.SSet.empty
  

  let add_atom x =
    if not Utils.SSet.(mem x !atoms) then begin
      Format.printf "type atom_type_%s = primitive \"atom_type_%s\";
atom_expr_%s = primitive \"atom_expr_%s\" ();" x x x x;
      atoms := Utils.SSet.add x !atoms
    end
%}

%token PARAM TYPE OPAQUE BEHAVIOUR CALLBACK DEFMODTYPE DEFMODULE DEF DO END
%token <string> IDENT
%token <string> ATOM
%token EQ DCOL LPAR RPAR LCUR RCUR SCOL COMMA DOT ARR PREC EOF
%right ARR

%start program
%type <(string * Formal1.program) list> program

%%

type_decl: x = IDENT DCOL t = typ { x, t }

tower_of_ident:
  | x = IDENT { Var x }
  | e = tower_of_ident DOT x = IDENT { Dot (e, x) }
;

typ:
  | e = tower_of_ident { Expr e }
  | x = IDENT LPAR l = separated_list(COMMA, expr) RPAR 
    { List.fold_left (fun f x -> App (f, x)) (Var x) l }
  | LPAR t = typ RPAR { t }
  | t1 = typ ARR t2 = typ { FTy ("_", t1, t2, I) }
  | a = ATOM { add_atom a; TAtom a }
  | PERC LCUR l = separated_list(SCOL, type_decl) RCUR { Sig l }
;

expr:
  | e = expr_non_atom { e }
  | a = ATOM { add_atom a; EAtom a }
;

expr_assign: x = IDENT EQ e = expr { x, e } ;

expr_non_atom:
  | x = IDENT { Var x }
  | LPAR e = expr RPAR { e }
  | e = expr DOT x = IDENT { Dot (e, x) }
  | f = expr LPAR l = separated_list(COMMA, expr) RPAR
    { List.fold_left (fun f x -> App (f, x)) f l }
  | PERC LCUR l = separated_list(SCOL, expr_assign) RCUR { Struct l }
;

moduletype_decl:
  | PARAM x = IDENT { BParam x }
  | TYPE x = IDENT l = IDENT* EQ t = typ { BType (x, l, t) }
  | OPAQUE x = IDENT l = IDENT* { BOpaque (x, l) }
  | CALLBACK x = IDENT DCOL t = typ { BCallback (x, t) }
;

moduletype: DEFMODTYPE b = IDENT DO l = moduletype_decl* END { b, B l } ;

arg: x = IDENT DCOL t = typ { x, t };

module_decl:
  | PARAM x = IDENT { MParam x }
  | PARAM x = IDENT EQ t = typ { MParamE (x, t) }
  | TYPE x = IDENT l = IDENT* EQ t = typ { MType (x, l, t) }
  | BEHAVIOUR x = IDENT { MBehaviour x }
  | DEF f = IDENT LPAR x = separated_list(COMMA, arg) RPAR DCOL t = typ EQ
      e = expr { MDef (f, x, t, e) }
;

modul: DEFMODULE m = IDENT DO l = module_decl* END { m, M l }
;

top_level:
  | m = modul { m }
  | m = moduletype { m }
;

program:
  l = top_level* EOF { l }
;

%%
