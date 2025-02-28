%{
  open Syntax
  open Core
  open Formal1
%}

%token PARAM TYPE OPAQUE BEHAVIOUR CALLBACK DEFMODULETYPE DEFMODULE DEF DO END
%token <string> IDENT
%token EQ DCOL LPAR RPAR COMMA DOT ARR
%right ARR

%start program

%%

typ:
  | e = expr { Expr e }
  | t1 = typ ARR t2 = typ { FTy ("", t1, t2, I) }
;

expr:
  | LPAR e = expr RPAR { e }
  | e = expr DOT x = IDENT { Dot (e, x) }
  | f = expr LPAR l = separated_list(COMMA, expr) RPAR
    { List.fold_left (fun f x -> App (f, x)) f l }
;

moduletype_decl:
  | PARAM x = IDENT { BParam x }
  | TYPE x = IDENT l = IDENT* EQ t = typ { BType (x, l, t) }
  | OPAQUE x = IDENT l = IDENT* { BOpaque (x, l) }
  | CALLBACK x = IDENT DCOL t = typ { BCallback (x, t) }
;

moduletype: DEFMODULETYPE b = IDENT DO l = moduletype_decl* END { b, B l } ;

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

top_level: m = modul { m } | m = moduletype { m }
  
program: l = top_level* { l }

%%
