{
  open Parser

  let dolkw = ["param", PARAM; "opaque", OPAQUE; "type", TYPE]
    |> Utils.smap_of_list

  let kw =
    ["do", DO; "end", END; "defmodule", DEFMODULE; "def", DEF;
     "callback", CALLBACK; "defmoduletype", DEFMODULETYPE]
    |> Utils.smap_of_list
}

let ident = (['a' - 'z'] | ['A' - 'Z'] | '_')*
rule lexer = parse
  | [' ' '\t' '\r' '\n'] { lexer lexbuf }
  | ("//" [^'\n']*) '\n' { lexer lexbuf }
  | '$' ident as s { Utils.SMap.find s dolkw }
  | ident as s
    { match Utils.SMap.find_opt s kw with
      | None -> IDENT s
      | Some t -> t }
  | "@behaviour" { BEHAVIOUR }
  | '=' { EQ }
  | '(' { LPAR }
  | ')' { RPAR }
  | ':' { DCOL }
  | ',' { COMMA }
  | '.' {  DOT }
  | "->" { ARR }
  | _ as c { Printf.eprintf "Unknown character : %c" c; exit 1 }

{

}