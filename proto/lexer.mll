{
open Parser
}

let ident = (['a' - 'z'] | ['A' - 'Z'] | '_')*
rule lexer = parse
| [' ' '\t' '\r' '\n'] { lexer lexbuf }
| ("//" [^'\n']*) '\n' { lexer lexbuf }
| ident as s
{

}