let trans_out (x, p) =
  Format.printf "%s = %a;" x Pprint.print_expr (Formal2core.trans_program p)

let read_file f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  let p = Parser.program Lexer.lexer lexbuf in
  let f2 = Formal1to2.trans_program p in
  List.iter trans_out f2;
  close_in ic

let _ =
  Arg.parse [] read_file ""
