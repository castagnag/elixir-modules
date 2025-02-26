open Formal2
open Core
open Utils

let trans_out (x, p) =
  Format.printf "%s = %a;" x Pprint.print_expr (Formal2core.trans_program p)

let _ =
  let pure_stack =
    ["PureStack",
     M { mparam = ["a"]
       ; mbehaviour = SMap.empty
       ; mbody =
           [MDef ("new",
                  ["list_of_element", Expr (App (Cst "list", Var "a"))],
                  Expr (App (Cst "list", Var "a")),
                  Var "list_of_element")]}]
  in
  List.iter trans_out pure_stack
