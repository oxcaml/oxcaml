(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Locally abstract types cannot cross quotation boundaries *)

let variable_eval (type a) (x : <[a]> expr) : a = Eval.eval x
[%%expect {|
Line 1, characters 34-35:
1 | let variable_eval (type a) (x : <[a]> expr) : a = Eval.eval x
                                      ^
Error: Identifier "a" is used at line 1, characters 34-35,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 24-25, outside any quotations.
|}]
