(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  expect.opt;
*)

#syntax quotations on

let _ = Eval.eval <[ List.append [1; 2] [3] ]>
[%%expect{|
- : int list = [1; 2; 3]
|}]

let _ = Eval.eval <[ List.append [1; 2] [3] ]>
[%%expect{|
Line 1:
Error: Error during linking (exit code 1)
|}]

let _ = Eval.eval <[ List.append [1; 2] [3] ]>
[%%expect{|
Line 1:
Error: Error during linking (exit code 1)
|}]
