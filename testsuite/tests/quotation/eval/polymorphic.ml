(* TEST
  include camlinternaleval;
  flags = "-extension runtime_metaprogramming";
  arch_amd64;
  native;
*)

#syntax quotations on
open Camlinternaleval

let eval1 (type a) = (eval : a expr -> a eval)
let eval2 : 'a. 'a expr -> 'a eval = (eval : a expr -> a eval)
