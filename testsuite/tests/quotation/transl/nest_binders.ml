(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

(* Make sure nesting binder-generated closures behaves *)

let _ = <[let a = 1 in a + 1]>

let _ = <[let a = 1 in let b = 2 in a + b + 1]>

let _ = <[let a = 1 in let b = 2 in let c = 3 in a + b + c + 1]>
