(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

let _ = <[let _first = 20130322 in let _second = 1 in () ]>
