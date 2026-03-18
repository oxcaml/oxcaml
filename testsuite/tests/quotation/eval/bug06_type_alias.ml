(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 6: TypeAlias printed without tick on type variable.
   "int as 'a" should print with the tick, not "int as a". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ fun (x : int as 'a) -> (x : 'a) ]> in
  Printf.printf "%s\n" s
