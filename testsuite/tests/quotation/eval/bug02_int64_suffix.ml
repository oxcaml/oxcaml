(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 2: Int64 constant printed without 'L' suffix.
   <[ 42L ]> should print as "42L", not "42". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ 42L ]> in
  Printf.printf "%s\n" s
