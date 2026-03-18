(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 1: Int32 constant printed without 'l' suffix.
   <[ 42l ]> should print as "42l", not "42". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ 42l ]> in
  Printf.printf "%s\n" s
