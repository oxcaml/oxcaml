(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 5: Negative constants not parenthesized in argument positions.
   "Some (-42)" should keep the parens around -42 so it doesn't
   get parsed as "Some" minus "42". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ Some (-42) ]> in
  Printf.printf "%s\n" s
