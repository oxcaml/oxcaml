(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 3: Nativeint constant printed without 'n' suffix.
   <[ 42n ]> should print as "42n", not "42". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ 42n ]> in
  Printf.printf "%s\n" s
