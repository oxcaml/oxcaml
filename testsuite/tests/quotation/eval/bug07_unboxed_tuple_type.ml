(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 7: TypeUnboxedTuple printed without '#' prefix.
   Unboxed tuple types should print with '#' like "#(int * string)". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ fun (x : #(int * string)) -> x ]> in
  Printf.printf "%s\n" s
