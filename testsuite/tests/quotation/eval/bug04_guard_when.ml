(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 4: Guard keyword printed as "with" instead of "when".
   Match guards should use "when", not "with". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ fun x -> match x with y when y > 0 -> y | _ -> 0 ]> in
  Printf.printf "%s\n" s
