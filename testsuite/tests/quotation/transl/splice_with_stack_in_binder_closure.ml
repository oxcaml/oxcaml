(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

(* The binders translate into closures, which mention stack-allocated [x]
   inside the splice below. They must not heap-allocate the closures. *)

let _ =
  let x = stack_ Some 42 in
  <[ let a = 1 in let b = 2 in
     $(match x with
       | Some x -> <[ a + b * $(Quote.Expr.int x) ]>
       | None -> <[ a / b ]>) ]>
