(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 8: VClosed variant type drops "present" tags.
   "[< `A | `B > `A ]" should preserve the "> `A" part. *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr
    <[ fun (x : [< `A of int | `B > `A ]) -> x ]> in
  Printf.printf "%s\n" s
