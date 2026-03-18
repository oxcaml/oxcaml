(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 10: Src_pos prints as "." instead of something meaningful.
   An expression containing [%src_pos] should not print as ".". *)

#syntax quotations on

let () =
  let s = Quote.string_of_expr <[ [%src_pos] ]> in
  Printf.printf "%s\n" s
