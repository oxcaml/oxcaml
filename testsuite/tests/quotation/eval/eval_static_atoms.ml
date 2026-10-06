(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* Zero-sized statics of JIT-compiled units are bound to the runtime's
   permanent atoms rather than emitted as per-unit blocks. Two eval'd
   units' empty arrays are therefore the same block. *)

let () =
  let a : int array = Eval.eval <[ ([||] : int array) ]> in
  let b : int array = Eval.eval <[ ([||] : int array) ]> in
  Printf.printf "lengths: %d %d\n" (Array.length a) (Array.length b);
  Printf.printf "shared: %b\n" (a == b)
