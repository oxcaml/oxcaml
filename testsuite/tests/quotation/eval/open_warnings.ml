(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  native;
*)

#syntax quotations on

(* As of 2026-05-19, we silence all warnings.
   This test ensures that, should we ever propagate warnings from compiling
   the generated program, then local opens don't cause any.
   Any legitimate warnings will be reported when compiling the metaprogram. *)

let () =
  print_string "shadowing open: ";
  Eval.eval <[
    let tl x = x in ignore tl;
    let open List in [0; 42; 1] |> tl |> hd
  ]> |> print_int;
  print_newline ()
;;

let () =
  print_string "unused open: ";
  Eval.eval <[
    let my_hd = function x :: _ -> x | _ -> 0 in
    let open Option in [42; 0; 1] |> my_hd
  ]> |> print_int;
  print_newline ()
;;
