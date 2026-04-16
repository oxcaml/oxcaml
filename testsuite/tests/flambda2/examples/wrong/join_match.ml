(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Wrong result: the test is not very interesting without a join.
   With -O2 we get a decent result, and match-in-match should improve it
   further. *)

type t =
  | A of int
  | B of int
  | C of int

let[@inline always] g t =
  match t with A x -> x + 1 | B x -> x * 2 | C x -> x * 42

let bar x =
  let t = if x < 0 then A x else B x in
  g t
