(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

let[@inline always] inline a b =
  let rint () = (Random.int[@inlined never]) 10 in
  let r = rint () in
  let s = rint () in
  let rec f x = if a then r + x else 0
  and g x y = if b then s + (x * y) else 0
  and h x y = (if a then f (x + y) else 0) + if b then g x y else 0 in
  h

(* let foo = inline true true *)
let bar = inline true false

let foobar = inline false false
