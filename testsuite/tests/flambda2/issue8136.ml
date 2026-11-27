(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -O2 -flambda2-result-types-all-functions -flambda2-join-algorithm=n-way";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
 *)

type r = { a : int }
let[@inline never] f (x : r) = #(x, x, x.a)
let g c (w : r) (z : r) =
  let x1 =
    let #(_, v, _) = if c then f w else #({ a = z.a }, z, 0) in
    v
  in
  x1.a
