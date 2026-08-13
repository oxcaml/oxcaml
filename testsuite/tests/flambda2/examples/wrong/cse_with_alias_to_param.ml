(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Wrong result: the last allocation of [Foo v] is not simplified by CSE.
   This works with `-O2` though (it only needs a proper join). *)

type 'a foo = Foo of 'a

let[@inline never] id lam = lam

let foo v opt =
  let x = match opt with None -> Foo v | Some _ -> id (Foo v) in
  Foo v, x
