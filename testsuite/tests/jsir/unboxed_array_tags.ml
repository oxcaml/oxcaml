(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 jsir;
*)

(* Arrays of unboxed numbers are ordinary blocks of tag 0, as in bytecode,
   whether they come from a literal or from the runtime: structural equality
   and hashing must not tell them apart. *)

external[@layout_poly] make :
  ('a : any mod separable). int -> 'a -> 'a array = "%makearray_dynamic"

let check name literal created =
  let literal = Sys.opaque_identity literal in
  let created = Sys.opaque_identity created in
  Printf.printf "%s: tags %d %d, equal %b, same hash %b\n" name
    (Obj.tag (Obj.repr literal))
    (Obj.tag (Obj.repr created))
    (literal = created)
    (Hashtbl.hash literal = Hashtbl.hash created)

let () =
  check "int64#" [| #1L; #1L; #1L |] (make 3 #1L);
  check "int32#" [| #1l; #1l; #1l |] (make 3 #1l);
  check "nativeint#" [| #1n; #1n; #1n |] (make 3 #1n);
  check "float32#" [| #1.s; #1.s; #1.s |] (make 3 #1.s);
  check "int8#" [| #1s; #1s; #1s |] (make 3 #1s);
  check "int16#" [| #1S; #1S; #1S |] (make 3 #1S);
  check "int#" [| #1m; #1m; #1m |] (make 3 #1m);
  check "float#" [| #1.; #1.; #1. |] (make 3 #1.)

(* Literals that are not constants *)
let () =
  let one = Sys.opaque_identity 1L in
  let one = Stdlib_upstream_compatible.Int64_u.of_int64 one in
  check "int64# (not constant)" [| one; one; one |] (make 3 #1L)
