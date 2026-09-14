(* TEST
 flags = " -w -a ";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The same group as the companion "bad" test plus an unused third member, which
   raises the unroll depth (the member count) to 3 and makes it type-check. *)

module rec A : sig type anchor type a1 = B.b1 end =
  struct type anchor = Leaf type a1 = B.b1 end
and B : sig type b1 = A.anchor type checked = A.anchor end =
  struct type b1 = A.anchor type checked = A.a1 end
and Unused : sig end = struct end
