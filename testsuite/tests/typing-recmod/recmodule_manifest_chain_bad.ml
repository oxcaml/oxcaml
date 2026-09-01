(* TEST
 flags = " -w -a ";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A two-member [module rec] group whose manifest-type chain crosses the module
   boundary twice, so proving the ascription needs three unroll generations
   while the group has only two members. Rejected: the unroll depth is the
   member count (2), one short of the required 3. The companion "ok" test adds
   an unused third member to raise the count to 3. *)

module rec A : sig type anchor type a1 = B.b1 end =
  struct type anchor = Leaf type a1 = B.b1 end
and B : sig type b1 = A.anchor type checked = A.anchor end =
  struct type b1 = A.anchor type checked = A.a1 end
