(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta -i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Constraining an lpoly instance variable's kind goes through
   [Jkind.intersection] (the unfixed-Tvar path of
   [Ctype.constrain_type_jkind]), not [sub]. Like [sub], the intersection
   must perform its sort equations before reading addressability marks:
   filling the instance copy's sort variable with [bits64] collapses its
   exact-unmarked slot, since [bits64] and [bits64 addressable] are the
   same kind.

   CR rtjoa: both modules should be accepted; they are currently rejected
   because [Layout.intersection] reads marks before equating sorts and
   wrongly finds [x]'s plain form disjoint from [bits64], inconsistently
   with [sub]. *)

module UseScalar (M : sig
  val g : layout_ x. ('a : x). unit -> 'a
end @ static) = struct
  let ok () = (M.g () : ('r : bits64))
end

module UseProduct (M : sig
  val g : layout_ x. ('a : x). unit -> 'a
end @ static) = struct
  let ok () = (M.g () : ('r : bits64 & bits64))
end
