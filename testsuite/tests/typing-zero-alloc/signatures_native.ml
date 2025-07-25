(* TEST
   native;
*)

(* This test is just checking that zero_alloc attributes in signatures don't
   result in warning 199 when compiling to native code (which was a bug in the
   initial implementation. *)

module type S = sig
  type t

  val f : int -> int [@@zero_alloc]

  val g : t [@@zero_alloc arity 3]
end
