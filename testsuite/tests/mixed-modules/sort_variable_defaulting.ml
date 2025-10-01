(* TEST
 reference = "${test_source_directory}/sort_variable_defaulting.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

module M = struct
  let x = assert false
end

module type S = sig
  val x : float#
end

module N : sig end = M (* This should not default sort variables... *)
module K : S = M       (* ...so this is not an error *)
