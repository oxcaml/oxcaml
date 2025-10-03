(* TEST
   expect;
*)

module M = struct
  let x = assert false
end

module type S = sig
  val x : float#
end

module N : sig end = M (* This should not default sort variables... *)
module K : S = M       (* ...so this is not an error *)

[%%expect{|
Exception: Assert_failure ("", 2, 10).
|}]
