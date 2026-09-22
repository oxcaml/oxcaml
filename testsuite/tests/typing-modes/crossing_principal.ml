(* TEST
 expect;
*)

(* CR zeisbach: the interface type below crosses portability via the kind of
   its type variable, so this should be accepted. Under [-principal] it is
   rejected, since [Ctype.is_principal] deems the copied variable is
   non-principal (as it is at [subject_level]) and no crossing is applied. *)
module M : sig
  val f : ('a : value mod portable). 'a -> 'a @ portable
end = struct
  let f x = (x : _ @ nonportable)
end
[%%expect{|
module M : sig val f : ('a : value mod portable). 'a -> 'a @ portable end
|}]
