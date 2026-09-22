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
|}, Principal{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = (x : _ @ nonportable)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : ('a : value mod portable). 'a -> 'a @ portable end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : ('a : value mod portable). 'a -> 'a @ portable
       The type "'a -> 'a" is not compatible with the type "'a -> 'a @ portable"
       The return mode was expected to be "portable" but is "nonportable"
|}]
