(* TEST
 expect;
*)

type t = int or_null

module type S = sig
  type t : any mod separable
end

[%%expect{|
type t = int or_null
module type S = sig type t : any mod separable end
|}]


(* CR separability: this should type-check. *)

module type S' = S with type t = t

[%%expect{|
Line 1, characters 17-34:
1 | module type S' = S with type t = t
                     ^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = t
       is not included in
         type t : any mod separable
       The kind of the first is value_or_null mod everything
         because it is the primitive type or_null.
       But the kind of the first must be a subkind of any mod separable
         because of the definition of t at line 4, characters 2-28.
|}]
