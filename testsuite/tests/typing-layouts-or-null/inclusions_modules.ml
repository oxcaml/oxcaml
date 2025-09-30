(* TEST
 flags += "-ikinds";
 expect;
*)

module A1 : sig type t : value end = struct
  type t : value_or_null
end

[%%expect{|
Lines 1-3, characters 37-3:
1 | .....................................struct
2 |   type t : value_or_null
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : value_or_null end
       is not included in
         sig type t end
       Type declarations do not match:
         type t : value_or_null
       is not included in
         type t
       The kind of the first is value_or_null
         because of the definition of t at line 2, characters 2-24.
       But the kind of the first must be a subkind of value
         because of the definition of t at line 1, characters 16-30.
|}]

module A2 : sig type t : any mod separable end = struct
  type t : any
end

[%%expect{|
Lines 1-3, characters 49-3:
1 | .................................................struct
2 |   type t : any
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : any end
       is not included in
         sig type t : any mod separable end
       Type declarations do not match:
         type t : any
       is not included in
         type t : any mod separable
       The kind of the first is any
         because of the definition of t at line 2, characters 2-14.
       But the kind of the first must be a subkind of any mod separable
         because of the definition of t at line 1, characters 16-42.
|}]

(* value_or_null is not a sublayout of value (module inclusion) *)
module M1 (X : sig type t : value_or_null end) : sig type t : value end = X

(* CR jujacobs: this should be rejected. Caching bug! *)
[%%expect{|
module M1 : functor (X : sig type t : value_or_null end) -> sig type t end
|}]

(* any is not a sublayout of any mod separable (module inclusion) *)
module M2 (X : sig type t : any end) : sig type t : any mod separable end = X

(* CR jujacobs: this should be rejected. Caching bug! *)
[%%expect{|
module M2 :
  functor (X : sig type t : any end) -> sig type t : any mod separable end
|}]
