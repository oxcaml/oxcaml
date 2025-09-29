(* TEST
 expect;
*)

(* value_or_null is not a sublayout of value (module inclusion) *)
module M1 (X : sig type t : value_or_null end) : sig type t : value end = X

[%%expect{|
Line 1, characters 74-75:
1 | module M1 (X : sig type t : value_or_null end) : sig type t : value end = X
                                                                              ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t end
       Type declarations do not match: type t = X.t is not included in type t
       The kind of the first is value_or_null
         because of the definition of t at line 1, characters 19-41.
       But the kind of the first must be a subkind of value
         because of the definition of t at line 1, characters 53-67.
|}]

(* any is not a sublayout of any mod separable (module inclusion) *)
module M2 (X : sig type t : any end) : sig type t : any mod separable end = X

[%%expect{|
Line 1, characters 76-77:
1 | module M2 (X : sig type t : any end) : sig type t : any mod separable end = X
                                                                                ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : any mod separable end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : any mod separable
       The kind of the first is any
         because of the definition of t at line 1, characters 19-31.
       But the kind of the first must be a subkind of any mod separable
         because of the definition of t at line 1, characters 43-69.
|}]

