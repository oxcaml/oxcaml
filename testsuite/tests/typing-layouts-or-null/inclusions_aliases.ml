(* TEST
 expect;
*)

(* Alias-based inclusion checks to cross-validate module-parameter cases. *)

(* value_or_null is not a sublayout of value *)
type t_vn : value_or_null
type q_v : value = t_vn

[%%expect{|
type t_vn : value_or_null
Line 2, characters 0-23:
2 | type q_v : value = t_vn
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_vn" is value_or_null
         because of the definition of t_vn at line 1, characters 0-25.
       But the kind of type "t_vn" must be a subkind of value
         because of the definition of q_v at line 2, characters 0-23.
|}]

(* any is not a sublayout of any mod separable *)
type t_any : any
type q_any_sep : any mod separable = t_any

[%%expect{|
type t_any : any
Line 2, characters 0-42:
2 | type q_any_sep : any mod separable = t_any
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_any" is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of type "t_any" must be a subkind of any mod separable
         because of the definition of q_any_sep at line 2, characters 0-42.
|}]

(* any mod separable is a sublayout of any *)
type t_sep : any mod separable
type q_any2 : any = t_sep

[%%expect{|
type t_sep : any mod separable
type q_any2 = t_sep
|}]

(* value is a sublayout of value_or_null *)
type t_val : value
type q_vn2 : value_or_null = t_val

[%%expect{|
type t_val
type q_vn2 = t_val
|}]

(* value_or_null is not a sublayout of any mod separable *)
type t_vn2 : value_or_null
type q_sep2 : any mod separable = t_vn2

[%%expect{|
type t_vn2 : value_or_null
Line 2, characters 0-39:
2 | type q_sep2 : any mod separable = t_vn2
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t_vn2" is value_or_null
         because of the definition of t_vn2 at line 1, characters 0-26.
       But the kind of type "t_vn2" must be a subkind of any mod separable
         because of the definition of q_sep2 at line 2, characters 0-39.
|}]

(* bits64 is a sublayout of any mod separable *)
type t_b64 : bits64
type q_sep3 : any mod separable = t_b64

[%%expect{|
type t_b64 : bits64
type q_sep3 = t_b64
|}]
