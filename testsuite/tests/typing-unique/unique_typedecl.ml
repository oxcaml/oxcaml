(* TEST
 expect;
*)

(* This file tests how unique and once are interpreted in signatures
   especially when currying is involved *)

(* When a [unique] argument appears in a function type with multiple arguments,
return modes are implicitly once until the final argument. *)
type equ_fn = unit
constraint
'a -> 'b @ unique -> 'c -> 'd -> 'e
= 'a -> 'b @ unique -> ('c -> ('d -> 'e) @ once) @ once
[%%expect{|
type equ_fn = unit
|}]

(* similar for once *)
type equ_fn = unit
constraint
'a -> 'b @ once -> 'c -> 'd -> 'e
= 'a -> 'b @ once -> ('c -> ('d -> 'e) @ once) @ once
[%%expect{|
type equ_fn = unit
|}]

(* uniqueness of closures are by default aliased,
   regardless of anything; unique would be better
   except for some backward compatibility issues *)
type equ_fn = unit
constraint
'a -> 'b @ unique -> 'c -> 'd -> 'e
= 'a -> 'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once
[%%expect{|
Lines 3-4, characters 0-69:
3 | 'a -> 'b @ unique -> 'c -> 'd -> 'e
4 | = 'a -> 'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once
Error: The type constraints are not consistent.
       Type "'a -> 'b @ unique -> 'c -> 'd -> 'e" is not compatible with type
         "'a -> 'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once"
       Type "'b @ unique -> 'c -> 'd -> 'e" is not compatible with type
         "'b @ unique -> ('c -> ('d -> 'e) @ unique once) @ unique once"
|}]

type distinct_sarg = unit constraint int @ unique -> int = int -> int
[%%expect{|
Line 1, characters 37-69:
1 | type distinct_sarg = unit constraint int @ unique -> int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ unique -> int" is not compatible with type "int -> int"
|}]
type distinct_sret = unit constraint int -> int @ unique = int -> int
[%%expect{|
Line 1, characters 37-69:
1 | type distinct_sret = unit constraint int -> int @ unique = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int -> int @ unique" is not compatible with type "int -> int"
|}]
type distinct_sarg_sret = unit constraint int @ unique -> int = int @ unique -> int @ unique
[%%expect{|
Line 1, characters 42-92:
1 | type distinct_sarg_sret = unit constraint int @ unique -> int = int @ unique -> int @ unique
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ unique -> int" is not compatible with type
         "int @ unique -> int @ unique"
|}]
