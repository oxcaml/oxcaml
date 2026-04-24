(* TEST
 expect;
*)

(* Kind annotations on existential type variables in patterns must not
   narrow the existential's declared kind. *)

type packed = Packed : ('a : value_or_null). 'a -> packed

[%%expect{|
type packed = Packed : 'a -> packed
|}]

(* Matching the declared kind exactly is fine. *)

let h (Packed (type (a : value_or_null)) (x : a)) = ()

[%%expect{|
val h : packed -> unit = <fun>
|}]

(* Cannot falsely lower [value_or_null] to [value] *)

type packed = P : ('a : value_or_null). 'a -> packed

let f (P (type (a : value)) (x : a)) = ()
[%%expect{|
type packed = P : 'a -> packed
Line 3, characters 20-25:
3 | let f (P (type (a : value)) (x : a)) = ()
                        ^^^^^
Error: The layout of a is value maybe_separable maybe_null
         because of the definition of packed at line 1, characters 0-52.
       But the layout of a must be a sublayout of value
         because of the annotation on the existential variable a.
|}]

(* Omitting the kind annotation defaults to [value]. On a [value_or_null]
   constructor that is also unsound, and is rejected the same way. *)

let k (Packed (type a) (x : a)) = ()

[%%expect{|
Line 1, characters 20-21:
1 | let k (Packed (type a) (x : a)) = ()
                        ^
Error: The layout of a is value maybe_separable maybe_null
         because of the definition of packed at line 1, characters 0-57.
       But the layout of a must be a sublayout of value
         because it's an unannotated existential type variable.
|}]

(* Omitting the annotation is fine when the declared kind is [value]. *)

type packed_val = Packed_val : 'a. 'a -> packed_val

let m (Packed_val (type a) (x : a)) = ()

[%%expect{|
type packed_val = Packed_val : 'a -> packed_val
val m : packed_val -> unit = <fun>
|}]

(* Cannot falsely lower [value] to [value non_pointer] *)

type packed = P : ('a : value). 'a -> packed

let f (P (type (a : value non_pointer)) (x : a)) = ()
[%%expect{|
type packed = P : 'a -> packed
Line 3, characters 20-37:
3 | let f (P (type (a : value non_pointer)) (x : a)) = ()
                        ^^^^^^^^^^^^^^^^^
Error: The layout of a is value
         because of the definition of packed at line 1, characters 0-44.
       But the layout of a must be a sublayout of value non_pointer
         because of the annotation on the existential variable a.
       Note: The layout of immediate is value non_pointer.
|}]

(* Cannot falsely lower along mode crossing:
   [value] to [value mod portable] *)

type packed = P : ('a : value). 'a -> packed

let f (P (type (a : value mod portable)) (x : a)) = ()
[%%expect{|
type packed = P : 'a -> packed
Line 3, characters 20-38:
3 | let f (P (type (a : value mod portable)) (x : a)) = ()
                        ^^^^^^^^^^^^^^^^^^
Error: The kind of a is value
         because of the definition of packed at line 1, characters 0-44.
       But the kind of a must be a subkind of value mod portable
         because of the annotation on the existential variable a.
|}]

(* Cannot falsely lower along a non-modal axis:
   [value] to [value mod external_] *)

type packed = P : ('a : value). 'a -> packed

let f (P (type (a : value mod external_)) (x : a)) = ()
[%%expect{|
type packed = P : 'a -> packed
Line 3, characters 20-39:
3 | let f (P (type (a : value mod external_)) (x : a)) = ()
                        ^^^^^^^^^^^^^^^^^^^
Error: The kind of a is value
         because of the definition of packed at line 1, characters 0-44.
       But the kind of a must be a subkind of value mod external_
         because of the annotation on the existential variable a.
|}]

(* Cannot falsely lower [any] to [float64] *)

type ('a : any) with_phantom
type with_phantom_packed =
  | P : ('a : any). 'a with_phantom -> with_phantom_packed

let f (P (type (a : float64)) (x : a with_phantom)) = ()
[%%expect{|
type ('a : any) with_phantom
type with_phantom_packed =
    P : ('a : any). 'a with_phantom -> with_phantom_packed
Line 5, characters 20-27:
5 | let f (P (type (a : float64)) (x : a with_phantom)) = ()
                        ^^^^^^^
Error: The layout of a is any
         because of the definition of with_phantom_packed at lines 2-3, characters 0-58.
       But the layout of a must be a sublayout of float64
         because of the annotation on the existential variable a.
|}]

(* Reverse direction *)

type packed = P : ('a : value). 'a -> packed

let f (P (type (a : value_or_null)) (x : a)) = ()
[%%expect{|
type packed = P : 'a -> packed
Line 3, characters 41-42:
3 | let f (P (type (a : value_or_null)) (x : a)) = ()
                                             ^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type "('a : value)"
       The layout of a is value maybe_separable maybe_null
         because of the annotation on the existential variable a.
       But the layout of a must be a sublayout of value
         because of the definition of packed at line 1, characters 0-44.
|}]
