(* TEST
 expect;
*)

(* Tests for the order of checks when typechecking record label accesses,
   observable for records whose representation varies per use site (i.e. with
   fields of kind [any]). *)

type ('a : any) t = { mutable v : 'a }
[%%expect{|
type ('a : any) t = { mutable v : 'a; }
|}]

(* Assignment is fine when the field is representable *)
let set_int (r : int t) (x : int) = r.v <- x
[%%expect{|
val set_int : int t -> int -> unit = <fun>
|}]

(* Assignment to a record whose representation is undetermined fails, as the
   field must be representable *)
let set (type a : any) (r : a t) = r.v <- assert false
[%%expect{|
Line 1, characters 35-54:
1 | let set (type a : any) (r : a t) = r.v <- assert false
                                       ^^^^^^^^^^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a field being assigned a value.
|}]

(* Ill-typed assignment error still takes precedence over
   undetermined-representation error *)
let set_bad (type a : any) (r : a t) = r.v <- "hello"
[%%expect{|
Line 1, characters 46-53:
1 | let set_bad (type a : any) (r : a t) = r.v <- "hello"
                                                  ^^^^^^^
Error: This constant has type "string" but an expression was expected of type "a"
|}]

(* Atomic fields may be declared in a record whose representation is
   undetermined *)
type ('a : any) u = { mutable n : int [@atomic]; y : 'a }
[%%expect{|
type ('a : any) u = { mutable n : int [@atomic]; y : 'a; }
|}]

(* [%atomic.loc] requires a determined layout *)
let atomic_loc_bad (type a : any) (r : a u) = [%atomic.loc r.n]
[%%expect{|
Line 1, characters 46-63:
1 | let atomic_loc_bad (type a : any) (r : a u) = [%atomic.loc r.n]
                                                  ^^^^^^^^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a field in a record being projected from.
|}]
