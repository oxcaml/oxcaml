(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Tests for the order of checks when typechecking record label accesses,
   observable for records whose representation varies per use site (i.e. with
   fields of kind [any]). *)

type ('a : any) t = { mutable v : 'a }
[%%expect{|
type ('a : any) t = { mutable v : 'a; }
|}]

(* Control: assignment at a representable instantiation is fine. *)
let set_int (r : int t) (x : int) = r.v <- x
[%%expect{|
val set_int : int t -> int -> unit = <fun>
|}]

(* Assignment to a record whose representation is undetermined: the record
   must be representable. *)
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

(* Assignment where the assigned value is ill-typed AND the record's
   representation is undetermined: which error is reported? *)
let set_bad (type a : any) (r : a t) = r.v <- "hello"
[%%expect{|
Line 1, characters 39-53:
1 | let set_bad (type a : any) (r : a t) = r.v <- "hello"
                                           ^^^^^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of a/2 is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a/2 must be representable
         because it's the type of a field being assigned a value.
|}]

(* Atomic fields may be declared in a record whose representation is
   undetermined; [%atomic.loc] computes the record's representation to check
   that it supports atomic access, so it too requires representable fields. *)
type ('a : any) u = { mutable n : int [@atomic]; y : 'a }
[%%expect{|
type ('a : any) u = { mutable n : int [@atomic]; y : 'a; }
|}]

let atomic_loc_bad (type a : any) (r : a u) = [%atomic.loc r.n]
[%%expect{|
Line 1, characters 46-63:
1 | let atomic_loc_bad (type a : any) (r : a u) = [%atomic.loc r.n]
                                                  ^^^^^^^^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a field being assigned a value.
|}]
