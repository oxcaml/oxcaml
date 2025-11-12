(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

(* CR layouts-scannable: These tests should test out the built-ins once they
   get updated to be appropriately non_pointer *)

(* type declarations *)

type t : value non_pointer
[%%expect{|
type t : value non_pointer
|}]

type t : immutable_data non_pointer
[%%expect{|
type t : immutable_data non_pointer
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any non_pointer, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value maybe_pointer & float64
(* CR zeisbach: this should not be printing as float64 maybe_pointer...! *)
[%%expect{|
type t : value non_pointer & value & float64 maybe_pointer
|}]

(* checking non_pointer annotations, based on
   [typing-layouts-or-null/separability] *)
(* CR layouts-scannable: as separability becomes a scannable axis, move this! *)

(* sub-layout relation *)

type t_maybeptr : any maybe_pointer
type t_nonptr : any non_pointer
[%%expect{|
type t_maybeptr : any
type t_nonptr : any non_pointer
|}]

type ('a : any maybe_pointer) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
[%%expect{|
type ('a : any) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
|}]

type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
[%%expect{|
type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
|}]

type fails = t_maybeptr accepts_nonptr
[%%expect{|
Line 1, characters 13-23:
1 | type fails = t_maybeptr accepts_nonptr
                 ^^^^^^^^^^
Error: This type "t_maybeptr" should be an instance of type
         "('a : any non_pointer)"
       The layout of t_maybeptr is any maybe_pointer
         because of the definition of t_maybeptr at line 1, characters 0-35.
       But the layout of t_maybeptr must be a sublayout of any non_pointer
         because of the definition of accepts_nonptr at line 2, characters 0-42.
|}]
type succeeds = t_nonptr accepts_nonptr
[%%expect{|
type succeeds = t_nonptr accepts_nonptr
|}]

(* CR layouts-separability: as base jkinds become non_pointer, test these!
   including the interesting cases where annotations don't matter *)

(* unboxed records *)

type t_maybeptr_val : value maybe_pointer
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val
type t_nonptr_val : value non_pointer
|}]

type fails : value non_pointer = #{ a : t_maybeptr_val }
[%%expect{|
Line 1, characters 0-56:
1 | type fails : value non_pointer = #{ a : t_maybeptr_val }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "fails" is value maybe_pointer
         because it is an unboxed record.
       But the layout of type "fails" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type fails.
|}]
type succeeds : value non_pointer = #{ a : t_nonptr_val }
[%%expect{|
Line 1, characters 0-57:
1 | type succeeds : value non_pointer = #{ a : t_nonptr_val }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "succeeds" is value maybe_pointer
         because it is an unboxed record.
       But the layout of type "succeeds" must be a sublayout of
           value non_pointer
         because of the annotation on the declaration of the type succeeds.
|}]

type succeeds : value non_pointer & value non_pointer = #{ a : t_nonptr_val; b : t_nonptr_val }
[%%expect{| |}]

(* CR zeisbach: add tests to make sure that the first component of a
[value non_pointer & value] record can be passed to a value non_pointer
   accepting function *)

(* CR zeisbach: module inclusion tests (see line 941 of sep file) *)




let f (a : (_ : any non_pointer)) (b : (_ : any maybe_pointer)) =
  let g (x : (_ : any non_pointer)) = () in
  let _unify_them = [| a; b |] in
  g b
