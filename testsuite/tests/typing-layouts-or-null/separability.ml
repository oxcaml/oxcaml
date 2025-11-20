(* TEST
 expect;
*)

(* Tests for the separability jkind axis. *)

(* Basic subkinding relation. *)

(* CR layouts v3.4: [mod maybe_separable] doesn't do anything
   and should raise a warning. *)
type t_maybesep : any mod maybe_separable
type t_sep : any mod separable
type t_nonfloat : any mod non_float

[%%expect{|
type t_maybesep : any
type t_sep : any separable
type t_nonfloat : any non_float
|}]

type ('a :  any mod maybe_separable) accepts_maybesep
type ('a : any mod separable) accepts_sep
type ('a : any mod non_float) accepts_nonfloat

[%%expect{|
type ('a : any) accepts_maybesep
type ('a : any separable) accepts_sep
type ('a : any non_float) accepts_nonfloat
|}]

type succeeds = t_maybesep accepts_maybesep
type succeeds = t_sep accepts_maybesep
type succeeds = t_nonfloat accepts_maybesep

[%%expect{|
type succeeds = t_maybesep accepts_maybesep
type succeeds = t_sep accepts_maybesep
type succeeds = t_nonfloat accepts_maybesep
|}]

type fails = t_maybesep accepts_sep
[%%expect{|
Line 1, characters 13-23:
1 | type fails = t_maybesep accepts_sep
                 ^^^^^^^^^^
Error: This type "t_maybesep" should be an instance of type
         "('a : any separable)"
       The layout of t_maybesep is any
         because of the definition of t_maybesep at line 1, characters 0-41.
       But the layout of t_maybesep must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type succeeds = t_sep accepts_sep
type succeeds = t_nonfloat accepts_sep


[%%expect{|
type succeeds = t_sep accepts_sep
type succeeds = t_nonfloat accepts_sep
|}]

type fails = t_maybesep accepts_nonfloat
[%%expect{|
Line 1, characters 13-23:
1 | type fails = t_maybesep accepts_nonfloat
                 ^^^^^^^^^^
Error: This type "t_maybesep" should be an instance of type
         "('a : any non_float)"
       The layout of t_maybesep is any
         because of the definition of t_maybesep at line 1, characters 0-41.
       But the layout of t_maybesep must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type fails = t_sep accepts_nonfloat
[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_sep accepts_nonfloat
                 ^^^^^
Error: This type "t_sep" should be an instance of type "('a : any non_float)"
       The layout of t_sep is any separable
         because of the definition of t_sep at line 2, characters 0-30.
       But the layout of t_sep must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type succeeds = t_nonfloat accepts_nonfloat

[%%expect{|
type succeeds = t_nonfloat accepts_nonfloat
|}]

(* Testing separability for various base jkinds. *)

(* [value_or_null] is maybe-separable: *)
type t_von : value_or_null

type succeeds = t_von accepts_maybesep
[%%expect{|
type t_von : value_or_null
type succeeds = t_von accepts_maybesep
|}]

type fails = t_von accepts_sep

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_von accepts_sep
                 ^^^^^
Error: This type "t_von" should be an instance of type "('a : any separable)"
       The layout of t_von is value
         because of the definition of t_von at line 1, characters 0-26.
       But the layout of t_von must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = t_von accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_von accepts_nonfloat
                 ^^^^^
Error: This type "t_von" should be an instance of type "('a : any non_float)"
       The layout of t_von is value
         because of the definition of t_von at line 1, characters 0-26.
       But the layout of t_von must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [value] is separable *)
type t_val : value

type succeeds = t_val accepts_maybesep
type succeeds = t_val accepts_sep

[%%expect{|
type t_val
type succeeds = t_val accepts_maybesep
type succeeds = t_val accepts_sep
|}]

type fails = t_val accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_val accepts_nonfloat
                 ^^^^^
Error: This type "t_val" should be an instance of type "('a : any non_float)"
       The layout of t_val is value separable
         because of the definition of t_val at line 1, characters 0-18.
       But the layout of t_val must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [any] is maybe-separable *)
type t_any : any

type succeeds = t_any accepts_maybesep

[%%expect{|
type t_any : any
type succeeds = t_any accepts_maybesep
|}]

type fails = t_any accepts_sep

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_any accepts_sep
                 ^^^^^
Error: This type "t_any" should be an instance of type "('a : any separable)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = t_any accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_any accepts_nonfloat
                 ^^^^^
Error: This type "t_any" should be an instance of type "('a : any non_float)"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* [any_separable] is separable *)
type t_ann : any mod separable
type succeeds = t_ann accepts_maybesep
type succeeds = t_ann accepts_sep
[%%expect{|
type t_ann : any separable
type succeeds = t_ann accepts_maybesep
type succeeds = t_ann accepts_sep
|}]

type fails = t_ann accepts_nonfloat

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_ann accepts_nonfloat
                 ^^^^^
Error: This type "t_ann" should be an instance of type "('a : any non_float)"
       The layout of t_ann is any separable
         because of the definition of t_ann at line 1, characters 0-30.
       But the layout of t_ann must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

(* Testing additional non-float jkinds *)

(* [immutable_data] is non-float *)
type t_imm : immutable_data
type succeeds = t_imm accepts_maybesep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat

[%%expect{|
type t_imm : immutable_data
type succeeds = t_imm accepts_maybesep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat
|}]

(* [mutable_data] is non-float *)
type t_mut : mutable_data
type succeeds = t_mut accepts_maybesep
type succeeds = t_mut accepts_sep
type succeeds = t_mut accepts_nonfloat

[%%expect{|
type t_mut : mutable_data
type succeeds = t_mut accepts_maybesep
type succeeds = t_mut accepts_sep
type succeeds = t_mut accepts_nonfloat
|}]

(* [sync_data] is non-float *)
type t_sync : sync_data
type succeeds = t_sync accepts_maybesep
type succeeds = t_sync accepts_sep
type succeeds = t_sync accepts_nonfloat

[%%expect{|
type t_sync : sync_data
type succeeds = t_sync accepts_maybesep
type succeeds = t_sync accepts_sep
type succeeds = t_sync accepts_nonfloat
|}]

(* [immediate] is non-float *)
type t_imm : immediate
type succeeds = t_imm accepts_maybesep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat

[%%expect{|
type t_imm : immediate
type succeeds = t_imm accepts_maybesep
type succeeds = t_imm accepts_sep
type succeeds = t_imm accepts_nonfloat
|}]

(* [immediate64] is non-float *)
type t_imm64 : immediate64
type succeeds = t_imm64 accepts_maybesep
type succeeds = t_imm64 accepts_sep
type succeeds = t_imm64 accepts_nonfloat

[%%expect{|
type t_imm64 : immediate64
type succeeds = t_imm64 accepts_maybesep
type succeeds = t_imm64 accepts_sep
type succeeds = t_imm64 accepts_nonfloat
|}]

(* Testing non-value layouts. *)

(* [bits32] is non-float *)
type t_b32 : bits32
type succeeds = t_b32 accepts_maybesep
type succeeds = t_b32 accepts_sep
type succeeds = t_b32 accepts_nonfloat

[%%expect{|
type t_b32 : bits32
type succeeds = t_b32 accepts_maybesep
type succeeds = t_b32 accepts_sep
type succeeds = t_b32 accepts_nonfloat
|}]

(* [bits64] is non-float *)
type t_b64 : bits64
type succeeds = t_b64 accepts_maybesep
type succeeds = t_b64 accepts_sep
type succeeds = t_b64 accepts_nonfloat

[%%expect{|
type t_b64 : bits64
type succeeds = t_b64 accepts_maybesep
type succeeds = t_b64 accepts_sep
type succeeds = t_b64 accepts_nonfloat
|}]

(* [word] is non-float *)
type t_word : word
type succeeds = t_word accepts_maybesep
type succeeds = t_word accepts_sep
type succeeds = t_word accepts_nonfloat

[%%expect{|
type t_word : word
type succeeds = t_word accepts_maybesep
type succeeds = t_word accepts_sep
type succeeds = t_word accepts_nonfloat
|}]

(* [vec128] is non-float *)
type t_vec : vec128
type succeeds = t_vec accepts_maybesep
type succeeds = t_vec accepts_sep
type succeeds = t_vec accepts_nonfloat

[%%expect{|
type t_vec : vec128
type succeeds = t_vec accepts_maybesep
type succeeds = t_vec accepts_sep
type succeeds = t_vec accepts_nonfloat
|}]

(* Testing non-value float layouts. *)
(* "non_float" in the separability axis refers to "legacy" OCaml float heap blocks,
    so float32/float64 layouts are still non_float. *)

(* [float32] is non-float *)
type t_f32 : float32
type succeeds = t_f32 accepts_maybesep
type succeeds = t_f32 accepts_sep
type succeeds = t_f32 accepts_nonfloat

[%%expect{|
type t_f32 : float32
type succeeds = t_f32 accepts_maybesep
type succeeds = t_f32 accepts_sep
type succeeds = t_f32 accepts_nonfloat
|}]

(* [float64] is non-float *)
type t_f64 : float64
type succeeds = t_f64 accepts_maybesep
type succeeds = t_f64 accepts_sep
type succeeds = t_f64 accepts_nonfloat

[%%expect{|
type t_f64 : float64
type succeeds = t_f64 accepts_maybesep
type succeeds = t_f64 accepts_sep
type succeeds = t_f64 accepts_nonfloat
|}]

(* Test basic types. *)

type succeeds = int accepts_nonfloat
type succeeds = string accepts_nonfloat
type succeeds = unit option accepts_nonfloat

[%%expect{|
type succeeds = int accepts_nonfloat
type succeeds = string accepts_nonfloat
type succeeds = unit option accepts_nonfloat
|}]

(* Floats are separable: *)

type fails = float accepts_nonfloat
[%%expect{|
Line 1, characters 13-18:
1 | type fails = float accepts_nonfloat
                 ^^^^^
Error: This type "float" should be an instance of type "('a : any non_float)"
       The layout of float is value separable
         because it is the primitive type float.
       But the layout of float must be a sublayout of any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type succeeds = float accepts_sep
[%%expect{|
type succeeds = float accepts_sep
|}]

(* Separability is a shallow property: lists or arrays of floats are non-float: *)

type succeeds = float list accepts_nonfloat
type succeeds = float array accepts_nonfloat

[%%expect{|
type succeeds = float list accepts_nonfloat
type succeeds = float array accepts_nonfloat
|}]

(* Records and variants are all non-float: *)

type t1 = { f1 : string; f2: int }
type t2 = { f1 : float; f2 : float }
type t3 = { f1 : float }
type t4 = { f1 : string; f2 : string; f3 : int }
type t5 = { f1 : string; f2 : float; f3: int64#; f4: int32# }
[%%expect{|
type t1 = { f1 : string; f2 : int; }
type t2 = { f1 : float; f2 : float; }
type t3 = { f1 : float; }
type t4 = { f1 : string; f2 : string; f3 : int; }
type t5 = { f1 : string; f2 : float; f3 : int64#; f4 : int32#; }
|}]

type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
type succeeds = t4 accepts_nonfloat
type succeeds = t5 accepts_nonfloat

[%%expect{|
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
type succeeds = t4 accepts_nonfloat
type succeeds = t5 accepts_nonfloat
|}]

type t1 = | A | B | C
type t2 = | A | B of string | C of { f1 : float; f2 : int }
type t3 = | A of { f1: int64#; f2: float# } | B of int32#

[%%expect{|
type t1 = A | B | C
type t2 = A | B of string | C of { f1 : float; f2 : int; }
type t3 = A of { f1 : int64#; f2 : float#; } | B of int32#
|}]

type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat

[%%expect{|
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
type succeeds = t3 accepts_nonfloat
|}]

type t1 = [ `A | `B | `C ]
type t2 = [ `A of string | `B ]
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat

[%%expect{|
type t1 = [ `A | `B | `C ]
type t2 = [ `A of string | `B ]
type succeeds = t1 accepts_nonfloat
type succeeds = t2 accepts_nonfloat
|}]

(* [or_null] and separability. *)

(* ['a] must be [non_float] for ['a or_null] to be [non_float] or [separable].  *)

type 'a narrowed = 'a or_null accepts_nonfloat

[%%expect{|
type ('a : value non_float) narrowed = 'a or_null accepts_nonfloat
|}]

type 'a narrowed = 'a or_null accepts_sep

[%%expect{|
type ('a : value non_float) narrowed = 'a or_null accepts_sep
|}]

type 'a succeds = 'a or_null accepts_maybesep

[%%expect{|
type 'a succeds = 'a or_null accepts_maybesep
|}]

type t_val_nonfloat : value mod non_float

type succeeds = t_val_nonfloat or_null accepts_nonfloat

[%%expect{|
type t_val_nonfloat : value non_float
type succeeds = t_val_nonfloat or_null accepts_nonfloat
|}]

type fails = t_val or_null accepts_sep

[%%expect{|
Line 1, characters 13-26:
1 | type fails = t_val or_null accepts_sep
                 ^^^^^^^^^^^^^
Error: This type "t_val or_null" should be an instance of type
         "('a : any separable)"
       The layout of t_val or_null is value
         because it is the primitive type or_null.
       But the layout of t_val or_null must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

type fails = t_b64 or_null accepts_maybesep

[%%expect{|
Line 1, characters 13-18:
1 | type fails = t_b64 or_null accepts_maybesep
                 ^^^^^
Error: This type "t_b64" should be an instance of type
         "('a : value maybe_separable)"
       The layout of t_b64 is bits64
         because of the definition of t_b64 at line 1, characters 0-19.
       But the layout of t_b64 must be a sublayout of value
         because the type argument of or_null has layout value.
|}]

type t_maybesep_val : value_or_null mod non_null
type succeeds = t_maybesep_val or_null

[%%expect{|
type t_maybesep_val : value maybe_separable
type succeeds = t_maybesep_val or_null
|}]

type ('a : value mod non_float) succeeds = 'a or_null accepts_nonfloat

[%%expect{|
type ('a : value non_float) succeeds = 'a or_null accepts_nonfloat
|}]

type ('a : value mod non_float) succeeds = 'a or_null accepts_sep
[%%expect{|
type ('a : value non_float) succeeds = 'a or_null accepts_sep
|}]

type ('a : value mod non_float) succeeds = 'a or_null accepts_maybesep

[%%expect{|
type ('a : value non_float) succeeds = 'a or_null accepts_maybesep
|}]

(* [float or_null] is not separable: *)

type fails = float or_null accepts_sep

[%%expect{|
Line 1, characters 13-26:
1 | type fails = float or_null accepts_sep
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any separable)"
       The layout of float or_null is value
         because it is the primitive type or_null.
       But the layout of float or_null must be a sublayout of any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

(* Separability and arrays: *)

(* Arrays accept separable values: *)

type fails = t_maybesep_val array

[%%expect{|
Line 1, characters 13-27:
1 | type fails = t_maybesep_val array
                 ^^^^^^^^^^^^^^
Error: This type "t_maybesep_val" should be an instance of type
         "('a : any separable)"
       The layout of t_maybesep_val is value
         because of the definition of t_maybesep_val at line 1, characters 0-48.
       But the layout of t_maybesep_val must be a sublayout of any separable
         because it's the type argument to the array type.
|}]

type ('a : value mod separable) succeeds = 'a array

[%%expect{|
type 'a succeeds = 'a array
|}]

(* Arrays accept [value_or_null mod separable] elements. *)

type should_succeed = int or_null array

[%%expect{|
type should_succeed = int or_null array
|}]

type should_succeed = string or_null array

[%%expect{|
type should_succeed = string or_null array
|}]

(* Arrays should not accept [float or_null]s: *)

type fails = float or_null array

[%%expect{|
Line 1, characters 13-26:
1 | type fails = float or_null array
                 ^^^^^^^^^^^^^
Error: This type "float or_null" should be an instance of type
         "('a : any separable)"
       The layout of float or_null is value
         because it is the primitive type or_null.
       But the layout of float or_null must be a sublayout of any separable
         because it's the type argument to the array type.
|}]

(* Arrays accept non-float [or_null] values. *)

type should_succeed = string or_null array

[%%expect{|
type should_succeed = string or_null array
|}]

type should_succeed = int or_null array

[%%expect{|
type should_succeed = int or_null array
|}]

(* With-kinds and separability. *)

(* Separability is shallow and does not interact with with-kinds. *)

type ('a : value_or_null) record : value mod non_float =
  { x : 'a; y : int or_null; z : float }

[%%expect{|
type ('a : value_or_null) record = { x : 'a; y : int or_null; z : float; }
|}]

type ('a : value_or_null) smth : immediate with 'a

type ('a : immediate) bounded

(* CR layouts v2.8: with-bounds don't peek through unboxed-like types. *)

type works = int or_null smth bounded

[%%expect{|
type ('a : value_or_null) smth : immediate with 'a
type ('a : immediate) bounded
type works = int or_null smth bounded
|}, Principal{|
type ('a : value_or_null) smth : immediate with 'a
type ('a : immediate) bounded
Line 7, characters 13-29:
7 | type works = int or_null smth bounded
                 ^^^^^^^^^^^^^^^^
Error: This type "int or_null smth" should be an instance of type
         "('a : immediate)"
       The kind of int or_null smth is immediate with int or_null
         because of the definition of smth at line 1, characters 0-50.
       But the kind of int or_null smth must be a subkind of immediate
         because of the definition of bounded at line 3, characters 0-29.
|}]


type 'a t : value mod non_float with 'a
type ('a : value mod non_float) req_non_float
type test = float t req_non_float
[%%expect{|
type 'a t : value non_float
type ('a : value non_float) req_non_float
type test = float t req_non_float
|}]

(* Separability and [@@unboxed]. *)

type unbx = { unbx : t_maybesep_val } [@@unboxed]

[%%expect{|
type unbx = { unbx : t_maybesep_val; } [@@unboxed]
|}]

type ('a : value_or_null mod non_null) unbx' = Unbx of 'a [@@unboxed]

[%%expect{|
type ('a : value maybe_separable) unbx' = Unbx of 'a [@@unboxed]
|}]

(* Separability and unboxed records. *)

(* One-element unboxed records inherit the separability of the element type .*)

type a : value = #{ a : t_maybesep_val }

[%%expect{|
Line 1, characters 0-40:
1 | type a : value = #{ a : t_maybesep_val }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "a" is value
         because it is an unboxed record.
       But the layout of type "a" must be a sublayout of value separable
         because of the annotation on the declaration of the type a.
|}]

type a : value_or_null mod non_null = #{ a : t_maybesep_val }

[%%expect{|
type a = #{ a : t_maybesep_val; }
|}]

(* In the past, [non_null] and [non_float] applied to the whole product and
   each element was not checked. Now, each element is checked so this fails. *)

type b = #{ a : int; b: t_maybesep_val; c: float# }

type ('b : value & value & float64 mod non_null non_float) fails = unit constraint 'b = b

[%%expect{|
type b = #{ a : int; b : t_maybesep_val; c : float#; }
Line 3, characters 83-89:
3 | type ('b : value & value & float64 mod non_null non_float) fails = unit constraint 'b = b
                                                                                       ^^^^^^
Error: The type constraints are not consistent.
       Type "('b : value & value & float64)" is not compatible with type "b"
       The layout of b is value non_pointer & value & float64
         because of the definition of b at line 1, characters 0-51.
       But the layout of b must be a sublayout of
           value separable & value separable & float64
         because of the annotation on 'b in the declaration of the type fails.
|}]

type c = #( float * float or_null * float# )

type ('c : value & value & float64 mod non_null non_float) fails = unit constraint 'c = c

[%%expect{|
type c = #(float * float or_null * float#)
Line 3, characters 83-89:
3 | type ('c : value & value & float64 mod non_null non_float) fails = unit constraint 'c = c
                                                                                       ^^^^^^
Error: The type constraints are not consistent.
       Type "('c : value & value & float64)" is not compatible with type
         "c" = "#(float * float or_null * float#)"
       The layout of c is value separable & value & float64
         because it is an unboxed tuple.
       But the layout of c must be a sublayout of
           value separable & value separable & float64
         because of the annotation on 'c in the declaration of the type fails.
|}]

(* Separability and [@@or_null_reexport]. *)

(* Re-exported [or_null] types preserve separability behavior. *)

module Or_null_reexport = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end

(* Generic ['a or_null] re-export is still maybe-separable. *)

type 'a narrowed = 'a Or_null_reexport.t accepts_nonfloat

[%%expect{|
module Or_null_reexport :
  sig type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport] end
type ('a : value non_float) narrowed = 'a Or_null_reexport.t accepts_nonfloat
|}]

type 'a narrowed = 'a Or_null_reexport.t accepts_sep

[%%expect{|
type ('a : value non_float) narrowed = 'a Or_null_reexport.t accepts_sep
|}]

type 'a succeeds = 'a Or_null_reexport.t accepts_maybesep

[%%expect{|
type 'a succeeds = 'a Or_null_reexport.t accepts_maybesep
|}]

(* Non-float constraints are preserved through re-export. *)

type ('a : value mod non_float) succeeds = 'a Or_null_reexport.t accepts_nonfloat

[%%expect{|
type ('a : value non_float) succeeds = 'a Or_null_reexport.t accepts_nonfloat
|}]

(* Specific type instances preserve their separability. *)

type succeeds = int Or_null_reexport.t accepts_nonfloat

[%%expect{|
type succeeds = int Or_null_reexport.t accepts_nonfloat
|}]

type fails = float Or_null_reexport.t accepts_nonfloat

[%%expect{|
Line 1, characters 13-37:
1 | type fails = float Or_null_reexport.t accepts_nonfloat
                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "float Or_null_reexport.t" = "float or_null"
       should be an instance of type "('a : any non_float)"
       The layout of float Or_null_reexport.t is value
         because it is the primitive type or_null.
       But the layout of float Or_null_reexport.t must be a sublayout of
           any non_float
         because of the definition of accepts_nonfloat at line 3, characters 0-46.
|}]

type fails = float Or_null_reexport.t accepts_sep

[%%expect{|
Line 1, characters 13-37:
1 | type fails = float Or_null_reexport.t accepts_sep
                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type "float Or_null_reexport.t" = "float or_null"
       should be an instance of type "('a : any separable)"
       The layout of float Or_null_reexport.t is value
         because it is the primitive type or_null.
       But the layout of float Or_null_reexport.t must be a sublayout of
           any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

(* Peeking through unboxed types *)

(* CR or-null: test [('a : value_or_null) unbx] when it's allowed. *)

type 'a unbx : value = Box of 'a [@@unboxed]

[%%expect{|
type 'a unbx = Box of 'a [@@unboxed]
|}]

type succeeds = string unbx or_null accepts_nonfloat

[%%expect{|
type succeeds = string unbx or_null accepts_nonfloat
|}]

type fails = float unbx or_null accepts_sep

[%%expect{|
Line 1, characters 13-31:
1 | type fails = float unbx or_null accepts_sep
                 ^^^^^^^^^^^^^^^^^^
Error: This type "float unbx or_null" should be an instance of type
         "('a : any separable)"
       The layout of float unbx or_null is value
         because it is the primitive type or_null.
       But the layout of float unbx or_null must be a sublayout of
           any separable
         because of the definition of accepts_sep at line 2, characters 0-41.
|}]

(* [@@layout_poly] respects separability. *)

external[@layout_poly] make_vect : ('a : any mod separable) . int -> 'a -> 'a array =
  "%makearray_dynamic"

[%%expect{|
external make_vect : ('a : any separable). int -> 'a -> 'a array
  = "%makearray_dynamic" [@@layout_poly]
|}]

let succeeds = make_vect 2 (This 4)
[%%expect{|
val succeeds : int or_null array = [|This 4; This 4|]
|}]

let fails = make_vect 3 (This 5.)

[%%expect{|
Line 1, characters 30-32:
1 | let fails = make_vect 3 (This 5.)
                                  ^^
Error: This expression has type "float" but an expression was expected of type
         "('a : value non_float)"
       The layout of float is value separable
         because it is the primitive type float.
       But the layout of float must be a sublayout of value non_float
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* Module inclusion tests with or_null and separability *)

module type S = sig
  type ('a : value mod non_float) t : value_or_null mod non_float
end

module M1 : S = struct
  type ('a : value mod non_float) t = 'a or_null
end

[%%expect{|
module type S =
  sig type ('a : value non_float) t : value_or_null non_float end
module M1 : S
|}]

module M2 : S = struct
  type ('a : value mod non_float) t = 'a Or_null_reexport.t
end

[%%expect{|
module M2 : S
|}]

module M3_fail : S = struct
  type 'a t = float or_null
end

[%%expect{|
Lines 1-3, characters 21-3:
1 | .....................struct
2 |   type 'a t = float or_null
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = float or_null end
       is not included in
         S
       Type declarations do not match:
         type 'a t = float or_null
       is not included in
         type ('a : value non_float) t : value_or_null non_float
       The layout of the first is value
         because it is the primitive type or_null.
       But the layout of the first must be a sublayout of value non_float
         because of the definition of t at line 2, characters 2-65.
|}]

module M4 : S = struct
  type 'a t = int or_null
end

[%%expect{|
module M4 : S
|}]

module type S0 = sig
  type t : value_or_null mod separable
end

[%%expect{|
module type S0 = sig type t : value_or_null separable end
|}]

module M5 : S0 = struct
  type t = int
end

[%%expect{|
module M5 : S0
|}]

module M6 : S0 = struct
  type t = string or_null
end

[%%expect{|
module M6 : S0
|}]

module M7_fail : S0 = struct
  type t = float or_null
end

[%%expect{|
Lines 1-3, characters 22-3:
1 | ......................struct
2 |   type t = float or_null
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = float or_null end
       is not included in
         S0
       Type declarations do not match:
         type t = float or_null
       is not included in
         type t : value_or_null separable
       The layout of the first is value
         because it is the primitive type or_null.
       But the layout of the first must be a sublayout of value separable
         because of the definition of t at line 2, characters 2-38.
|}]

(* Separability and [@@unboxed] existential types. *)

(* Some [@@unboxed] existentials are non-separable and thus forbidden. *)
(* CR separability: mark them as non-separable instead. *)

type 'a abstract

type packed = P : 'a abstract -> packed [@@unboxed]
[%%expect{|
type 'a abstract
Line 3, characters 0-51:
3 | type packed = P : 'a abstract -> packed [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* [non_float] annotations allow us to bypass this check. *)

type 'a non_float : value mod non_float

type packed = P : 'a non_float -> packed [@@unboxed]

[%%expect{|
type 'a non_float : value non_float
type packed = P : 'a non_float -> packed [@@unboxed]
|}]

(* [non_float] also works on existential variables. *)

type exists = E : ('a : value mod non_float) . 'a -> exists [@@unboxed]

[%%expect{|
type exists = E : ('a : value non_float). 'a -> exists [@@unboxed]
|}]

(* Non-value layouts do not trigger the check. *)

type 'a void : void

type packed_void = P : 'a void -> packed_void [@@unboxed]

[%%expect{|
type 'a void : void
type packed_void = P : 'a void -> packed_void [@@unboxed]
|}]

type exists_word = W : ('a : word) . 'a -> exists_word [@@unboxed]

[%%expect{|
type exists_word = W : ('a : word). 'a -> exists_word [@@unboxed]
|}]

(* With-bounds don't affect separability here: *)

type 'a abs1 : value mod non_float with 'a

type packed = P : 'a abs1 -> packed [@@unboxed]

[%%expect{|
type 'a abs1 : value non_float
type packed = P : 'a abs1 -> packed [@@unboxed]
|}]
