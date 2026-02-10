(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* This test checks that you get an error if you attempts to manipulate iarrays
   of unboxed products are rejected. *)
(* CR layouts v7.1: Support iarrays of unboxed products. *)


(* makearray_dynamic *)
external[@layout_poly] make_vect : ('a : any mod separable) . int -> 'a -> 'a iarray
  = "%makearray_dynamic"

let make_scannable (x : #(int * string)) = make_vect 42 x
[%%expect{|
external make_vect : ('a : any mod separable). int -> 'a -> 'a iarray
  = "%makearray_dynamic" [@@layout_poly]
val make_scannable : #(int * string) -> #(int * string) iarray = <fun>
|}]

let make_ignorable (x : #(int * float#)) = make_vect 42 x
[%%expect{|
val make_ignorable : #(int * float#) -> #(int * float#) iarray = <fun>
|}]

let make_bad (x : #(string * float#)) = make_vect 42 x

[%%expect{|
Line 1, characters 40-54:
1 | let make_bad (x : #(string * float#)) = make_vect 42 x
                                            ^^^^^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* array length *)
external[@layout_poly] len : ('a : any mod separable) . 'a iarray -> int =
  "%array_length"

let length_scannable (x : #(int * string) iarray) = len x
[%%expect{|
external len : ('a : any mod separable). 'a iarray -> int = "%array_length"
  [@@layout_poly]
val length_scannable : #(int * string) iarray -> int = <fun>
|}]

let length_ignorable (x : #(int * float#) iarray) = len x
[%%expect{|
val length_ignorable : #(int * float#) iarray -> int = <fun>
|}]

let length_bad (x : #(string * float#) iarray) = len x
[%%expect{|
Line 1, characters 49-54:
1 | let length_bad (x : #(string * float#) iarray) = len x
                                                     ^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* safe get *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int -> 'a =
  "%array_safe_get"

let get_scannable (x : #(int * string) iarray) = get x 42
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x 42
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 46-54:
1 | let get_bad (x : #(string * float#) iarray) = get x 42
                                                  ^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* unsafe get *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int -> 'a =
  "%array_unsafe_get"

let get_scannable (x : #(int * string) iarray) = get x 42
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int -> 'a
  = "%array_unsafe_get" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x 42
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 46-54:
1 | let get_bad (x : #(string * float#) iarray) = get x 42
                                                  ^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* safe get indexed by int64# *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int64# -> 'a =
  "%array_safe_get_indexed_by_int64#"

let get_scannable (x : #(int * string) iarray) = get x #42L
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int64# -> 'a
  = "%array_safe_get_indexed_by_int64#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42L
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42L
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* unsafe get indexed by int64# *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int64# -> 'a =
  "%array_unsafe_get_indexed_by_int64#"

let get_scannable (x : #(int * string) iarray) = get x #42L
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int64# -> 'a
  = "%array_unsafe_get_indexed_by_int64#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42L
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42L
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* safe get indexed by int32# *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int32# -> 'a =
  "%array_safe_get_indexed_by_int32#"

let get_scannable (x : #(int * string) iarray) = get x #42l
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int32# -> 'a
  = "%array_safe_get_indexed_by_int32#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42l
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42l
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* unsafe get indexed by int32# *)
external[@layout_poly] get : ('a : any mod separable) . 'a iarray -> int32# -> 'a =
  "%array_unsafe_get_indexed_by_int32#"

let get_scannable (x : #(int * string) iarray) = get x #42l
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int32# -> 'a
  = "%array_unsafe_get_indexed_by_int32#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42l
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42l
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* safe get indexed by nativeint# *)
external[@layout_poly] get :
  ('a : any mod separable) . 'a iarray -> nativeint# -> 'a =
  "%array_safe_get_indexed_by_nativeint#"

let get_scannable (x : #(int * string) iarray) = get x #42n
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> nativeint# -> 'a
  = "%array_safe_get_indexed_by_nativeint#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42n
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42n
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* unsafe get indexed by nativeint# *)
external[@layout_poly] get :
  ('a : any mod separable) . 'a iarray -> nativeint# -> 'a =
  "%array_unsafe_get_indexed_by_nativeint#"

let get_scannable (x : #(int * string) iarray) = get x #42n
[%%expect{|
external get : ('a : any mod separable). 'a iarray -> nativeint# -> 'a
  = "%array_unsafe_get_indexed_by_nativeint#" [@@layout_poly]
val get_scannable : #(int * string) iarray -> #(int * string) = <fun>
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42n
[%%expect{|
val get_ignorable : #(int * float#) iarray -> #(int * float#) = <fun>
|}]

let get_bad (x : #(string * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42n
                                                  ^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(string * float#), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* expression literals *)
let f_scannable_literal (type a : value mod external_)
      (x : int) (y : a) (z : bool option) = [: #(x, y, z) :]
[%%expect{|
val f_scannable_literal :
  ('a : value mod external_).
    int -> 'a -> bool option -> #(int * 'a * bool option) iarray =
  <fun>
|}]

let f_scannable_empty_literal (type a : value mod external_)
  : #(int * a * bool option) iarray = [: :]
[%%expect{|
val f_scannable_empty_literal :
  ('a : value mod external_). #(int * 'a * bool option) iarray = [::]
|}]

let f_ignorable_literal (type a : value mod external_)
      (x : int) (y : a) (z : #(int64# * float#)) = [: #(x, y, z) :]
[%%expect{|
val f_ignorable_literal :
  ('a : value mod external_).
    int ->
    'a -> #(int64# * float#) -> #(int * 'a * #(int64# * float#)) iarray =
  <fun>
|}]

let f_ignorable_empty_literal (type a : value mod external_)
  : #(int * a * #(int64# * float#)) iarray = [: :]
[%%expect{|
val f_ignorable_empty_literal :
  ('a : value mod external_). #(int * 'a * #(int64# * float#)) iarray =
  [::]
|}]

let f_illegal_literal (type a : value mod external_)
      (x : float#) (y : a) (z : bool option) = [: #(x, y, z) :]
[%%expect{|
Line 2, characters 47-63:
2 |       (x : float#) (y : a) (z : bool option) = [: #(x, y, z) :]
                                                   ^^^^^^^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(float# * a * bool option), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

let f_illegal_empty_literal (type a : value mod external_)
  : #(float# * a * bool option) iarray = [: :]
[%%expect{|
Lines 1-2, characters 28-46:
1 | ............................(type a : value mod external_)
2 |   : #(float# * a * bool option) iarray = [: :]
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(float# * 'a * bool option), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

(* pattern literals *)
let f_scannable_literal arr : #(bool option * string * int) =
  match arr with
  | [: :] -> #(None, "hi", 42)
  | [: #(x, y, z) :] -> #(z, y, x)
  | _ -> assert false
[%%expect{|
val f_scannable_literal :
  #(int * string * bool option) iarray -> #(bool option * string * int) =
  <fun>
|}]

let f_ignorable_literal arr : #(#(int64# * float#) * int32# * int) =
  match arr with
  | [: :] -> #(#(#42L, #3.14), #10l, 43)
  | [: #(x, y, #(z, q)) :] -> #(#(q, z), y, x)
  | _ -> assert false
[%%expect{|
val f_ignorable_literal :
  #(int * int32# * #(float# * int64#)) iarray ->
  #(#(int64# * float#) * int32# * int) = <fun>
|}]

let f_illegal_literal : #(float# * bool option * int) iarray -> int =
  function
  | [: #(a,b,c) :] -> 1
  | _ -> 0
[%%expect{|
Line 3, characters 4-18:
3 |   | [: #(a,b,c) :] -> 1
        ^^^^^^^^^^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(float# * bool option * int), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]

let f_illegal_empty_literal : #(float# * bool option * int) iarray -> int =
  function
  | [: :] -> 0
  | _ -> 1
[%%expect{|
Line 3, characters 4-9:
3 |   | [: :] -> 0
        ^^^^^
Error: An unboxed product array element must be formed from all
       external types (which are ignored by the gc) or all gc-scannable types.
       But this array operation is peformed for an array whose
       element type is #(float# * bool option * int), which is an unboxed product
       that is not external and contains a type with the non-scannable
       layout float64.
       Hint: if the array contents should not be scanned, annotating
       contained abstract types as [mod external] may resolve this error.
|}]
