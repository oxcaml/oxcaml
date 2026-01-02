(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension-universe beta";
   expect;
   }{
   flags = "-extension-universe stable";
   expect;
 }
*)
(* Tests around type-checking iarrays of unboxed types. Tests around
   compilation correctness should go somewhere else. *)

(********************************************)
(* Test 1: Support unboxed types in iarrays *)

type t_any_mod_separable : any mod separable

type t1 = float# iarray
type t2 = int32# iarray
type t3 = int64# iarray
type t4 = nativeint# iarray
type t5 = t_any_mod_separable iarray
type t6 = float32# iarray

type ('a : float64) t1' = 'a iarray
type ('a : bits32) t2' = 'a iarray
type ('a : bits64) t3' = 'a iarray
type ('a : word) t4' = 'a iarray
type ('a : any mod separable) t5' = 'a iarray
type ('a : float32) t6' = 'a iarray

[%%expect{|
type t_any_mod_separable : any mod separable
type t1 = float# iarray
type t2 = int32# iarray
type t3 = int64# iarray
type t4 = nativeint# iarray
type t5 = t_any_mod_separable iarray
type t6 = float32# iarray
type ('a : float64) t1' = 'a iarray
type ('a : bits32) t2' = 'a iarray
type ('a : bits64) t3' = 'a iarray
type ('a : word) t4' = 'a iarray
type ('a : any mod separable) t5' = 'a iarray
type ('a : float32) t6' = 'a iarray
|}];;

(******************************)
(* Test 2: iarray expressions *)

let v1 = [:  #1. :]
[%%expect{|
val v1 : float# iarray = [:<abstr>:]
|}];;


let v2 = [: #1l :]
[%%expect{|
val v2 : int32# iarray = [:<abstr>:]
|}];;


let v3 = [: #1L :]
[%%expect{|
val v3 : int64# iarray = [:<abstr>:]
|}];;


let v4 = [: #1n :]
[%%expect{|
val v4 : nativeint# iarray = [:<abstr>:]
|}];;

let v5 = [: #1.s :]
[%%expect{|
val v5 : float32# iarray = [:<abstr>:]
|}];;

(****************************************)
(* Test 3: Array operations do not work *)

let f (x : float# iarray) = x.:(0)
[%%expect{|
Line 1, characters 28-34:
1 | let f (x : float# iarray) = x.:(0)
                                ^^^^^^
Error: Unbound value "(.:())"
|}];;

let f (x : float# iarray) = Iarray.length x
[%%expect{|
Line 1, characters 28-41:
1 | let f (x : float# iarray) = Iarray.length x
                                ^^^^^^^^^^^^^
Error: Unbound module "Iarray"
Hint: Did you mean "Array"?
|}];;

(*****************************************************************
 * Test 4: Calling wrong primitives on unboxed iarray kinds fails *)

external get : float# iarray -> int -> float = "%floatarray_safe_get"
let d (x : float# iarray) = get x 0

[%%expect{|
external get : float# iarray -> int -> float = "%floatarray_safe_get"
Line 2, characters 28-35:
2 | let d (x : float# iarray) = get x 0
                                ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;


(* [Obj.magic] can bypass the error but this should be discouraged *)
external get : floatarray -> int -> float = "%floatarray_safe_get"
let d (x : float# iarray) = get (Obj.magic x : floatarray) 0

[%%expect{|
external get : floatarray -> int -> float = "%floatarray_safe_get"
val d : float# iarray -> float = <fun>
|}];;

external get :
  ('a : any mod separable). 'a iarray -> int -> float = "%floatarray_safe_get"
let d (x : 'a iarray) = get x 0

[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int -> float
  = "%floatarray_safe_get"
val d : ('a : value_or_null mod separable). 'a iarray -> float = <fun>
|}];;

external get : int32# iarray -> int -> float = "%floatarray_safe_get"
let d (x : int32# iarray) = get x 0

[%%expect{|
external get : int32# iarray -> int -> float = "%floatarray_safe_get"
Line 2, characters 28-35:
2 | let d (x : int32# iarray) = get x 0
                                ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : int64# iarray -> int -> float = "%floatarray_safe_get"
let d (x : int64# iarray) = get x 0

[%%expect{|
external get : int64# iarray -> int -> float = "%floatarray_safe_get"
Line 2, characters 28-35:
2 | let d (x : int64# iarray) = get x 0
                                ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : nativeint# iarray -> int -> float = "%floatarray_safe_get"
let d (x : nativeint# iarray) = get x 0

[%%expect{|
external get : nativeint# iarray -> int -> float = "%floatarray_safe_get"
Line 2, characters 32-39:
2 | let d (x : nativeint# iarray) = get x 0
                                    ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : float32# iarray -> int -> float = "%floatarray_safe_get"
let d (x : float32# iarray) = get x 0

[%%expect{|
external get : float32# iarray -> int -> float = "%floatarray_safe_get"
Line 2, characters 30-37:
2 | let d (x : float32# iarray) = get x 0
                                  ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

(***************************)
(* Test 5: [@layout_poly] *)

external[@layout_poly] get :
  ('a : any mod separable). 'a iarray -> int -> 'a = "%array_safe_get"
let f1 (x : float# iarray) = get x 0
let f2 (x : int32# iarray) = get x 0
let f3 (x : int64# iarray) = get x 0
let f4 (x : nativeint# iarray) = get x 0
let f5 (x : float32# iarray) = get x 0

[%%expect{|
external get : ('a : any mod separable). 'a iarray -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
val f1 : float# iarray -> float# = <fun>
val f2 : int32# iarray -> int32# = <fun>
val f3 : int64# iarray -> int64# = <fun>
val f4 : nativeint# iarray -> nativeint# = <fun>
val f5 : float32# iarray -> float32# = <fun>
|}];;

(************************************)
(* Test 6: sort variable inference *)

module M6_1 = struct
  (* sort var in pat *)

  let get_third arr =
    match arr with
    | [: _; _; z :] -> z
    | _ -> assert false

  let _ = assert (Stdlib_upstream_compatible.Int32_u.equal
                    #42l (get_third [: #0l; #1l; #42l :]))

  let _ = assert (Stdlib_upstream_compatible.Int64_u.equal
                    #42L (get_third [: #0L; #1L; #42L :]))
end

[%%expect{|
Line 13, characters 39-42:
13 |                     #42L (get_third [: #0L; #1L; #42L :]))
                                            ^^^
Error: This expression has type "int64#" but an expression was expected of type
         "('a : bits32 mod separable)"
       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of bits32
         because of the definition of get_third at lines 4-7, characters 16-23.
|}]

module M6_2 = struct
  (* sort var in exp *)

  external[@layout_poly] get :
    ('a : any mod separable). 'a iarray -> int -> 'a = "%array_safe_get"

  let arr = [::]

  let f1 idx : float# = get arr idx
  let f2 idx : int32# = get arr idx
end

[%%expect{|
Line 10, characters 24-35:
10 |   let f2 idx : int32# = get arr idx
                             ^^^^^^^^^^^
Error: This expression has type "('a : float64 mod separable)"
       but an expression was expected of type "int32#"
       The layout of int32# is bits32
         because it is the unboxed version of the primitive type int32.
       But the layout of int32# must be a sublayout of float64
         because of the definition of arr at line 7, characters 12-16.
|}]

(**********************)
(* Test 7: rec check *)

(* See upstream PR #6939 *)

let _ =
  let[@warning "-10"] rec x = [: x :]; #42.0 in
  ();;
[%%expect{|
Line 2, characters 39-44:
2 |   let[@warning "-10"] rec x = [: x :]; #42.0 in
                                           ^^^^^
Error: This expression has type "float#" but an expression was expected of type
         "('a : value_or_null mod separable)"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value
         because it's the type of an array element.
|}]

let _ =
  let[@warning "-10"] rec x = [: x :]; #42l in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [: x :]; #42l in
                                           ^^^^
Error: This expression has type "int32#" but an expression was expected of type
         "('a : value_or_null mod separable)"
       The layout of int32# is bits32
         because it is the unboxed version of the primitive type int32.
       But the layout of int32# must be a sublayout of value
         because it's the type of an array element.
|}]

let _ =
  let[@warning "-10"] rec x = [: x :]; #42L in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [: x :]; #42L in
                                           ^^^^
Error: This expression has type "int64#" but an expression was expected of type
         "('a : value_or_null mod separable)"
       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the type of an array element.
|}]

let _ =
  let[@warning "-10"] rec x = [: x :]; #42n in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [: x :]; #42n in
                                           ^^^^
Error: This expression has type "nativeint#"
       but an expression was expected of type
         "('a : value_or_null mod separable)"
       The layout of nativeint# is word
         because it is the unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's the type of an array element.
|}]

let _ =
  let[@warning "-10"] rec x = [: x :]; #42.0s in
  ();;

[%%expect{|
Line 2, characters 39-45:
2 |   let[@warning "-10"] rec x = [: x :]; #42.0s in
                                           ^^^^^^
Error: This expression has type "float32#"
       but an expression was expected of type
         "('a : value_or_null mod separable)"
       The layout of float32# is float32
         because it is the unboxed version of the primitive type float32.
       But the layout of float32# must be a sublayout of value
         because it's the type of an array element.
|}]

(*******************************************************************)
(* Test 8: Locality - iarrays can contain products with local values *)

let use_global (x : string) = ()
[%%expect{|
val use_global : string -> unit = <fun>
|}]

(* iarrays can hold unboxed products containing local values *)
let f () =
  let x @ local = "hello" in
  let _ : #(string * int) iarray = [: #(x, 42) :] in
  ()
[%%expect{|
val f : unit -> unit = <fun>
|}]

(* mutable arrays cannot hold unboxed products containing local values *)
let f () =
  let x @ local = "hello" in
  let _ : #(string * int) array = [| #(x, 42) |] in
  ()
[%%expect{|
Line 3, characters 39-40:
3 |   let _ : #(string * int) array = [| #(x, 42) |] in
                                           ^
Error: This value is "local" but is expected to be "global"
       because it is an element of the tuple at Line 3, characters 37-45
       which is expected to be "global"
       because it is an element (with some modality) of the array at Line 3, characters 34-48.
|}]

(* Extracting local values from iarrays via pattern matching:
   the extracted value is local when the iarray is local *)
let f (arr : #(string * int) iarray @ local) =
  match arr with
  | [: #(s, _) :] -> use_global s
  | _ -> ()
[%%expect{|
Line 3, characters 32-33:
3 |   | [: #(s, _) :] -> use_global s
                                    ^
Error: This value is "local" to the parent region
       because it is an element of the tuple at Line 3, characters 7-14
       which is "local" to the parent region
       because it is an element of the array at Line 3, characters 4-17
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* Same with let pattern *)
let f (arr : #(string * int) iarray @ local) =
  let [: #(s, _) :] = arr in
  use_global s
[%%expect{|
Line 2, characters 6-19:
2 |   let [: #(s, _) :] = arr in
          ^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[:  :]

Line 3, characters 13-14:
3 |   use_global s
                 ^
Error: This value is "local" to the parent region
       because it is an element of the tuple at Line 2, characters 9-16
       which is "local" to the parent region
       because it is an element of the array at Line 2, characters 6-19
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]
