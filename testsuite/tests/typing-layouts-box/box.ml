(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Test 1: box_ on unboxed types - the manifest is stored as Tbox but
   expands to the boxed type during unification *)

type t1 = float# box_;;
[%%expect{|
type t1 = float# box_
|}]

type t2 = int32# box_;;
[%%expect{|
type t2 = int32# box_
|}]

type t3 = int64# box_;;
[%%expect{|
type t3 = int64# box_
|}]

type t4 = nativeint# box_;;
[%%expect{|
type t4 = nativeint# box_
|}]

(* Test 2: box_ through type aliases - the key test showing that
   u box_ where u = float# is equal to float *)

type u = float#
type t = u box_;;
[%%expect{|
type u = float#
type t = u box_
|}]

(* This is the key test: t unifies with float because u box_ expands to float *)
let f (x : t) : float = x;;
[%%expect{|
val f : t -> float = <fun>
|}]

let g (x : float) : t = x;;
[%%expect{|
val g : float -> t = <fun>
|}]

(* Test 3: Direct float# box_ also unifies with float *)

let h (x : float# box_) : float = x;;
[%%expect{|
val h : float# box_ -> float = <fun>
|}]

let i (x : float) : float# box_ = x;;
[%%expect{|
val i : float -> float# box_ = <fun>
|}]

(* Test 4: box_ types unify with themselves in function types *)

let eq_box (x : int box_) (y : int box_) = x = y;;
[%%expect{|
val eq_box : int box_ -> int box_ -> bool = <fun>
|}]

let eq_float_box (x : float# box_) (y : float# box_) = x = y;;
[%%expect{|
val eq_float_box : float# box_ -> float# box_ -> bool = <fun>
|}]

(* Test 5: box_ in module signatures *)

module type S = sig
  type t = float# box_
  val x : t
end;;
[%%expect{|
module type S = sig type t = float# box_ val x : t end
|}]

module M : S = struct
  type t = float# box_
  let x = 1.0
end;;
[%%expect{|
module M : S
|}]

(* Test 6: Using the module's type *)

let use_m : float = M.x;;
[%%expect{|
val use_m : float = 1.
|}]

(* Test 7: int32# box_ = int32 *)

type t32 = int32# box_;;
[%%expect{|
type t32 = int32# box_
|}]

let f32 (x : t32) : int32 = x;;
[%%expect{|
val f32 : t32 -> int32 = <fun>
|}]

(* Test 8: int64# box_ = int64 *)

type t64 = int64# box_;;
[%%expect{|
type t64 = int64# box_
|}]

let f64 (x : t64) : int64 = x;;
[%%expect{|
val f64 : t64 -> int64 = <fun>
|}]

(* Test 9: nativeint# box_ = nativeint *)

type tnat = nativeint# box_;;
[%%expect{|
type tnat = nativeint# box_
|}]

let fnat (x : tnat) : nativeint = x;;
[%%expect{|
val fnat : tnat -> nativeint = <fun>
|}]

(* Test 10: Polymorphic box_ with explicit jkind annotation *)

let check_boxed_by : type (a : float64). a -> a box_ -> unit =
  fun _ _ -> ();;
[%%expect{|
val check_boxed_by : ('a : float64). 'a -> 'a box_ -> unit = <fun>
|}]

type uf = float#
let test_check (u : uf) (f : float) = check_boxed_by u f;;
[%%expect{|
type uf = float#
val test_check : uf -> float -> unit = <fun>
|}]
