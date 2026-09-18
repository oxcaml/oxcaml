(* TEST
 include stdlib_stable;
 flags = "-extension layouts_beta";
 flambda2;
 { expect.opt; }
*)

external box : ('a : any). ('a[@local_opt]) -> ('a box[@local_opt]) = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box[@local_opt]) -> ('a[@local_opt]) = "%unbox" [@@layout_poly]
[%%expect{|
external box : ('a : any). ('a [@local_opt]) -> ('a box [@local_opt])
  = "%box" [@@layout_poly]
external unbox : ('a : any). ('a box [@local_opt]) -> ('a [@local_opt])
  = "%unbox" [@@layout_poly]
|}]

(* TESTING INVARIANT: [unbox (box x)] is [x]. For values this means physical
   equality; for unboxed numbers it means equality of the underlying number.
   See [box_primitive.ml] for the shape of the intermediate block. *)

external untag_int : int -> int# = "%int#_of_int"
external int_equal : int# -> int# -> bool = "%int#_equal"
external int8_equal : int8# -> int8# -> bool = "%int8#_equal"
external int16_equal : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
[%%expect{|
external untag_int : int -> int# = "%int#_of_int"
external int_equal : int# -> int# -> bool = "%int#_equal"
external int8_equal : int8# -> int8# -> bool = "%int8#_equal"
external int16_equal : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
|}]

(* A string allocated at runtime so that physical equality is meaningful. *)
let s = String.make 3 'a'
[%%expect{|
val s : string = "aaa"
|}]

(* Values *)

type mixed = { mx : int64_u; my : string }
type with_product = { wp : #(int64_u * string); wq : int }
type all_flat = { af : float#; ag : int32_u }
type float_record = { fr1 : float; fr2 : float }
type variant = A of int64_u * string | B | C of #(float# * int)

let () =
  assert (unbox (box 42) = 42);
  assert (unbox (box s) == s);
  let fl = Sys.opaque_identity 3.25 in
  assert (unbox (box fl) == fl);
  let o = Some s in
  assert (unbox (box o) == o);
  let m = { mx = #1L; my = s } in
  assert (unbox (box m) == m);
  let p = { wp = #(#2L, s); wq = 3 } in
  assert (unbox (box p) == p);
  let f = { af = #1.5; ag = #4l } in
  assert (unbox (box f) == f);
  let fr = { fr1 = 1.5; fr2 = 2.5 } in
  assert (unbox (box fr) == fr);
  let a = A (#5L, s) and b = B and c = C #(#6.5, 7) in
  assert (unbox (box a) == a);
  assert (unbox (box b) == b);
  assert (unbox (box c) == c)
[%%expect{|
type mixed = { mx : int64_u; my : string; }
type with_product = { wp : #(int64_u * string); wq : int; }
type all_flat = { af : float#; ag : int32_u; }
type float_record = { fr1 : float; fr2 : float; }
type variant = A of int64_u * string | B | C of #(float# * int)
|}]

(* The same, but through [Sys.opaque_identity] so that the box is not
   statically known and the load really happens. *)

let () =
  assert (unbox (Sys.opaque_identity (box 42)) = 42);
  assert (unbox (Sys.opaque_identity (box s)) == s);
  let o = Some s in
  assert (unbox (Sys.opaque_identity (box o)) == o);
  let m = { mx = #1L; my = s } in
  assert (unbox (Sys.opaque_identity (box m)) == m);
  let a = A (#5L, s) in
  assert (unbox (Sys.opaque_identity (box a)) == a)
[%%expect{|
|}]

(* Unboxed numbers stored in a whole word *)

let () =
  assert (Float.equal (box_float (unbox (box #3.25))) 3.25);
  assert (Int64.equal (box_int64 (unbox (box #42L))) 42L);
  assert (Nativeint.equal (box_nativeint (unbox (box #42n))) 42n);
  assert (int_equal (unbox (box (untag_int 42))) (untag_int 42));
  (* not statically known *)
  assert (Float.equal (box_float (unbox (Sys.opaque_identity (box #3.25)))) 3.25);
  assert (Int64.equal (box_int64 (unbox (Sys.opaque_identity (box #42L)))) 42L);
  assert (Nativeint.equal
            (box_nativeint (unbox (Sys.opaque_identity (box #42n)))) 42n);
  assert (int_equal (unbox (Sys.opaque_identity (box (untag_int 42))))
            (untag_int 42))
[%%expect{|
|}]

(* Unboxed numbers narrower than a word *)

let () =
  assert (box_float32 (unbox (box #3.25s)) = box_float32 #3.25s);
  assert (Int32.equal (box_int32 (unbox (box #42l))) 42l);
  assert (int8_equal (unbox (box #42s)) #42s);
  assert (int16_equal (unbox (box #42S)) #42S);
  (* not statically known *)
  assert (box_float32 (unbox (Sys.opaque_identity (box #3.25s)))
          = box_float32 #3.25s);
  assert (Int32.equal (box_int32 (unbox (Sys.opaque_identity (box #42l)))) 42l);
  assert (int8_equal (unbox (Sys.opaque_identity (box #42s))) #42s);
  assert (int16_equal (unbox (Sys.opaque_identity (box #42S))) #42S)
[%%expect{|
|}]

(* Local allocation. Unboxing a local box yields a local value. *)

let () =
  let local_ value = box s in
  assert (unbox value == s);
  let local_ o = Some s in
  let local_ outer = box o in
  assert (unbox outer == o);
  let local_ f = box #3.25 in
  assert (Float.equal (box_float (unbox f)) 3.25);
  let local_ i = box #42L in
  assert (Int64.equal (box_int64 (unbox i)) 42L);
  let local_ n = box #42s in
  assert (int8_equal (unbox n) #42s)
[%%expect{|
|}]
