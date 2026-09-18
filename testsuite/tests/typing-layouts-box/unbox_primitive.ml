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

(* Unboxed products. The box is laid out like a record with one field per
   component, with values moved to a prefix and flat data to a suffix, so
   unboxing must undo that permutation. *)

(* Unboxed tuples of values only *)

let () =
  let #(a, b) = unbox (box #(42, s)) in
  assert (a = 42 && b == s);
  let o = Some s in
  let #(a, b, c) = unbox (box #(s, o, 7)) in
  assert (a == s && b == o && c = 7);
  (* not statically known *)
  let #(a, b) = unbox (Sys.opaque_identity (box #(42, s))) in
  assert (a = 42 && b == s);
  let #(a, b, c) = unbox (Sys.opaque_identity (box #(s, o, 7))) in
  assert (a == s && b == o && c = 7)
[%%expect{|
|}]

(* CR zeisbach: add tests for mixed tuples after rebasing onto them *)
(*
(* Unboxed tuples mixing values and flat data, in various orders *)

let () =
  let #(a, b) = unbox (box #(#42L, s)) in
  assert (Int64.equal (box_int64 a) 42L && b == s);
  let #(a, b) = unbox (box #(s, #42L)) in
  assert (a == s && Int64.equal (box_int64 b) 42L);
  let #(a, b, c, d, e) = unbox (box #(#1L, #2.5, s, 3, #4L)) in
  assert (Int64.equal (box_int64 a) 1L);
  assert (Float.equal (box_float b) 2.5);
  assert (c == s && d = 3);
  assert (Int64.equal (box_int64 e) 4L);
  (* not statically known *)
  let #(a, b) = unbox (Sys.opaque_identity (box #(#42L, s))) in
  assert (Int64.equal (box_int64 a) 42L && b == s);
  let #(a, b) = unbox (Sys.opaque_identity (box #(s, #42L))) in
  assert (a == s && Int64.equal (box_int64 b) 42L);
  let #(a, b, c, d, e) =
    unbox (Sys.opaque_identity (box #(#1L, #2.5, s, 3, #4L)))
  in
  assert (Int64.equal (box_int64 a) 1L);
  assert (Float.equal (box_float b) 2.5);
  assert (c == s && d = 3);
  assert (Int64.equal (box_int64 e) 4L)
[%%expect{|
|}]
*)

(*
(* Nested unboxed tuples: the nesting is flattened away in the box and must be
   rebuilt by unboxing. *)

let () =
  let #(#(a, b), c) = unbox (box #(#(#42L, s), #2.5)) in
  assert (Int64.equal (box_int64 a) 42L && b == s);
  assert (Float.equal (box_float c) 2.5);
  let #(a, #(b, #(c, d)), e) = unbox (box #(s, #(#1L, #(2, #3.25s)), #4l)) in
  assert (a == s && Int64.equal (box_int64 b) 1L && c = 2);
  assert (box_float32 d = box_float32 #3.25s);
  assert (Int32.equal (box_int32 e) 4l);
  let #(#(a, b), #(c, d)) = unbox (box #(#(1, #2L), #(#3L, 4))) in
  assert (a = 1 && Int64.equal (box_int64 b) 2L);
  assert (Int64.equal (box_int64 c) 3L && d = 4);
  (* not statically known *)
  let #(#(a, b), c) = unbox (Sys.opaque_identity (box #(#(#42L, s), #2.5))) in
  assert (Int64.equal (box_int64 a) 42L && b == s);
  assert (Float.equal (box_float c) 2.5);
  let #(a, #(b, #(c, d)), e) =
    unbox (Sys.opaque_identity (box #(s, #(#1L, #(2, #3.25s)), #4l)))
  in
  assert (a == s && Int64.equal (box_int64 b) 1L && c = 2);
  assert (box_float32 d = box_float32 #3.25s);
  assert (Int32.equal (box_int32 e) 4l);
  let #(#(a, b), #(c, d)) =
    unbox (Sys.opaque_identity (box #(#(1, #2L), #(#3L, 4))))
  in
  assert (a = 1 && Int64.equal (box_int64 b) 2L);
  assert (Int64.equal (box_int64 c) 3L && d = 4)
[%%expect{|
|}]
*)

(* Unboxed records. [t# box = t], so we can also unbox a directly constructed
   record. *)

type p_flat_first = { c : int64_u; d : string }
type p_value_first = { e : string; f : int64_u }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u }
type p_nested = { n1 : #(int64_u * string); n2 : float# }
type ur = { u1 : int64_u; u2 : string; u3 : int }

let () =
  let #{ c; d } = unbox (box #{ c = #42L; d = s }) in
  assert (Int64.equal (box_int64 c) 42L && d == s);
  let #{ e; f } = unbox (box #{ e = s; f = #42L }) in
  assert (e == s && Int64.equal (box_int64 f) 42L);
  let #{ g; h; k; l; m } = unbox (box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L }) in
  assert (Int64.equal (box_int64 g) 1L);
  assert (Float.equal (box_float h) 2.5);
  assert (k == s && l = 3);
  assert (Int64.equal (box_int64 m) 4L);
  let #{ n1 = #(n1a, n1b); n2 } = unbox (box #{ n1 = #(#42L, s); n2 = #2.5 }) in
  assert (Int64.equal (box_int64 n1a) 42L && n1b == s);
  assert (Float.equal (box_float n2) 2.5);
  let #{ u1; u2; u3 } = unbox (box #{ u1 = #42L; u2 = s; u3 = 7 }) in
  assert (Int64.equal (box_int64 u1) 42L && u2 == s && u3 = 7);
  (* not statically known *)
  let #{ c; d } = unbox (Sys.opaque_identity (box #{ c = #42L; d = s })) in
  assert (Int64.equal (box_int64 c) 42L && d == s);
  let #{ e; f } = unbox (Sys.opaque_identity (box #{ e = s; f = #42L })) in
  assert (e == s && Int64.equal (box_int64 f) 42L);
  let #{ g; h; k; l; m } =
    unbox (Sys.opaque_identity (box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L }))
  in
  assert (Int64.equal (box_int64 g) 1L);
  assert (Float.equal (box_float h) 2.5);
  assert (k == s && l = 3);
  assert (Int64.equal (box_int64 m) 4L);
  let #{ n1 = #(n1a, n1b); n2 } =
    unbox (Sys.opaque_identity (box #{ n1 = #(#42L, s); n2 = #2.5 }))
  in
  assert (Int64.equal (box_int64 n1a) 42L && n1b == s);
  assert (Float.equal (box_float n2) 2.5)
[%%expect{|
type p_flat_first = { c : int64_u; d : string; }
type p_value_first = { e : string; f : int64_u; }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u; }
type p_nested = { n1 : #(int64_u * string); n2 : float#; }
type ur = { u1 : int64_u; u2 : string; u3 : int; }
|}]

(* Unboxing a directly constructed record *)

let () =
  let r : p_many = { g = #7L; h = #0.5; k = s; l = 9; m = #8L } in
  let #{ g; h; k; l; m } = unbox r in
  assert (Int64.equal (box_int64 g) 7L);
  assert (Float.equal (box_float h) 0.5);
  assert (k == s && l = 9);
  assert (Int64.equal (box_int64 m) 8L);
  let r : p_nested = { n1 = #(#6L, s); n2 = #1.25 } in
  let #{ n1 = #(n1a, n1b); n2 } = unbox r in
  assert (Int64.equal (box_int64 n1a) 6L && n1b == s);
  assert (Float.equal (box_float n2) 1.25);
  (* not statically known *)
  let r : p_many = Sys.opaque_identity { g = #7L; h = #0.5; k = s; l = 9; m = #8L } in
  let #{ g; h; k; l; m } = unbox r in
  assert (Int64.equal (box_int64 g) 7L);
  assert (Float.equal (box_float h) 0.5);
  assert (k == s && l = 9);
  assert (Int64.equal (box_int64 m) 8L);
  let r : ur = Sys.opaque_identity { u1 = #5L; u2 = s; u3 = 6 } in
  let #{ u1; u2; u3 } = unbox r in
  assert (Int64.equal (box_int64 u1) 5L && u2 == s && u3 = 6)
[%%expect{|
|}]

(* Unboxed records nested inside unboxed records and tuples *)

type inner_u = #{ ix : int64_u; iy : string }
type outer_u = #{ o1 : inner_u; o2 : float#; o3 : int }
type wrap_u = #{ w1 : int; w2 : #(inner_u * int32_u); w3 : string }

let () =
  let #{ o1 = #{ ix; iy }; o2; o3 } =
    unbox (box #{ o1 = #{ ix = #1L; iy = s }; o2 = #2.5; o3 = 3 })
  in
  assert (Int64.equal (box_int64 ix) 1L && iy == s);
  assert (Float.equal (box_float o2) 2.5 && o3 = 3);
  let #{ w1; w2 = #(#{ ix; iy }, w2b); w3 } =
    unbox (box #{ w1 = 1; w2 = #(#{ ix = #2L; iy = s }, #3l); w3 = s })
  in
  assert (w1 = 1 && Int64.equal (box_int64 ix) 2L && iy == s);
  assert (Int32.equal (box_int32 w2b) 3l && w3 == s);
  (* CR zeisbach: mixed tuple, enable after rebasing onto mixed tuples
  let #(#{ ix; iy }, b) = unbox (box #(#{ ix = #4L; iy = s }, #5.5)) in
  assert (Int64.equal (box_int64 ix) 4L && iy == s);
  assert (Float.equal (box_float b) 5.5);
  *)
  (* not statically known *)
  let #{ o1 = #{ ix; iy }; o2; o3 } =
    unbox
      (Sys.opaque_identity
         (box #{ o1 = #{ ix = #1L; iy = s }; o2 = #2.5; o3 = 3 }))
  in
  assert (Int64.equal (box_int64 ix) 1L && iy == s);
  assert (Float.equal (box_float o2) 2.5 && o3 = 3);
  let #{ w1; w2 = #(#{ ix; iy }, w2b); w3 } =
    unbox
      (Sys.opaque_identity
         (box #{ w1 = 1; w2 = #(#{ ix = #2L; iy = s }, #3l); w3 = s }))
  in
  assert (w1 = 1 && Int64.equal (box_int64 ix) 2L && iy == s);
  assert (Int32.equal (box_int32 w2b) 3l && w3 == s)
  (* CR zeisbach: mixed tuple, enable after rebasing onto mixed tuples
  ;
  let #(#{ ix; iy }, b) =
    unbox (Sys.opaque_identity (box #(#{ ix = #4L; iy = s }, #5.5)))
  in
  assert (Int64.equal (box_int64 ix) 4L && iy == s);
  assert (Float.equal (box_float b) 5.5)
  *)
[%%expect{|
type inner_u = #{ ix : int64_u; iy : string; }
type outer_u = #{ o1 : inner_u; o2 : float#; o3 : int; }
type wrap_u = #{ w1 : int; w2 : #(inner_u * int32_u); w3 : string; }
|}]

(* Records with narrow data and deep nesting *)

type narrow =
  { na : int8#; nb : string; nc : int16#; nd : float32_u; ne : int32_u;
    nf : int#; ng : nativeint_u }
type deep = { d1 : string; d2 : #(int64_u * #(int * float32_u)); d3 : int32_u }

let () =
  let #{ na; nb; nc; nd; ne; nf; ng } =
    unbox (box #{ na = #42s; nb = s; nc = #42S; nd = #3.25s; ne = #42l;
                  nf = untag_int 42; ng = #42n })
  in
  assert (int8_equal na #42s && nb == s && int16_equal nc #42S);
  assert (box_float32 nd = box_float32 #3.25s);
  assert (Int32.equal (box_int32 ne) 42l);
  assert (int_equal nf (untag_int 42));
  assert (Nativeint.equal (box_nativeint ng) 42n);
  let #{ d1; d2 = #(d2a, #(d2b, d2c)); d3 } =
    unbox (box #{ d1 = s; d2 = #(#1L, #(2, #3.25s)); d3 = #4l })
  in
  assert (d1 == s && Int64.equal (box_int64 d2a) 1L && d2b = 2);
  assert (box_float32 d2c = box_float32 #3.25s);
  assert (Int32.equal (box_int32 d3) 4l);
  (* not statically known *)
  let #{ na; nb; nc; nd; ne; nf; ng } =
    unbox
      (Sys.opaque_identity
         (box #{ na = #42s; nb = s; nc = #42S; nd = #3.25s; ne = #42l;
                 nf = untag_int 42; ng = #42n }))
  in
  assert (int8_equal na #42s && nb == s && int16_equal nc #42S);
  assert (box_float32 nd = box_float32 #3.25s);
  assert (Int32.equal (box_int32 ne) 42l);
  assert (int_equal nf (untag_int 42));
  assert (Nativeint.equal (box_nativeint ng) 42n);
  let #{ d1; d2 = #(d2a, #(d2b, d2c)); d3 } =
    unbox
      (Sys.opaque_identity (box #{ d1 = s; d2 = #(#1L, #(2, #3.25s)); d3 = #4l }))
  in
  assert (d1 == s && Int64.equal (box_int64 d2a) 1L && d2b = 2);
  assert (box_float32 d2c = box_float32 #3.25s);
  assert (Int32.equal (box_int32 d3) 4l);
[%%expect{|
type narrow = {
  na : int8#;
  nb : string;
  nc : int16#;
  nd : float32_u;
  ne : int32_u;
  nf : int#;
  ng : nativeint_u;
}
type deep = {
  d1 : string;
  d2 : #(int64_u * #(int * float32_u));
  d3 : int32_u;
}
|}]

(* Void components contribute no fields to the box and no data to the unboxed
   result. *)

type all_void = { x : unit#; kept : unit# }
type void_mixed = { v1 : #(unit# * int64_u); v2 : string; v3 : unit# }

let () =
  let #{ x = _; kept = _ } = unbox (box #{ x = #(); kept = #() }) in
  (* not statically known *)
  let #{ v1 = #(_, a); v2 = b; v3 = _ } =
    unbox (Sys.opaque_identity (box #{ v1 = #(#(), #1L); v2 = s; v3 = #() }))
  in
  assert (Int64.equal (box_int64 a) 1L && b == s);
  let #{ x = _; kept = _ } =
    unbox (Sys.opaque_identity (box #{ x = #(); kept = #() }))
  in
  let r : all_void = Sys.opaque_identity { x = #(); kept = #() } in
  let #{ x = _; kept = _ } = unbox r in
  ()
[%%expect{|
type all_void = { x : unit#; kept : unit#; }
type void_mixed = { v1 : #(unit# * int64_u); v2 : string; v3 : unit#; }
|}]

(* Local allocation. Unboxing a local box yields local components. *)

let () =
  let local_ b = box #(42, s) in
  let #(a, c) = unbox b in
  assert (a = 42 && c == s);
  let local_ r = box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L } in
  let #{ g; h; k; l; m } = unbox r in
  assert (Int64.equal (box_int64 g) 1L);
  assert (Float.equal (box_float h) 2.5);
  assert (k == s && l = 3);
  assert (Int64.equal (box_int64 m) 4L)
  (* CR zeisbach: mixed tuples, enable after rebasing onto mixed tuples
  ;
  let local_ b = box #(#1L, s) in
  let #(a, c) = unbox b in
  assert (Int64.equal (box_int64 a) 1L && c == s);
  let local_ n = box #(s, #(#2L, #(3, #4.5s))) in
  let #(a, #(c, #(d, e))) = unbox n in
  assert (a == s && Int64.equal (box_int64 c) 2L && d = 3);
  assert (box_float32 e = box_float32 #4.5s)
  *)
[%%expect{|
|}]
