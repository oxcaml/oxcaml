(* TEST
 include stdlib_stable;
 flags = "-extension layouts_alpha";
 flambda2;
 { expect; expect.opt; }
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

(* Each test runs twice: once with the box statically known, so that the
   middle end may fold the load away, and once with the box hidden behind
   [Sys.opaque_identity], so that the load really happens. *)
type hide = { hide : ('a : value_or_null). 'a -> 'a }

let both_ways (test : hide -> unit) =
  test { hide = (fun x -> x) };
  test { hide = Sys.opaque_identity }
[%%expect{|
type hide = { hide : 'a. 'a -> 'a; }
val both_ways : (hide -> unit) -> unit = <fun>
|}]

external untag_int : int -> int# = "%int#_of_int"
external eq_int : int# -> int# -> bool = "%int#_equal"
external eq_i8 : int8# -> int8# -> bool = "%int8#_equal"
external eq_i16 : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external unbox_int32 : int32 -> int32_u = "%unbox_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"

let eq_f64 x y = Float.equal (box_float x) (box_float y)
let eq_f32 x y = box_float32 x = box_float32 y
let eq_i32 x y = Int32.equal (box_int32 x) (box_int32 y)
let eq_i64 x y = Int64.equal (box_int64 x) (box_int64 y)
let eq_n x y = Nativeint.equal (box_nativeint x) (box_nativeint y)
[%%expect{|
external untag_int : int -> int# = "%int#_of_int"
external eq_int : int# -> int# -> bool = "%int#_equal"
external eq_i8 : int8# -> int8# -> bool = "%int8#_equal"
external eq_i16 : int16# -> int16# -> bool = "%int16#_equal"
external box_float : float# -> float = "%box_float"
external box_float32 : float32_u -> float32 = "%box_float32"
external box_int32 : int32_u -> int32 = "%box_int32"
external unbox_int32 : int32 -> int32_u = "%unbox_int32"
external box_int64 : int64_u -> int64 = "%box_int64"
external box_nativeint : nativeint_u -> nativeint = "%box_nativeint"
val eq_f64 : float# -> float# -> bool = <fun>
val eq_f32 : float32_u -> float32_u -> bool = <fun>
val eq_i32 : int32_u -> int32_u -> bool = <fun>
val eq_i64 : int64_u -> int64_u -> bool = <fun>
val eq_n : nativeint_u -> nativeint_u -> bool = <fun>
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

let check_value { hide } x = assert (unbox (hide (box x)) == x)

let () = both_ways (fun h ->
  check_value h 42;
  check_value h s;
  check_value h 3.25;
  check_value h (Some s);
  check_value h { mx = #1L; my = s };
  check_value h { wp = #(#2L, s); wq = 3 };
  check_value h { af = #1.5; ag = #4l };
  check_value h { fr1 = 1.5; fr2 = 2.5 };
  check_value h (A (#5L, s));
  check_value h B;
  check_value h (C #(#6.5, 7)))
[%%expect{|
type mixed = { mx : int64_u; my : string; }
type with_product = { wp : #(int64_u * string); wq : int; }
type all_flat = { af : float#; ag : int32_u; }
type float_record = { fr1 : float; fr2 : float; }
type variant = A of int64_u * string | B | C of #(float# * int)
val check_value : hide -> 'a -> unit = <fun>
|}]

(* Unboxed numbers, both whole-word and narrower than a word *)

let () = both_ways (fun { hide } ->
  assert (eq_f64 (unbox (hide (box #3.25))) #3.25);
  assert (eq_i64 (unbox (hide (box #42L))) #42L);
  assert (eq_n (unbox (hide (box #42n))) #42n);
  assert (eq_int (unbox (hide (box (untag_int 42)))) (untag_int 42));
  assert (eq_f32 (unbox (hide (box #3.25s))) #3.25s);
  assert (eq_i32 (unbox (hide (box #42l))) #42l);
  assert (eq_i8 (unbox (hide (box #42s))) #42s);
  assert (eq_i16 (unbox (hide (box #42S))) #42S))
[%%expect{|
|}]

type addressed_i8 = #{ addressed_i8 : int8# }
type inherited_i8 = #{ inherit inherited_i8 : int8# }
type addressed_f64 = #{ addressed_f64 : float# }
type inherited_f64 = #{ inherit inherited_f64 : float# }
type addressed_void = #{ addressed_void : unit# }

let () = both_ways (fun { hide } ->
  let #{ addressed_i8 } = unbox (hide (box #{ addressed_i8 = -#42s })) in
  assert (eq_i8 addressed_i8 (-#42s));
  let #{ inherited_i8 } = unbox (hide (box #{ inherited_i8 = -#42s })) in
  assert (eq_i8 inherited_i8 (-#42s));
  let #{ addressed_f64 } = unbox (hide (box #{ addressed_f64 = #3.25 })) in
  assert (eq_f64 addressed_f64 #3.25);
  let #{ inherited_f64 } = unbox (hide (box #{ inherited_f64 = #3.25 })) in
  assert (eq_f64 inherited_f64 #3.25);
  let #{ addressed_void = _ } = unbox (hide (box #{ addressed_void = #() })) in
  let #() = unbox (hide (box #())) in
  ())
[%%expect{|
type addressed_i8 = #{ addressed_i8 : int8#; }
type inherited_i8 = #{ inherit inherited_i8 : int8#; }
type addressed_f64 = #{ addressed_f64 : float#; }
type inherited_f64 = #{ inherit inherited_f64 : float#; }
type addressed_void = #{ addressed_void : unit#; }
|}]

let () = both_ways (fun { hide } ->
  List.iter (fun bits ->
    let x = Stdlib_stable.Float32_u.of_float32
        (Stdlib_stable.Float32.of_bits bits) in
    let y = unbox (hide (box x)) in
    assert (Int32.equal
      (Stdlib_stable.Float32.to_bits (box_float32 y)) bits))
    [0l; 0x80000000l; 0x7fc00001l; 0x7f800000l; 0xff800000l];
  List.iter (fun x ->
    let y = unbox_int32 x in
    assert (eq_i32 (unbox (hide (box y))) y))
    [Int32.min_int; Int32.max_int; -1l; 0l])
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
  assert (eq_f64 (unbox f) #3.25);
  let local_ i = box #42L in
  assert (eq_i64 (unbox i) #42L);
  let local_ n = box #42s in
  assert (eq_i8 (unbox n) #42s)
[%%expect{|
|}]

(* Unboxed products. The box is laid out like a record with one field per
   component, with values moved to a prefix and flat data to a suffix, so
   unboxing must undo that permutation. *)

(* Unboxed tuples of values only *)

let () = both_ways (fun { hide } ->
  let #(a, b) = unbox (hide (box #(42, s))) in
  assert (a = 42 && b == s);
  let o = Some s in
  let #(a, b, c) = unbox (hide (box #(s, o, 7))) in
  assert (a == s && b == o && c = 7))
[%%expect{|
|}]

(* Unboxed tuples mixing values and flat data, in various orders *)

let () = both_ways (fun { hide } ->
  let #(a, b) = unbox (hide (box #(#42L, s))) in
  assert (eq_i64 a #42L && b == s);
  let #(a, b) = unbox (hide (box #(s, #42L))) in
  assert (a == s && eq_i64 b #42L);
  let #(a, b, c, d, e) = unbox (hide (box #(#1L, #2.5, s, 3, #4L))) in
  assert (eq_i64 a #1L && eq_f64 b #2.5 && c == s && d = 3 && eq_i64 e #4L))
[%%expect{|
|}]

(* Flat data narrower than a word inside products *)

let () = both_ways (fun { hide } ->
  let #(a, b, c, d, e) = unbox (hide (box #(#42s, s, #42S, #3.25s, #42l))) in
  assert (eq_i8 a #42s && b == s && eq_i16 c #42S && eq_f32 d #3.25s
          && eq_i32 e #42l);
  let #(a, b, c) = unbox (hide (box #(untag_int 42, #42n, s))) in
  assert (eq_int a (untag_int 42) && eq_n b #42n && c == s))
[%%expect{|
|}]

(* Nested unboxed tuples: the nesting is flattened away in the box and must be
   rebuilt by unboxing. *)

let () = both_ways (fun { hide } ->
  let #(#(a, b), c) = unbox (hide (box #(#(#42L, s), #2.5))) in
  assert (eq_i64 a #42L && b == s && eq_f64 c #2.5);
  let #(a, #(b, #(c, d)), e) =
    unbox (hide (box #(s, #(#1L, #(2, #3.25s)), #4l)))
  in
  assert (a == s && eq_i64 b #1L && c = 2 && eq_f32 d #3.25s && eq_i32 e #4l);
  let #(#(a, b), #(c, d)) = unbox (hide (box #(#(1, #2L), #(#3L, 4)))) in
  assert (a = 1 && eq_i64 b #2L && eq_i64 c #3L && d = 4))
[%%expect{|
|}]

(* Unboxed records. [t# box = t], so we can also unbox a directly constructed
   record. *)

type p_flat_first = { c : int64_u; d : string }
type p_value_first = { e : string; f : int64_u }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u }
type p_nested = { n1 : #(int64_u * string); n2 : float# }
type ur = { u1 : int64_u; u2 : string; u3 : int }

let () = both_ways (fun { hide } ->
  let #{ c; d } = unbox (hide (box #{ c = #42L; d = s })) in
  assert (eq_i64 c #42L && d == s);
  let #{ e; f } = unbox (hide (box #{ e = s; f = #42L })) in
  assert (e == s && eq_i64 f #42L);
  let #{ g; h; k; l; m } =
    unbox (hide (box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L }))
  in
  assert (eq_i64 g #1L && eq_f64 h #2.5 && k == s && l = 3 && eq_i64 m #4L);
  let #{ n1 = #(n1a, n1b); n2 } =
    unbox (hide (box #{ n1 = #(#42L, s); n2 = #2.5 }))
  in
  assert (eq_i64 n1a #42L && n1b == s && eq_f64 n2 #2.5);
  let #{ u1; u2; u3 } = unbox (hide (box #{ u1 = #42L; u2 = s; u3 = 7 })) in
  assert (eq_i64 u1 #42L && u2 == s && u3 = 7))
[%%expect{|
type p_flat_first = { c : int64_u; d : string; }
type p_value_first = { e : string; f : int64_u; }
type p_many = { g : int64_u; h : float#; k : string; l : int; m : int64_u; }
type p_nested = { n1 : #(int64_u * string); n2 : float#; }
type ur = { u1 : int64_u; u2 : string; u3 : int; }
|}]

(* Unboxing a directly constructed record *)

let () = both_ways (fun { hide } ->
  let #{ g; h; k; l; m } =
    unbox (hide { g = #7L; h = #0.5; k = s; l = 9; m = #8L })
  in
  assert (eq_i64 g #7L && eq_f64 h #0.5 && k == s && l = 9 && eq_i64 m #8L);
  let #{ n1 = #(n1a, n1b); n2 } = unbox (hide { n1 = #(#6L, s); n2 = #1.25 }) in
  assert (eq_i64 n1a #6L && n1b == s && eq_f64 n2 #1.25);
  let #{ u1; u2; u3 } = unbox (hide { u1 = #5L; u2 = s; u3 = 6 }) in
  assert (eq_i64 u1 #5L && u2 == s && u3 = 6))
[%%expect{|
|}]

(* Unboxed records nested inside unboxed records and tuples *)

type inner_u = #{ ix : int64_u; iy : string }
type outer_u = #{ o1 : inner_u; o2 : float#; o3 : int }
type wrap_u = #{ w1 : int; w2 : #(inner_u * int32_u); w3 : string }

let () = both_ways (fun { hide } ->
  let #{ o1 = #{ ix; iy }; o2; o3 } =
    unbox (hide (box #{ o1 = #{ ix = #1L; iy = s }; o2 = #2.5; o3 = 3 }))
  in
  assert (eq_i64 ix #1L && iy == s && eq_f64 o2 #2.5 && o3 = 3);
  let #{ w1; w2 = #(#{ ix; iy }, w2b); w3 } =
    unbox (hide (box #{ w1 = 1; w2 = #(#{ ix = #2L; iy = s }, #3l); w3 = s }))
  in
  assert (w1 = 1 && eq_i64 ix #2L && iy == s && eq_i32 w2b #3l && w3 == s);
  let #(#{ ix; iy }, b) = unbox (hide (box #(#{ ix = #4L; iy = s }, #5.5))) in
  assert (eq_i64 ix #4L && iy == s && eq_f64 b #5.5)
  )
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
type two = { t1 : #(int * int64_u); t2 : #(int64_u * int) }

let () = both_ways (fun { hide } ->
  let #{ na; nb; nc; nd; ne; nf; ng } =
    unbox (hide (box #{ na = #42s; nb = s; nc = #42S; nd = #3.25s; ne = #42l;
                        nf = untag_int 42; ng = #42n }))
  in
  assert (eq_i8 na #42s && nb == s && eq_i16 nc #42S && eq_f32 nd #3.25s
          && eq_i32 ne #42l && eq_int nf (untag_int 42) && eq_n ng #42n);
  let #{ d1; d2 = #(d2a, #(d2b, d2c)); d3 } =
    unbox (hide (box #{ d1 = s; d2 = #(#1L, #(2, #3.25s)); d3 = #4l }))
  in
  assert (d1 == s && eq_i64 d2a #1L && d2b = 2 && eq_f32 d2c #3.25s
          && eq_i32 d3 #4l);
  let #{ t1 = #(t1a, t1b); t2 = #(t2a, t2b) } =
    unbox (hide (box #{ t1 = #(1, #2L); t2 = #(#3L, 4) }))
  in
  assert (t1a = 1 && eq_i64 t1b #2L && eq_i64 t2a #3L && t2b = 4))
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
type two = { t1 : #(int * int64_u); t2 : #(int64_u * int); }
|}]

(* Void components contribute no fields to the box and no data to the unboxed
   result. *)

type void_mixed = { v1 : #(unit# * int64_u); v2 : string; v3 : unit# }

let () = both_ways (fun { hide } ->
  let #{ v1 = #(_, a); v2 = b; v3 = _ } =
    unbox (hide (box #{ v1 = #(#(), #1L); v2 = s; v3 = #() }))
  in
  assert (eq_i64 a #1L && b == s);
  let #(_, _) = unbox (hide (box #(#(), #()))) in
  let #(_, _) = unbox (hide (#(), #())) in
  let #(#(_, a), b, _) = unbox (hide (box #(#(#(), #1L), s, #()))) in
  assert (eq_i64 a #1L && b == s)
  )
[%%expect{|
type void_mixed = { v1 : #(unit# * int64_u); v2 : string; v3 : unit#; }
|}]

(* Local allocation. Unboxing a local box yields local components. *)

let () =
  let local_ b = box #(42, s) in
  let #(a, c) = unbox b in
  assert (a = 42 && c == s);
  let local_ r = box #{ g = #1L; h = #2.5; k = s; l = 3; m = #4L } in
  let #{ g; h; k; l; m } = unbox r in
  assert (eq_i64 g #1L && eq_f64 h #2.5 && k == s && l = 3 && eq_i64 m #4L);
  let local_ b = box #(#1L, s) in
  let #(a, c) = unbox b in
  assert (eq_i64 a #1L && c == s);
  let local_ n = box #(s, #(#2L, #(3, #4.5s))) in
  let #(a, #(c, #(d, e))) = unbox n in
  assert (a == s && eq_i64 c #2L && d = 3 && eq_f32 e #4.5s)
[%%expect{|
|}]

(* Mutability. If the loads were [Immutable], the second [unbox] would be
   rewritten to reuse the first and miss the write in between. *)

type mut = { mutable c : int64_u; d : string }

let read_write_read (r : mut) =
  let #{ c = before; d = _ } = unbox r in
  r.c <- #2L;
  let #{ c = after; d = _ } = unbox r in
  assert (eq_i64 before #1L && eq_i64 after #2L)

let () = read_write_read (Sys.opaque_identity { c = #1L; d = s })
[%%expect{|
type mut = { mutable c : int64_u; d : string; }
val read_write_read : mut -> unit = <fun>
|}]

(* Abstraction. The client only sees [M.u], an abstract type of layout
   [value & value], so it cannot tell that [u box] is a record with a mutable
   field. *)
module M : sig
  type u : value & value
  val make : int -> int -> u
  val c : u -> int
  val set_c : u box -> int -> unit
end = struct
  type t = { mutable c : int; d : int }
  type u = t#
  let make c d = #{ c; d }
  let c (#{ c; d = _ } : u) = c
  (* [u box] does not reduce to [t] through the alias, so annotate with [t] *)
  let set_c (r : t) v = r.c <- v
end

let () =
  let r = box (M.make 1 0) in
  let before = M.c (unbox r) in
  M.set_c r 2;
  let after = M.c (unbox r) in
  assert (before = 1 && after = 2)
[%%expect{|
module M :
  sig
    type u : value & value
    val make : int -> int -> u
    val c : u -> int
    val set_c : u box -> int -> unit
  end
|}]

(* Aliasing. In bytecode unboxed products are blocks, so [unbox] must copy
   deeply: *)

type inner = #{ ix : int; iy : int }
type outer = { mutable u : inner; tag : int }

let () =
  let r : outer = { u = #{ ix = 1; iy = 2 }; tag = 7 } in
  (* A round trip through [unbox] and [box] must not share the nested product
     with [r]. *)
  let copy : outer = box (unbox r) in
  Stdlib_stable.Idx_mut.set copy (.u.#ix) 10;
  assert (copy.u.#ix = 10 && r.u.#ix = 1);
  (* The unboxed value must not observe later writes to the record, whether
     into the nested product or replacing it wholesale. *)
  let un = unbox r in
  Stdlib_stable.Idx_mut.set r (.u.#iy) 20;
  assert (r.u.#iy = 20 && un.#u.#iy = 2);
  r.u <- #{ ix = 30; iy = 40 };
  assert (r.u.#ix = 30 && un.#u.#ix = 1 && un.#tag = 7)
[%%expect{|
type inner = #{ ix : int; iy : int; }
type outer = { mutable u : inner; tag : int; }
|}]

module Abs : sig
  type t : (value & float64) box
  val make : int -> float# -> t
  val get : t -> int * float
end = struct
  type t = { i : int; f : float# }
  let make i f = { i; f }
  let get { i; f } = i, box_float f
end
[%%expect{|
module Abs :
  sig
    type t : (value & float64) box
    val make : int -> float# -> t
    val get : t -> int * float
  end
|}]

let unbox_abs : Abs.t -> Abs.t# = unbox
let box_abs : Abs.t# -> Abs.t = box

let () =
  let original = Abs.make 42 #3.25 in
  let contents = unbox_abs (Sys.opaque_identity original) in
  let copy = box_abs contents in
  assert (Abs.get copy = (42, 3.25))
[%%expect{|
val unbox_abs : Abs.t -> Abs.t# = <fun>
val box_abs : Abs.t# -> Abs.t = <fun>
|}]
