(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 expect;
*)

(* A block whose only field is an unboxed product stores the fields of that
   product directly, rather than a pointer to a separately allocated product.
   This is needed to maintain the boxing invariant. As an example, the types
   [{ p : #(int * int) }] and [{ x : int; y : int }] both have layout
   [(value & value) box]; these tests check that they are represented
   identically, and that operations on them work as expected. This test only
   runs in bytecode, as the native toplevel prints unboxed product fields as
   [<abstr>]. *)

open Stdlib_stable
module Float_u = Stdlib_upstream_compatible.Float_u

external[@layout_poly] get_ptr :
  'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b = "%unsafe_get_ptr"

external[@layout_poly] set_ptr :
  'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b -> unit = "%unsafe_set_ptr"

let size x = Obj.size (Obj.repr x)

type pair = #{ a : int; b : int }
[%%expect{|
module Float_u = Stdlib_upstream_compatible.Float_u
external get_ptr : 'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b
  = "%unsafe_get_ptr" [@@layout_poly]
external set_ptr : 'a ('b : any). #('a * ('a, 'b) idx_mut) -> 'b -> unit
  = "%unsafe_set_ptr" [@@layout_poly]
val size : 'a -> int = <fun>
type pair = #{ a : int; b : int; }
|}]

(* Records *)

type tup = { t : #(int * string) }

let r = { t = #(1, "one") }

let r_size = size r

let r_t =
  let #(i, s) = r.t in
  i, s
[%%expect{|
type tup = { t : #(int * string); }
val r : tup = {t = #(1, "one")}
val r_size : int = 2
val r_t : int * string = (1, "one")
|}]

type rp = { mutable p : pair }

(* The products are not literals, so their fields must be read out of them *)
let r = { p = Sys.opaque_identity #{ a = 1; b = 2 } }

let r_size = size r

let r_p =
  r.p <- Sys.opaque_identity #{ a = 3; b = 4 };
  r.p.#a, r.p.#b
[%%expect{|
type rp = { mutable p : pair; }
val r : rp = {p = #{a = 1; b = 2}}
val r_size : int = 2
val r_p : int * int = (3, 4)
|}]

type nest = #{ inner : pair; c : int }

type rn = { mutable n : nest }

(* Only the outer product is flattened: [inner] is still boxed *)
let r = { n = #{ inner = Sys.opaque_identity #{ a = 1; b = 2 }; c = 3 } }

let r_size = size r

let r_n_initial = r.n.#inner.#a, r.n.#inner.#b, r.n.#c

let deepened =
  let i : (rn, pair) idx_mut = (.n.#inner) in
  Idx_mut.set r (.idx_mut(i).#b) 20;
  (* The field access is compiled to [Getfield]s, whereas the index is a
     runtime path followed in C; we check that they agree. *)
  r.n.#inner.#b, Idx_mut.get r (.idx_mut(i).#b)

let r_n =
  r.n <- #{ inner = #{ a = 4; b = 5 }; c = 6 };
  r.n.#inner.#a, r.n.#inner.#b, r.n.#c
[%%expect{|
type nest = #{ inner : pair; c : int; }
type rn = { mutable n : nest; }
val r : rn = {n = #{inner = <unboxed product>; c = 3}}
val r_size : int = 2
val r_n_initial : int * int * int = (1, 2, 3)
val deepened : int * int = (20, 20)
val r_n : int * int * int = (4, 5, 6)
|}]

type mixed = #{ x : int; u : float# }

type rm = { mutable m : mixed }

let r = { m = #{ x = 1; u = #2.0 } }

let r_size = size r

let r_m_initial = r.m.#x, Float_u.to_float r.m.#u

let u_via_idx =
  Idx_mut.set r (.m.#u) #3.0;
  Float_u.to_float (Idx_mut.get r (.m.#u))

let r_m =
  r.m <- #{ x = 4; u = #5.0 };
  r.m.#x, Float_u.to_float r.m.#u

let m_via_idx =
  Idx_mut.set r (.m) #{ x = 6; u = #7.0 };
  let m = Idx_mut.get r (.m) in
  m.#x, Float_u.to_float m.#u
[%%expect{|
type mixed = #{ x : int; u : float#; }
type rm = { mutable m : mixed; }
val r : rm = {m = #{x = 1; u = <abstr>}}
val r_size : int = 2
val r_m_initial : int * float = (1, 2.)
val u_via_idx : float = 3.
val r_m : int * float = (4, 5.)
val m_via_idx : int * float = (6, 7.)
|}]

(* Void is not an unboxed product, so a block whose only field is void is not
   flattened: the void keeps its slot, like any other field in bytecode. *)

type ru = { mutable e : unit# }

let r = { e = #() }

let r_size = size r

let r_size_after_set =
  r.e <- #();
  let { e = #() } = r in
  size r
[%%expect{|
type ru = { mutable e : unit#; }
val r : ru = {e = <void>}
val r_size : int = 1
val r_size_after_set : int = 1
|}]

(* A one-field unboxed record is represented like its field, so wrapping the
   product in any number of them does not change the layout. *)

type ('a : any) wrap = #{ w : 'a }

type rwp = { mutable wp : pair wrap wrap wrap }

let mk_wp a b = #{ w = #{ w = #{ w = #{ a; b } } } }

let read_wp r =
  let p = r.wp.#w.#w.#w in
  p.#a, p.#b

let r = { wp = Sys.opaque_identity (mk_wp 1 2) }

let r_size = size r

let r_initial = read_wp r

let r_after_set =
  r.wp <- mk_wp 3 4;
  read_wp r
[%%expect{|
type ('a : any) wrap = #{ w : 'a; }
type rwp = { mutable wp : pair wrap wrap wrap; }
val mk_wp : int -> int -> pair wrap wrap wrap = <fun>
val read_wp : rwp -> int * int = <fun>
val r : rwp = {wp = #{w = <unknown>}}
val r_size : int = 2
val r_initial : int * int = (1, 2)
val r_after_set : int * int = (3, 4)
|}]

(* The wrappers add nothing to the path, so this index refers to the whole
   block *)
let whole () : (rwp, pair wrap wrap) idx_mut = (.wp.#w)

let after_idx_set =
  Idx_mut.set r (whole ()) (mk_wp 5 6).#w;
  read_wp r

let via_idx_get =
  let p = (Idx_mut.get r (whole ())).#w.#w in
  p.#a, p.#b

let after_inner_idx_set =
  Idx_mut.set r (.wp.#w.#w.#w.#a) 7;
  read_wp r

let after_deepened_idx_set =
  Idx_mut.set r (.idx_mut(whole ()).#w.#w.#b) 8;
  read_wp r
[%%expect{|
val whole : unit -> (rwp, pair wrap wrap) idx_mut = <fun>
val after_idx_set : int * int = (5, 6)
val via_idx_get : int * int = (5, 6)
val after_inner_idx_set : int * int = (7, 6)
val after_deepened_idx_set : int * int = (7, 8)
|}]

(* Block indices to the whole product, whose runtime path is empty *)

let idx_whole_product =
  let r = { p = #{ a = 1; b = 2 } } in
  let i : (rp, pair) idx_mut = (.p) in
  let v = Idx_mut.get r i in
  Idx_mut.set r i #{ a = 3; b = 4 };
  let after_set = r.p.#a, r.p.#b in
  (* Deepening an index with an empty runtime path *)
  Idx_mut.set r (.idx_mut(i).#b) 40;
  let after_deepened_set = r.p.#b, Idx_mut.get r (.idx_mut(i).#b) in
  (* Pointers built from such an index *)
  let ptr = #(r, i) in
  set_ptr ptr #{ a = 5; b = 6 };
  let via_ptr = get_ptr ptr in
  (v.#a, v.#b), after_set, after_deepened_set, (via_ptr.#a, via_ptr.#b), r
[%%expect{|
val idx_whole_product :
  (int * int) * (int * int) * (int * int) * (int * int) * rp =
  ((1, 2), (3, 4), (40, 40), (5, 6), {p = #{a = 5; b = 6}})
|}]

(* Constructors *)

type v =
  | Const
  | A of #(int * string)
  | B of { f : pair }
  | C of #(int * float#)
  | D of { mutable g : pair }

let a = Sys.opaque_identity (A #(1, "one"))

let b = Sys.opaque_identity (B { f = #{ a = 2; b = 3 } })

let c = Sys.opaque_identity (C #(4, #5.0))

let d = Sys.opaque_identity (D { g = #{ a = 6; b = 7 } })

let sizes = List.map size [a; b; c; d]
[%%expect{|
type v =
    Const
  | A of #(int * string)
  | B of { f : pair; }
  | C of #(int * float#)
  | D of { mutable g : pair; }
val a : v = A <unboxed product>
val b : v = B {f = #{a = 2; b = 3}}
val c : v = C <unboxed product>
val d : v = D {g = #{a = 6; b = 7}}
val sizes : int list = [2; 2; 2; 2]
|}]

let args =
  ( (match a with A #(i, s) -> i, s | _ -> assert false),
    (match b with B { f = #{ a; b } } -> a, b | _ -> assert false),
    (match c with C #(i, f) -> i, Float_u.to_float f | _ -> assert false),
    match d with D { g = #{ a; b } } -> a, b | _ -> assert false )

let d_after_set =
  match d with
  | D r ->
    r.g <- #{ a = 8; b = 9 };
    (r.g.#a, r.g.#b), d
  | _ -> assert false
[%%expect{|
val args : (int * string) * (int * int) * (int * float) * (int * int) =
  ((1, "one"), (2, 3), (4, 5.), (6, 7))
val d_after_set : (int * int) * v = ((8, 9), D {g = #{a = 8; b = 9}})
|}]

type vw = W of #(int * string) wrap wrap

let w = Sys.opaque_identity (W #{ w = #{ w = #(7, "seven") } })

let w_size = size w

let w_arg = match w with W #{ w = #{ w = #(i, s) } } -> i, s
[%%expect{|
type vw = W of #(int * string) wrap wrap
val w : vw = W <unboxed product>
val w_size : int = 2
val w_arg : int * string = (7, "seven")
|}]

(* Recursive definitions, whose sizes are computed by [Value_rec_compiler] *)

type self = { s : self_inner }

and self_inner = #{ me : self; k : int }

type self_mixed = { sm : self_mixed_inner }

and self_mixed_inner = #{ me_m : self_mixed; f : float# }

type cyc = Cyc of #(cyc * int)

let x =
  let rec x = { s = #{ me = x; k = 1 } } in
  x

let x_props = size x, x.s.#me == x, x.s.#k

let y =
  let rec y = { sm = #{ me_m = y; f = #2.0 } } in
  y

let y_props = size y, y.sm.#me_m == y, Float_u.to_float y.sm.#f

let z =
  let rec z = Cyc #(z, 3) in
  z

let z_props = size z, match z with Cyc #(z', i) -> z' == z, i
[%%expect{|
type self = { s : self_inner; }
and self_inner = #{ me : self; k : int; }
type self_mixed = { sm : self_mixed_inner; }
and self_mixed_inner = #{ me_m : self_mixed; f : float#; }
type cyc = Cyc of #(cyc * int)
val x : self = {s = #{me = <cycle>; k = 1}}
val x_props : int * bool * int = (2, true, 1)
val y : self_mixed = {sm = #{me_m = <cycle>; f = <abstr>}}
val y_props : int * bool * float = (2, true, 2.)
val z : cyc = Cyc <unboxed product>
val z_props : int * (bool * int) = (2, (true, 3))
|}]

(* Modules whose only binding is an unboxed product *)

module type S = sig
  val pair : #(int * string)
end

module M : S = struct
  let pair = #(1, "m")
end

module F (X : S) = struct
  let first () =
    let #(i, _) = X.pair in
    i
end

(* Packing a module gives access to its block, to check its size *)
let m_size = size (module M : S)

let m_pair =
  let #(i, s) = M.pair in
  i, s

let fresh =
  (module struct
    let pair = Sys.opaque_identity #(2, "fresh")
  end : S)

let fresh_size = size fresh

let functor_result =
  let module Fresh = F ((val fresh : S)) in
  Fresh.first ()
[%%expect{|
module type S = sig val pair : #(int * string) end
module M : S
module F : functor (X : S) -> sig val first : unit -> int end
val m_size : int = 2
val m_pair : int * string = (1, "m")
val fresh : (module S) = <module>
val fresh_size : int = 2
val functor_result : int = 2
|}]
