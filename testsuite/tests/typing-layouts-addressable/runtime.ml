(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 flags = "-extension layouts_beta";
 {
   native;
 } {
   bytecode;
 }
*)
(* CR-soon rtjoa: remove layouts_beta once records-containing-any is stable *)

(* This just tests that values at [addressable] kinds still act the same at
   runtime. Not much interesting is going on here yet, as addressability does
   not (yet) affect boxed representations. *)

module Float_u = Stdlib_upstream_compatible.Float_u

type inherited_float : float64 = #{ inherit x : float# }

let[@inline never] inherit_float x = #{ x }

let () =
  let r = inherit_float #2.5 in
  assert (Float_u.to_float r.#x = 2.5);
  let #{ x } = r in
  assert (Float_u.to_float x = 2.5)

(* We cannot inhabit a made-addressable kind organically, but we can via
   magic; addressability does not change the runtime representation. *)
module F64a : sig
  type t : float64 addressable

  val of_float_u : float# -> t

  val to_float_u : t -> float#
end = struct
  type t : float64 addressable

  external magic_to : ('a : any) ('b : any addressable). 'a -> 'b
    = "%identity"
    [@@layout_poly]

  external magic_of : ('a : any addressable) ('b : any). 'a -> 'b
    = "%identity"
    [@@layout_poly]

  let of_float_u (x : float#) : t = magic_to x

  let to_float_u (x : t) : float# = magic_of x
end

let check (x : F64a.t) v = assert (Float_u.to_float (F64a.to_float_u x) = v)

(* Round-trip *)
let () = check (F64a.of_float_u #3.25) 3.25

(* Through a function call *)
let[@inline never] through (x : F64a.t) = x

let () = check (through (F64a.of_float_u #4.5)) 4.5

(* As a record field at kind [float64 addressable] *)
type ('a : float64 addressable) r = { fa : 'a; s : string }

let () =
  let r = { fa = F64a.of_float_u #5.75; s = "s" } in
  check r.fa 5.75;
  assert (String.equal r.s "s")

(* As a variant field *)
type v = V of F64a.t * string

let () =
  match V (F64a.of_float_u #6.5, "v") with
  | V (x, s) ->
    check x 6.5;
    assert (String.equal s "v")

(* As an unboxed record field expecting [any], then [any addressable] *)
type ('a : any) ur_any = #{ a : 'a; s : string }

let () =
  let u : F64a.t ur_any = #{ a = F64a.of_float_u #7.25; s = "u" } in
  check u.#a 7.25;
  assert (String.equal u.#s "u")

type ('a : any addressable) ur_addr = #{ a : 'a; s : string }

let () =
  let u : F64a.t ur_addr = #{ a = F64a.of_float_u #8.25; s = "u" } in
  check u.#a 8.25;
  assert (String.equal u.#s "u")

(* In unboxed tuples with one, then two, made-addressable components *)
let () =
  let #(a, s) = #(F64a.of_float_u #9.5, "b") in
  check a 9.5;
  assert (String.equal s "b")

let () =
  let #(a, b) = #(F64a.of_float_u #10.5, F64a.of_float_u #11.5) in
  check a 10.5;
  check b 11.5

external box : ('a : any). 'a -> 'a box = "%box" [@@layout_poly]
external unbox : ('a : any). 'a box -> 'a = "%unbox" [@@layout_poly]
external equal_i8 : int8# -> int8# -> bool = "%int8#_equal"
external equal_i64 : int64_u -> int64_u -> bool = "%int64#_equal"

type inherited_boxed_float : float64 box = { inherit bf : float# }
type inherited_boxed_bits8 : bits8 box = { inherit bi : int8# }
type inherited_boxed_void : void box = { inherit bv : unit# }
type inherited_boxed_bits64 : bits64 box = { inherit bl : int64_u }
type inherited_boxed_value = { inherit bs : string }
type inherited_boxed_addressable = { inherit ba : F64a.t }
type inherited_boxed_last = { mutable head : int; inherit tail : float# }

let[@inline never] make_inherited_float bf = { bf }
let[@inline never] make_inherited_bits8 bi = { bi }
let[@inline never] make_inherited_void bv = { bv }
let[@inline never] make_inherited_bits64 bl = { bl }
let[@inline never] make_inherited_value bs = { bs }
let[@inline never] make_inherited_addressable ba = { ba }
let[@inline never] make_inherited_last head tail = { head; tail }

let check_inherited_float r expected =
  assert (Obj.tag (Obj.repr r) = Obj.double_tag);
  assert (Float_u.to_float r.bf = expected);
  let { bf } = r in
  assert (Float_u.to_float bf = expected);
  let u : inherited_boxed_float# = unbox r in
  assert (Float_u.to_float u.#bf = expected)

let () =
  check_inherited_float (make_inherited_float #3.25) 3.25;
  check_inherited_float (box #{ bf = #4.5 }) 4.5;
  List.iter
    (fun r ->
      assert (Obj.is_int (Obj.repr r));
      assert (equal_i8 r.bi (-#42s));
      let { bi } = r in
      assert (equal_i8 bi (-#42s));
      let u : inherited_boxed_bits8# = unbox r in
      assert (equal_i8 u.#bi (-#42s)))
    [make_inherited_bits8 (-#42s); box #{ bi = -#42s }];
  let r = make_inherited_void #() in
  assert (Obj.is_int (Obj.repr r));
  let { bv = #() } = r in
  let u : inherited_boxed_void# = unbox r in
  let #() = u.#bv in
  assert (Obj.is_int (Obj.repr (box #{ bv = #() })))

let () =
  let r = make_inherited_bits64 #42L in
  assert (Obj.tag (Obj.repr r) = 0);
  assert (equal_i64 r.bl #42L);
  let { bl } = r in
  assert (equal_i64 bl #42L);
  let s = String.make 3 'x' in
  let r = make_inherited_value s in
  assert (Obj.tag (Obj.repr r) = 0);
  assert (r.bs == s);
  let r = make_inherited_addressable (F64a.of_float_u #5.25) in
  assert (Obj.tag (Obj.repr r) = 0);
  check r.ba 5.25;
  let { ba } = r in
  check ba 5.25

let () =
  let r = make_inherited_last 42 #6.25 in
  assert (Obj.tag (Obj.repr r) = 0);
  assert (r.head = 42 && Float_u.to_float r.tail = 6.25);
  let { head; tail } = r in
  assert (head = 42 && Float_u.to_float tail = 6.25);
  r.head <- 43;
  let r = { r with tail = #7.5 } in
  assert (r.head = 43 && Float_u.to_float r.tail = 7.5);
  let r : inherited_boxed_last = box #{ head = 44; tail = #8.75 } in
  assert (r.head = 44 && Float_u.to_float r.tail = 8.75);
  let u : inherited_boxed_last# = unbox r in
  assert (u.#head = 44 && Float_u.to_float u.#tail = 8.75)

external get_imm_idx :
  ('a : any) ('b : any). 'a box -> ('a, 'b) idx_imm -> 'b
  = "%get_idx_imm" [@@layout_poly]
external get_mut_idx :
  ('a : any) ('b : any). 'a box -> ('a, 'b) idx_mut -> 'b
  = "%get_idx" [@@layout_poly]
external compose_imm_idx :
  ('a : any) ('b : any) ('c : any).
  ('a, 'b) idx_imm -> ('b, 'c) idx_imm -> ('a, 'c) idx_imm
  = "%idx_compose"
external compose_mut_imm_idx :
  ('a : any) ('b : any) ('c : any).
  ('a, 'b) idx_mut -> ('b, 'c) idx_imm -> ('a, 'c) idx_mut
  = "%idx_compose"

type inherited_payload = { pf : float#; ps : string }
type inherited_product = { inherit payload : inherited_payload# }
type inherited_product_holder = {
  mutable product : inherited_product#;
  after : string;
}

let[@inline never] make_inherited_product payload = { payload }

let check_inherited_payload p =
  assert (Float_u.to_float p.#pf = 12.5);
  assert (p.#ps = "payload")

let () =
  let p = #{ pf = #12.5; ps = "payload" } in
  let r = make_inherited_product p in
  check_inherited_payload r.payload;
  let { payload } = r in
  check_inherited_payload payload;
  check_inherited_payload (get_imm_idx r (.payload));
  assert (get_imm_idx r (.payload.#ps) = "payload");
  let ps = compose_imm_idx (.payload) (.ps) in
  assert (get_imm_idx r ps = "payload");
  let h = { product = #{ payload = p }; after = "after" } in
  let payload = compose_mut_imm_idx (.product) (.payload) in
  check_inherited_payload (get_mut_idx h payload);
  let ps = compose_mut_imm_idx (.product) ps in
  assert (get_mut_idx h ps = "payload");
  assert (h.after = "after")

let[@inline never] inherited_float_array_get
    (a : inherited_boxed_float array) i = a.(i)

let () =
  let a = Array.make 2 (make_inherited_float #1.25) in
  check_inherited_float (inherited_float_array_get a 0) 1.25;
  a.(1) <- make_inherited_float #2.5;
  check_inherited_float (inherited_float_array_get a 1) 2.5;
  check_inherited_float (inherited_float_array_get a 0) 1.25
