(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 { expect; }
 { expect.opt; }
*)

type ('a : any) t =
  | None
  | Some of 'a

[%%expect{|
type ('a : any) t = None | Some of 'a
|}]

let poly_ map_or f y = function
  | None -> y
  | Some x -> f x
[%%expect{|
val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b = <lpoly>
|}]

module _ : sig
  val map_or : int
end = struct
  let poly_ map_or f y = function
    | None -> y
    | Some x -> f x
end
[%%expect{|
Lines 3-7, characters 6-3:
3 | ......struct
4 |   let poly_ map_or f y = function
5 |     | None -> y
6 |     | Some x -> f x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b end
       is not included in
         sig val map_or : int end
       Values do not match:
         val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b
       is not included in
         val map_or : int
       The type "('a -> 'b) -> 'b -> 'a t -> 'b"
       is not compatible with the type "int"
|}]

type ('a : any) r = { x : 'a }
[%%expect{|
type ('a : any) r = { x : 'a; }
|}]

(* A layout-polymorphic field is maybe-void, so this record is not known to
   contain a runtime value. *)
let poly_ mk v = { x = v }
[%%expect{|
Line 1, characters 17-26:
1 | let poly_ mk v = { x = v }
                     ^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

type ('a : any) r2 = { x : 'a; y : int }
[%%expect{|
type ('a : any) r2 = { x : 'a; y : int; }
|}]

(* [x]'s layout is unknown, but [y] is certainly a runtime value. *)
let poly_ mk2 v = { x = v; y = 1 }
[%%expect{|
val poly_ mk2 : 'a -> 'a r2 = <lpoly>
|}]

external box_float : float# -> float = "%box_float"
external box_int64 : int64_u -> int64 = "%box_int64"
[%%expect{|
external box_float : float# -> float = "%box_float"
external box_int64 : int64_u -> int64 = "%box_int64"
|}]

let () =
  let poly_ map_or f y = function None -> y | Some x -> f x in
  let poly_ wrap x = Some x in
  assert (map_or String.length 0 (wrap "abc") = 3);
  assert (map_or box_float 0.0 (wrap #2.5) = 2.5);
  assert (map_or box_int64 0L (wrap #42L) = 42L);
  assert (map_or (fun #() -> 7) 0 (wrap #()) = 7);
  assert (map_or (fun #(x, y) -> x + y) 0 (wrap #(2, 3)) = 5);
  assert (map_or box_float 1.0 None = 1.0)
[%%expect{|
|}]

(* Multi-argument constructors: a layout-polymorphic argument between a value
   and a flat field, and two layout-polymorphic arguments in one constructor. *)

type ('a : any) triple = T of int * 'a * float#
type ('a : any, 'b : any) both = Both of 'a * int * 'b
[%%expect{|
type ('a : any) triple = T of int * 'a * float#
type ('a : any, 'b : any) both = Both of 'a * int * 'b
|}]

let () =
  let poly_ mk i x = T (i, x, #1.5) in
  let poly_ get (T (_, x, _)) = x in
  let poly_ sum (T (i, _, f)) = float_of_int i +. box_float f in
  assert (get (mk 1 "abc") = "abc");
  assert (box_float (get (mk 2 #2.5)) = 2.5);
  assert (sum (mk 3 #()) = 4.5);
  let #(x, y) = get (mk 4 #(7, #8.0)) in
  assert (x = 7 && box_float y = 8.0)
[%%expect{|
|}]

let () =
  let poly_ swap (Both (a, n, b)) = Both (b, n + 1, a) in
  let poly_ fst (Both (a, _, _)) = a in
  let poly_ snd (Both (_, _, b)) = b in
  let poly_ tag (Both (_, n, _)) = n in
  let x = swap (Both (#2.5, 1, "abc")) in
  assert (fst x = "abc" && box_float (snd x) = 2.5 && tag x = 2);
  let y = swap (Both (#(), 3, #42L)) in
  assert (box_int64 (fst y) = 42L && tag y = 4);
  let #() = snd y in
  ()
[%%expect{|
|}]

let () =
  let poly_ mk x = { x; y = 1 } in
  let poly_ get_x r = r.x in
  let poly_ unpack { x; y } = #(x, y) in
  let poly_ update_x r x = { r with x } in
  let poly_ update_y r y = { r with y } in
  assert (get_x (mk "abc") = "abc");
  assert (box_float (get_x (mk #2.5)) = 2.5);
  let #(x, y) = unpack (mk #42L) in
  assert (box_int64 x = 42L && y = 1);
  let #() = get_x (mk #()) in
  let #(x, y) = get_x (update_x (mk #(1, #2.5)) #(3, #4.5)) in
  assert (x = 3 && box_float y = 4.5);
  let r = mk #(1, #2.5) in
  let #(#(x, y), tag) = unpack (update_y r 7) in
  assert (x = 1 && box_float y = 2.5 && tag = 7 && r.y = 1);
  let #(#(), tag) = unpack (update_y (mk #()) 8) in
  assert (tag = 8)
[%%expect{|
|}]

type ('a : any) inlined = I of { mutable payload : 'a; tag : int }
[%%expect{|
type ('a : any) inlined = I of { mutable payload : 'a; tag : int; }
|}]

let () =
  let poly_ make_inline payload = I { payload; tag = 1 } in
  let poly_ get_inline (I r) = r.payload in
  let poly_ set_inline (I r) payload = r.payload <- payload in
  let poly_ copy_inline (I r) = I { r with tag = r.tag + 1 } in
  let r = make_inline #2.5 in
  set_inline r #4.5;
  assert (box_float (get_inline (copy_inline r)) = 4.5);
  let r = make_inline #(3, "abc") in
  set_inline r #(4, "def");
  let #(x, y) = get_inline r in
  assert (x = 4 && y = "def");
  let #() = get_inline (copy_inline (make_inline #())) in
  ()
[%%expect{|
|}]

(* Reading through a block index. *)

external get_int_idx : 'a -> ('a, int) idx_imm -> int = "%get_idx_imm"
[%%expect{|
external get_int_idx : 'a -> ('a, int) idx_imm -> int = "%get_idx_imm"
|}]

let () =
  let poly_ read_y r = get_int_idx r (.y) in
  assert (read_y { x = #2.5; y = 7 } = 7);
  assert (read_y { x = #(1, 2); y = 8 } = 8);
  assert (read_y { x = #(); y = 9 } = 9)
[%%expect{|
|}]

type ('a : any) atomic_record =
  { mutable count : int [@atomic]; payload : 'a }
[%%expect{|
type ('a : any) atomic_record = {
  mutable count : int [@atomic];
  payload : 'a;
}
|}]

(* Reading/writing an atomic field. *)

let () =
  let poly_ update r = r.count <- r.count + 1 in
  let r = { count = 1; payload = #2.5 } in
  update r;
  assert (r.count = 2);
  let r = { count = 3; payload = #() } in
  update r;
  assert (r.count = 4)
[%%expect{|
|}]


type ('a : any) node = { payload : 'a; next : 'a node }
[%%expect{|
type ('a : any) node = { payload : 'a; next : 'a node; }
|}]

(* Recursive preallocation still requires a concrete size before evaluation. *)
let poly_ cycle payload =
  let rec node = { payload; next = node } in
  node
[%%expect{|
Line 2, characters 17-41:
2 |   let rec node = { payload; next = node } in
                     ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Recursive definitions of layout-polymorphic blocks are not currently supported.
|}]

type ('a : any) pair = #('a * 'a)
type ('a : any) fields_256 = 'a pair pair pair pair pair pair pair pair
type ('a : any) large_record = { fields : 'a fields_256; flat : float# }
[%%expect{|
type ('a : any) pair = #('a * 'a)
type ('a : any) fields_256 = 'a pair pair pair pair pair pair pair pair
type ('a : any) large_record = { fields : 'a fields_256; flat : float#; }
|}]

(* This record has too many value fields. *)
let set_flat_value (r : string large_record) =
  let poly_ set_flat r = { r with flat = #2.0 } in
  set_flat r
[%%expect{|
Line 2, characters 25-47:
2 |   let poly_ set_flat r = { r with flat = #2.0 } in
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: Mixed blocks may contain at most 254 value fields prior to the flat suffix, but this one contains 256.
|}]

(* This one doesn't have any value fields.*)
let set_flat_void (r : unit# large_record) =
  let poly_ set_flat r = { r with flat = #2.0 } in
  set_flat r
[%%expect{|
val set_flat_void : unit# large_record -> unit# large_record = <fun>
|}]

type ('a : any) gap_record =
  { padding : 'a fields_256 pair; target : #(string * float#) }
[%%expect{|
type ('a : any) gap_record = {
  padding : 'a fields_256 pair;
  target : #(string * float#);
}
|}]

(* [float#] padding creates a gap that is too large to store in a block index. *)
let float_index () : (float# gap_record, #(string * float#)) idx_imm =
  let poly_ target_index () = (.target) in
  target_index ()
[%%expect{|
Line 2, characters 30-39:
2 |   let poly_ target_index () = (.target) in
                                  ^^^^^^^^^
Error: This block index cannot be created because it refers to values
       and non-values that are separated by 2^12 or more bytes in their
       block, or could be deepened to such an index.
|}]

(* [void] padding creates no such gap. *)
let void_index () : (unit# gap_record, #(string * float#)) idx_imm =
  let poly_ target_index () = (.target) in
  target_index ()
[%%expect{|
val void_index : unit -> (unit# gap_record, #(string * float#)) idx_imm =
  <fun>
|}]
