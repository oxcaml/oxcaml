(* TEST
 reference = "${test_source_directory}/ext_ptr_access.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 modules = "ptr_of_value.c";
 flambda2;
 native;
*)

(* Test [%unsafe_get_ext_ptr] and [%unsafe_set_ext_ptr], which take only the
   byte offset and behave as if the base were NULL (i.e. the offset is a pointer
   to something external). This mirrors [null_ptr_access.ml], which exercises the
   same behaviour through [%unsafe_get_ptr]/[%unsafe_set_ptr] with a NULL base. *)

open Stdlib_stable
open Stdlib_upstream_compatible

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) @@ portable =
    "%box_int64"

  external of_int64 : (int64[@local_opt]) -> t @@ portable =
    "%unbox_int64"

  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

external get_ext_ptr
  : ('a : any).
  int64# @ local -> 'a @ local
  = "%unsafe_get_ext_ptr"
[@@layout_poly]

external set_ext_ptr
  : ('a : any).
  int64# @ local -> 'a @ local -> unit
  = "%unsafe_set_ext_ptr"
[@@layout_poly]

external addr_of_value
  : ('a : value_or_null).
  'a @ local -> int64#
  = "" "caml_native_pointer_of_value"

(************************************************)
(* Test 1: int field (value type, GC-scannable) *)

type pt_int = { x : int; mutable y : int }

let[@inline never] set_int_field pt y =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_int_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 1: int field (value type)";
  let pt = stack_ { x = 10; y = 20 } in
  let i : int = get_int_field pt in
  Printf.printf "  get: expected 20, got %d\n" i;
  set_int_field pt 200;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %d)\n" pt.x pt.y;
  ()

(******************************************************)
(* Test 2: float# field (flat type, not GC-scannable) *)

type pt_float = { x : int; mutable y : float# }

let[@inline never] set_float_field pt (y : float#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_float_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 2: float# field (flat type)";
  let pt = stack_ { x = 10; y = #20.5 } in
  let f : float# = get_float_field pt in
  Printf.printf "  get: expected 20.5, got %.1f\n" (Float_u.to_float f);
  set_float_field pt #200.5;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200.5), got (%d, %.1f)\n"
    pt.x (Float_u.to_float pt.y);
  ()

(********************************************************)
(* Test 3: float32# field (flat type, not GC-scannable) *)

type pt_float32 = { x : int; mutable y : float32# }

let[@inline never] set_float32_field pt (y : float32#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_float32_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 3: float32# field (flat type)";
  let pt = stack_ { x = 10; y = #20.5s } in
  let f : float32# = get_float32_field pt in
  Printf.printf "  get: expected 20.5, got %.1f\n"
    (Float_u.to_float (Float32_u.to_float f));
  set_float32_field pt #200.5s;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200.5), got (%d, %.1f)\n"
    pt.x (Float_u.to_float (Float32_u.to_float pt.y));
  ()

(******************************************************)
(* Test 4: int32# field (flat type, not GC-scannable) *)

type pt_int32 = { x : int; mutable y : int32# }

let[@inline never] set_int32_field pt (y : int32#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_int32_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 4: int32# field (flat type)";
  let pt = stack_ { x = 10; y = #20l } in
  let i : int32# = get_int32_field pt in
  Printf.printf "  get: expected 20, got %ld\n" (Int32_u.to_int32 i);
  set_int32_field pt #200l;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %ld)\n"
    pt.x (Int32_u.to_int32 pt.y);
  ()

(******************************************************)
(* Test 5: int64# field (flat type, not GC-scannable) *)

type pt_int64 = { x : int; mutable y : int64# }

let[@inline never] set_int64_field pt (y : int64#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_int64_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 5: int64# field (flat type)";
  let pt = stack_ { x = 10; y = #20L } in
  let i : int64# = get_int64_field pt in
  Printf.printf "  get: expected 20, got %Ld\n" (Int64_u.to_int64 i);
  set_int64_field pt #200L;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %Ld)\n"
    pt.x (Int64_u.to_int64 pt.y);
  ()

(**********************************************************)
(* Test 6: nativeint# field (flat type, not GC-scannable) *)

type pt_nativeint = { x : int; mutable y : nativeint# }

let[@inline never] set_nativeint_field pt (y : nativeint#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_nativeint_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 6: nativeint# field (flat type)";
  let pt = stack_ { x = 10; y = #20n } in
  let i : nativeint# = get_nativeint_field pt in
  Printf.printf "  get: expected 20, got %nd\n" (Nativeint_u.to_nativeint i);
  set_nativeint_field pt #200n;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %nd)\n"
    pt.x (Nativeint_u.to_nativeint pt.y);
  ()

(***************************************************************)
(* Test 7: unboxed product of ints (value types, GC-scannable) *)

type int_pair = #{ a : int; b : int }
type pt_int_pair = { x : int; mutable y : int_pair }

let[@inline never] set_int_pair_field pt (y : int_pair) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_int_pair_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 7: unboxed product of ints (value types)";
  let pt = stack_ { x = 10; y = #{ a = 20; b = 30 } } in
  let #{ a; b } : int_pair = get_int_pair_field pt in
  Printf.printf "  get: expected (20, 30), got (%d, %d)\n" a b;
  set_int_pair_field pt #{ a = 200; b = 300 };
  let pt = Sys.opaque_identity pt in
  let #{ a; b } = pt.y in
  Printf.printf "  set: expected (10, (200, 300)), got (%d, (%d, %d))\n"
    pt.x a b;
  ()

(**************************************************)
(* Test 8: unboxed product of floats (flat types) *)

type float_pair = #{ a : float#; b : float# }
type pt_float_pair = { x : int; mutable y : float_pair }

let[@inline never] set_float_pair_field pt (y : float_pair) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ext_ptr y_addr y

let[@inline never] get_float_pair_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ext_ptr y_addr

let () =
  print_endline "Test 8: unboxed product of floats (flat types)";
  let pt = stack_ { x = 10; y = #{ a = #20.5; b = #30.5 } } in
  let #{ a; b } : float_pair = get_float_pair_field pt in
  Printf.printf "  get: expected (20.5, 30.5), got (%.1f, %.1f)\n"
    (Float_u.to_float a) (Float_u.to_float b);
  set_float_pair_field pt #{ a = #200.5; b = #300.5 };
  let pt = Sys.opaque_identity pt in
  let #{ a; b } = pt.y in
  Printf.printf "  set: expected (10, (200.5, 300.5)), got (%d, (%.1f, %.1f))\n"
    pt.x (Float_u.to_float a) (Float_u.to_float b);
  ()

(***********************************************************************)
(* Test 9: embedded mixed product, exercising a NONZERO gap.          *)
(*                                                                    *)
(* A block index for a mixed (value + flat) product packs a "gap" in  *)
(* its top 12 bits and a byte offset in its low 52 bits. When the     *)
(* product is embedded in a larger block, its value and flat halves   *)
(* are physically separated, so the gap is nonzero.                   *)
(*                                                                    *)
(* An ext pointer can still address such a field: a block index is    *)
(* [bits64], so we reinterpret it to [int64#] and add the record's    *)
(* base address. On conventional targets the address's top 12 bits    *)
(* are zero, so the addition only changes the low 52 offset bits and  *)
(* leaves the gap intact -- the codegen zeroes the gap to form the    *)
(* value offset and re-adds it for the flat. The test asserts that    *)
(* architectural assumption (and that the gap is genuinely nonzero).  *)

type vf = #{ v : string; f : int64# }
type pt_vf_pair = { mutable a : vf; mutable b : vf }

external idx_to_int64 : (pt_vf_pair, vf) idx_mut -> int64# = "%obj_magic"

(* Global (non-local) variants: the read value is a real heap object, so we can
   return it globally and print the string field. *)
external get_ext_ptr_g : ('a : any). int64# -> 'a = "%unsafe_get_ext_ptr"
[@@layout_poly]

external set_ext_ptr_g : ('a : any). int64# -> 'a -> unit = "%unsafe_set_ext_ptr"
[@@layout_poly]

let gap_of idx =
  Int64.shift_right_logical (Int64_u.to_int64 (idx_to_int64 idx)) 52

let[@inline never] get_vf_field pt idx =
  let addr = Int64_u.add (addr_of_value pt) (idx_to_int64 idx) in
  get_ext_ptr_g addr

let[@inline never] set_vf_field pt idx (y : vf) =
  let addr = Int64_u.add (addr_of_value pt) (idx_to_int64 idx) in
  set_ext_ptr_g addr y

let () =
  print_endline "Test 9: embedded mixed product (nonzero gap)";
  let pt = { a = #{ v = "a"; f = #1L }; b = #{ v = "b"; f = #2L } } in
  (* The construction requires the record's address to fit in the low 52 bits,
     i.e. its top 12 bits must be zero. *)
  assert (
    Int64.shift_right_logical (Int64_u.to_int64 (addr_of_value pt)) 52 = 0L);
  (* Both fields genuinely have a nonzero gap. *)
  assert (gap_of (.a) <> 0L);
  assert (gap_of (.b) <> 0L);
  let #{ v; f } : vf = get_vf_field pt (.b) in
  Printf.printf "  get b: expected (b, 2), got (%s, %Ld)\n"
    v (Int64_u.to_int64 f);
  set_vf_field pt (.b) #{ v = "z"; f = #9L };
  let pt = Sys.opaque_identity pt in
  let #{ v; f } = pt.b in
  Printf.printf "  set b: expected (z, 9), got (%s, %Ld)\n"
    v (Int64_u.to_int64 f);
  let #{ v; f } : vf = get_vf_field pt (.a) in
  Printf.printf "  get a: expected (a, 1), got (%s, %Ld)\n"
    v (Int64_u.to_int64 f);
  set_vf_field pt (.a) #{ v = "y"; f = #8L };
  let pt = Sys.opaque_identity pt in
  let #{ v; f } = pt.a in
  Printf.printf "  set a: expected (y, 8), got (%s, %Ld)\n"
    v (Int64_u.to_int64 f);
  ()
