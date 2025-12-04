(* TEST
 reference = "${test_source_directory}/null_ptr_access.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 modules = "ptr_of_value.c";
 flambda2;
 arch_amd64;
 native;
*)

(* Test [%unsafe_set_ptr] and [%unsafe_get_ptr] when the base is NULL, and the
   byte offset is a pointer something external. *)

open Stdlib_stable
open Stdlib_upstream_compatible

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) @@ portable =
    "%box_int64" [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> t @@ portable =
    "%unbox_int64" [@@warning "-187"]

  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

type nothing = |

external get_ptr_local
  : ('a : any).
  #(nothing or_null * int64#) @ local -> 'a @ local
  = "%unsafe_get_ptr"
[@@layout_poly]

external set_ptr_local
  : ('a : any).
  #(nothing or_null * int64#) @ local -> 'a @ local -> unit
  = "%unsafe_set_ptr"
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
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_int_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 1: int field (value type)";
  let pt = stack_ { x = 10; y = 20 } in
  let i : int = get_int_field pt in
  Printf.printf "  get: expected 20, got %d\n" i;
  set_int_field pt 200;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %d)\n" pt.x pt.y;
  (* keep [pt] live until this point *)
  let (_ : pt_int) = Sys.opaque_identity pt in
  ()

(******************************************************)
(* Test 2: float# field (flat type, not GC-scannable) *)

type pt_float = { x : int; mutable y : float# }

let[@inline never] set_float_field pt (y : float#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_float_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 2: float# field (flat type)";
  let pt = stack_ { x = 10; y = #20.5 } in
  let f : float# = get_float_field pt in
  Printf.printf "  get: expected 20.5, got %f\n" (Float_u.to_float f);
  set_float_field pt #200.5;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200.5), got (%d, %f)\n"
    pt.x (Float_u.to_float pt.y);
  let (_ : pt_float) = Sys.opaque_identity pt in
  ()

(********************************************************)
(* Test 3: float32# field (flat type, not GC-scannable) *)

type pt_float32 = { x : int; mutable y : float32# }

let[@inline never] set_float32_field pt (y : float32#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_float32_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 3: float32# field (flat type)";
  let pt = stack_ { x = 10; y = #20.5s } in
  let f : float32# = get_float32_field pt in
  Printf.printf "  get: expected 20.5, got %f\n"
    (Float_u.to_float (Float32_u.to_float f));
  set_float32_field pt #200.5s;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200.5), got (%d, %f)\n"
    pt.x (Float_u.to_float (Float32_u.to_float pt.y));
  let (_ : pt_float32) = Sys.opaque_identity pt in
  ()

(******************************************************)
(* Test 4: int32# field (flat type, not GC-scannable) *)

type pt_int32 = { x : int; mutable y : int32# }

let[@inline never] set_int32_field pt (y : int32#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_int32_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 4: int32# field (flat type)";
  let pt = stack_ { x = 10; y = #20l } in
  let i : int32# = get_int32_field pt in
  Printf.printf "  get: expected 20, got %ld\n" (Int32_u.to_int32 i);
  set_int32_field pt #200l;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %ld)\n"
    pt.x (Int32_u.to_int32 pt.y);
  let (_ : pt_int32) = Sys.opaque_identity pt in
  ()

(******************************************************)
(* Test 5: int64# field (flat type, not GC-scannable) *)

type pt_int64 = { x : int; mutable y : int64# }

let[@inline never] set_int64_field pt (y : int64#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_int64_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 5: int64# field (flat type)";
  let pt = stack_ { x = 10; y = #20L } in
  let i : int64# = get_int64_field pt in
  Printf.printf "  get: expected 20, got %Ld\n" (Int64_u.to_int64 i);
  set_int64_field pt #200L;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %Ld)\n"
    pt.x (Int64_u.to_int64 pt.y);
  let (_ : pt_int64) = Sys.opaque_identity pt in
  ()

(**********************************************************)
(* Test 6: nativeint# field (flat type, not GC-scannable) *)

type pt_nativeint = { x : int; mutable y : nativeint# }

let[@inline never] set_nativeint_field pt (y : nativeint#) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_nativeint_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 6: nativeint# field (flat type)";
  let pt = stack_ { x = 10; y = #20n } in
  let i : nativeint# = get_nativeint_field pt in
  Printf.printf "  get: expected 20, got %nd\n" (Nativeint_u.to_nativeint i);
  set_nativeint_field pt #200n;
  let pt = Sys.opaque_identity pt in
  Printf.printf "  set: expected (10, 200), got (%d, %nd)\n"
    pt.x (Nativeint_u.to_nativeint pt.y);
  let (_ : pt_nativeint) = Sys.opaque_identity pt in
  ()

(***************************************************************)
(* Test 7: unboxed product of ints (value types, GC-scannable) *)

type int_pair = #{ a : int; b : int }
type pt_int_pair = { x : int; mutable y : int_pair }

let[@inline never] set_int_pair_field pt (y : int_pair) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_int_pair_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

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
  let (_ : pt_int_pair) = Sys.opaque_identity pt in
  ()

(**************************************************)
(* Test 8: unboxed product of floats (flat types) *)

type float_pair = #{ a : float#; b : float# }
type pt_float_pair = { x : int; mutable y : float_pair }

let[@inline never] set_float_pair_field pt (y : float_pair) =
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  set_ptr_local #(Null, y_addr) y

let[@inline never] get_float_pair_field pt =
  exclave_
  let y_addr = Int64_u.add (addr_of_value pt) #8L in
  get_ptr_local #(Null, y_addr)

let () =
  print_endline "Test 8: unboxed product of floats (flat types)";
  let pt = stack_ { x = 10; y = #{ a = #20.5; b = #30.5 } } in
  let #{ a; b } : float_pair = get_float_pair_field pt in
  Printf.printf "  get: expected (20.5, 30.5), got (%f, %f)\n"
    (Float_u.to_float a) (Float_u.to_float b);
  set_float_pair_field pt #{ a = #200.5; b = #300.5 };
  let pt = Sys.opaque_identity pt in
  let #{ a; b } = pt.y in
  Printf.printf "  set: expected (10, (200.5, 300.5)), got (%d, (%f, %f))\n"
    pt.x (Float_u.to_float a) (Float_u.to_float b);
  let (_ : pt_float_pair) = Sys.opaque_identity pt in
  ()
