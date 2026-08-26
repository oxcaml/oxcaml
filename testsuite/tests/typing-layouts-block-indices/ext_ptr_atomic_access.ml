(* TEST
 reference = "${test_source_directory}/ext_ptr_atomic_access.reference";
 modules = "ptr_of_value.c";
 flambda2;
 native;
*)

(* This file mirrors [ext_ptr_access.ml], which exercises the
   same addressing scheme through the non-atomic external pointer
   primitives. *)

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) @@ portable = "%box_int64"
  external of_int64 : (int64[@local_opt]) -> t @@ portable = "%unbox_int64"

  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

external atomic_load_ext_ptr :
  ('a : value_or_null).
  int64# @ local -> 'a = "%unsafe_atomic_load_ext_ptr"

external atomic_set_ext_ptr :
  ('a : value_or_null).
  (int64#[@local_opt]) -> 'a -> unit = "%unsafe_atomic_set_ext_ptr"

external atomic_exchange_ext_ptr :
  ('a : value_or_null).
  (int64#[@local_opt]) -> 'a -> 'a = "%unsafe_atomic_exchange_ext_ptr"

external atomic_cas_ext_ptr :
  ('a : value_or_null).
  (int64#[@local_opt]) -> 'a -> 'a -> bool = "%unsafe_atomic_cas_ext_ptr"

external atomic_compare_exchange_ext_ptr :
  ('a : value_or_null).
  (int64#[@local_opt]) -> 'a -> 'a -> 'a
  = "%unsafe_atomic_compare_exchange_ext_ptr"

external atomic_fetch_add_ext_ptr :
  int64# @ local -> int -> int = "%unsafe_atomic_fetch_add_ext_ptr"

external atomic_add_ext_ptr :
  int64# @ local -> int -> unit = "%unsafe_atomic_add_ext_ptr"

external atomic_sub_ext_ptr :
  int64# @ local -> int -> unit = "%unsafe_atomic_sub_ext_ptr"

external atomic_land_ext_ptr :
  int64# @ local -> int -> unit = "%unsafe_atomic_land_ext_ptr"

external atomic_lor_ext_ptr :
  int64# @ local -> int -> unit = "%unsafe_atomic_lor_ext_ptr"

external atomic_lxor_ext_ptr :
  int64# @ local -> int -> unit = "%unsafe_atomic_lxor_ext_ptr"

external addr_of_value :
  ('a : value_or_null).
  'a @ local -> int64#
  = "" "caml_native_pointer_of_value"

(*******************************************************)
(* Test 1: int field (immediate), heap-allocated block *)

type pt_int = { x : int; mutable y : int }

(* [y_addr] is recomputed before every access because the GC may move [pt]
   between accesses. *)
let[@inline never] y_addr pt = Int64_u.add (addr_of_value pt) #8L

let () =
  print_endline "Test 1: int field (immediate), heap block";
  let pt = { x = 10; y = 20 } in
  Printf.printf "  load: expected 20, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_set_ext_ptr (y_addr pt) 30;
  Printf.printf "  set 30; load: expected 30, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  Printf.printf "  exchange 40: expected 30, got %d\n"
    (atomic_exchange_ext_ptr (y_addr pt) 40 : int);
  Printf.printf "  cas 40 50: expected true, got %b\n"
    (atomic_cas_ext_ptr (y_addr pt) 40 50);
  Printf.printf "  cas 40 60: expected false, got %b\n"
    (atomic_cas_ext_ptr (y_addr pt) 40 60);
  Printf.printf "  compare_exchange 50 60: expected 50, got %d\n"
    (atomic_compare_exchange_ext_ptr (y_addr pt) 50 60 : int);
  Printf.printf "  compare_exchange 50 70: expected 60, got %d\n"
    (atomic_compare_exchange_ext_ptr (y_addr pt) 50 70 : int);
  Printf.printf "  fetch_add 5: expected 60, got %d\n"
    (atomic_fetch_add_ext_ptr (y_addr pt) 5);
  atomic_add_ext_ptr (y_addr pt) 10;
  Printf.printf "  add 10; load: expected 75, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_sub_ext_ptr (y_addr pt) 25;
  Printf.printf "  sub 25; load: expected 50, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_land_ext_ptr (y_addr pt) 0b011;
  Printf.printf "  land 0b011; load: expected 2, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_lor_ext_ptr (y_addr pt) 0b101;
  Printf.printf "  lor 0b101; load: expected 7, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_lxor_ext_ptr (y_addr pt) 0b110;
  Printf.printf "  lxor 0b110; load: expected 1, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  let pt = Sys.opaque_identity pt in
  Printf.printf "  final: expected (10, 1), got (%d, %d)\n" pt.x pt.y;
  ()

(*******************************************************)
(* Test 2: string field (pointer), heap-allocated block.

   These go through the runtime functions (with a null base), which must skip
   the write barrier. *)

type pt_str = { x : int; mutable y : string }

let () =
  print_endline "Test 2: string field (pointer), heap block";
  let pt = { x = 10; y = "one" } in
  Printf.printf "  load: expected one, got %s\n"
    (atomic_load_ext_ptr (y_addr pt) : string);
  atomic_set_ext_ptr (y_addr pt) "two";
  Printf.printf "  set two; load: expected two, got %s\n"
    (atomic_load_ext_ptr (y_addr pt) : string);
  Printf.printf "  exchange three: expected two, got %s\n"
    (atomic_exchange_ext_ptr (y_addr pt) "three" : string);
  let three : string = atomic_load_ext_ptr (y_addr pt) in
  Printf.printf "  cas three four: expected true, got %b\n"
    (atomic_cas_ext_ptr (y_addr pt) three "four");
  Printf.printf "  cas three five: expected false, got %b\n"
    (atomic_cas_ext_ptr (y_addr pt) three "five");
  let four : string = atomic_load_ext_ptr (y_addr pt) in
  Printf.printf "  compare_exchange four six: expected four, got %s\n"
    (atomic_compare_exchange_ext_ptr (y_addr pt) four "six" : string);
  let pt = Sys.opaque_identity pt in
  Printf.printf "  final: expected (10, six), got (%d, %s)\n" pt.x pt.y;
  ()

(********************************************************)
(* Test 3: int field (immediate), stack-allocated block *)

let () =
  print_endline "Test 3: int field (immediate), stack block";
  let pt : pt_int = stack_ { x = 10; y = 20 } in
  Printf.printf "  load: expected 20, got %d\n"
    (atomic_load_ext_ptr (y_addr pt) : int);
  atomic_set_ext_ptr (y_addr pt) 30;
  Printf.printf "  exchange 40: expected 30, got %d\n"
    (atomic_exchange_ext_ptr (y_addr pt) 40 : int);
  Printf.printf "  fetch_add 5: expected 40, got %d\n"
    (atomic_fetch_add_ext_ptr (y_addr pt) 5);
  let pt = Sys.opaque_identity pt in
  Printf.printf "  final: expected (10, 45), got (%d, %d)\n" pt.x pt.y;
  ()

(*******************************************************)
(* Test 4: string field (pointer), stack-allocated block *)

let () =
  print_endline "Test 4: string field (pointer), stack block";
  let pt = stack_ { x = 10; y = "one" } in
  Printf.printf "  load: expected one, got %s\n"
    (atomic_load_ext_ptr (y_addr pt) : string);
  atomic_set_ext_ptr (y_addr pt) "two";
  Printf.printf "  exchange three: expected two, got %s\n"
    (atomic_exchange_ext_ptr (y_addr pt) "three" : string);
  let three : string = atomic_load_ext_ptr (y_addr pt) in
  Printf.printf "  cas three four: expected true, got %b\n"
    (atomic_cas_ext_ptr (y_addr pt) three "four");
  let pt = Sys.opaque_identity pt in
  (* Read the field back with an atomic load, since a string read from a
     stack-allocated record would be local. *)
  Printf.printf "  final: expected (10, four), got (%d, %s)\n" pt.x
    (atomic_load_ext_ptr (y_addr pt) : string);
  ()

(*******************************************************)
(* Test 5: fields of a mixed block.

   The value fields of a mixed block are reordered before the flat suffix, so
   rather than hard-coding the field offsets we take them from block indices
   (a block index to a single value field is just a byte offset). *)

type pt_mixed = { f : int64#; x : int; mutable y : int; mutable z : string }

external idx_to_int64 :
  ('a : value_or_null) ('b : value_or_null).
  ('a, 'b) idx_mut -> int64# = "%obj_magic"

let[@inline never] field_addr pt idx =
  Int64_u.add (addr_of_value pt) (idx_to_int64 idx)

let () =
  print_endline "Test 5: mixed block (int and string fields)";
  let pt = { f = #42L; x = 10; y = 20; z = "one" } in
  Printf.printf "  load y: expected 20, got %d\n"
    (atomic_load_ext_ptr (field_addr pt (.y)) : int);
  atomic_set_ext_ptr (field_addr pt (.y)) 30;
  Printf.printf "  exchange y 40: expected 30, got %d\n"
    (atomic_exchange_ext_ptr (field_addr pt (.y)) 40 : int);
  Printf.printf "  fetch_add y 5: expected 40, got %d\n"
    (atomic_fetch_add_ext_ptr (field_addr pt (.y)) 5);
  Printf.printf "  load z: expected one, got %s\n"
    (atomic_load_ext_ptr (field_addr pt (.z)) : string);
  atomic_set_ext_ptr (field_addr pt (.z)) "two";
  Printf.printf "  exchange z three: expected two, got %s\n"
    (atomic_exchange_ext_ptr (field_addr pt (.z)) "three" : string);
  let three : string = atomic_load_ext_ptr (field_addr pt (.z)) in
  Printf.printf "  cas z three four: expected true, got %b\n"
    (atomic_cas_ext_ptr (field_addr pt (.z)) three "four");
  let pt = Sys.opaque_identity pt in
  Printf.printf "  final: expected (42, 10, 45, four), got (%Ld, %d, %d, %s)\n"
    (Int64_u.to_int64 pt.f) pt.x pt.y pt.z;
  ()
