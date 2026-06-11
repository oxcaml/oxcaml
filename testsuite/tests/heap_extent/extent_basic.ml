(* TEST
 modules = "extent_stubs.c";
*)

(* Basic single-domain tests of heap extents: creation, liveness,
   field access, partial and complete death, sweeping, freeing, and
   finalisation of custom blocks.

   Extent blocks are only ever bound to locals inside functions, never
   in top-level units: in native code the locals of all top-level
   units share the module initialiser's frame and stay live to the end
   of the program. The [roots] global is the only persistent root. *)

external make_extent : int array -> Obj.t array = "heap_extent_make"
external make_custom_extent : int -> Obj.t array = "heap_extent_make_custom"
external freed_count : unit -> int = "heap_extent_freed_count"
external finalised_count : unit -> int = "heap_extent_finalised_count"

let roots : Obj.t array ref = ref [||]

let gc_until cond =
  let rec go n =
    if not (cond ()) && n > 0 then (Gc.full_major (); go (n - 1))
  in
  go 20

let check msg b =
  Printf.printf "%s: %s\n%!" msg (if b then "ok" else "FAILED")

let expect_freed msg expected =
  gc_until (fun () -> freed_count () = expected);
  let f = freed_count () in
  if f = expected then check msg true
  else Printf.printf "%s: FAILED (freed %d, expected %d)\n%!" msg f expected

let expected_string () = String.init 64 Char.chr

(* First extent: includes a zero-wosize block; fields hold pointers
   into the major heap and into the extent itself. *)

let create_first () = roots := make_extent [| 1; 2; 3; 0; 5 |]

let fill_first () =
  let b = !roots in
  Obj.set_field b.(0) 0 (Obj.repr (expected_string ()));
  Obj.set_field b.(1) 0 b.(3); (* point at the zero-wosize block *)
  Obj.set_field b.(1) 1 b.(4);
  Obj.set_field b.(4) 0 (Obj.repr 12345)

let check_first () =
  let b = !roots in
  check "heap pointer from extent block survives"
    (String.equal (Obj.obj (Obj.field b.(0) 0)) (expected_string ()));
  check "extent-to-extent pointer survives"
    ((Obj.obj (Obj.field (Obj.field b.(1) 1) 0) : int) = 12345)

let keep_only_block1 () = roots := [| !roots.(1) |]

let check_via_extent () =
  let b1 = !roots.(0) in
  check "block kept alive via extent block"
    ((Obj.obj (Obj.field (Obj.field b1 1) 0) : int) = 12345)

let () =
  create_first ();
  Gc.full_major ();
  Gc.full_major ();
  check "live extent not freed" (freed_count () = 0);
  fill_first ();
  Gc.full_major ();
  check_first ();
  (* Keep only one block alive; the blocks it points to (within the
     extent) must stay alive too, while its other neighbours die. *)
  keep_only_block1 ();
  Gc.full_major ();
  Gc.full_major ();
  check "partially-dead extent not freed" (freed_count () = 0);
  check_via_extent ();
  roots := [||];
  expect_freed "dead extent freed" 1

(* Interleaved live and dead blocks: dead blocks are swept while their
   live neighbours survive; consolidation of adjacent free blocks must
   not disturb the survivors. *)

let create_interleaved () =
  let b = make_extent (Array.make 16 4) in
  Array.iteri (fun i blk -> Obj.set_field blk 0 (Obj.repr (i * 7))) b;
  (* keep every fourth block *)
  roots := Array.init 4 (fun i -> b.(i * 4))

let check_interleaved_survivors () =
  let ok = ref true in
  Array.iteri
    (fun i blk ->
      if (Obj.obj (Obj.field blk 0) : int) <> i * 4 * 7 then ok := false)
    !roots;
  check "surviving blocks intact after neighbours swept" !ok

let () =
  create_interleaved ();
  Gc.full_major ();
  Gc.full_major ();
  Gc.full_major ();
  check "interleaved extent not freed" (freed_count () = 1);
  check_interleaved_survivors ();
  roots := [||];
  expect_freed "interleaved extent freed after survivors dropped" 2

(* Custom blocks in an extent: their finalisers run when they are
   swept. Drop them half at a time to exercise partial sweeping. *)

let create_customs () = roots := make_custom_extent 8

let drop_half_customs () = roots := Array.sub !roots 4 4

let () =
  create_customs ();
  Gc.full_major ();
  check "no custom finalisers run while live" (finalised_count () = 0);
  drop_half_customs ();
  gc_until (fun () -> finalised_count () = 4);
  check "finalisers run for dead custom blocks" (finalised_count () = 4);
  check "partially-dead custom extent not freed" (freed_count () = 2);
  roots := [||];
  gc_until (fun () -> finalised_count () = 8 && freed_count () = 3);
  check "all custom finalisers run" (finalised_count () = 8);
  expect_freed "custom extent freed" 3

(* A single-block extent the size of one large value. *)

let create_big () =
  roots := make_extent [| 1000 |];
  Obj.set_field !roots.(0) 999 (Obj.repr 999)

let check_big () =
  check "last field of single-block extent intact"
    ((Obj.obj (Obj.field !roots.(0) 999) : int) = 999)

let () =
  create_big ();
  Gc.full_major ();
  check "single-block extent not freed while live" (freed_count () = 3);
  check_big ();
  roots := [||];
  expect_freed "single-block extent freed" 4

(* Marking from extents: a chain of pointers through the blocks of an
   extent, rooted only at its head, ending in a long heap-allocated
   list reachable only through the last extent block. Everything must
   be kept alive by marking through extent block fields. *)

let list_len = 10_000

let create_chain () =
  let b = make_extent (Array.make 32 2) in
  for i = 0 to 30 do
    Obj.set_field b.(i) 0 b.(i + 1)
  done;
  Obj.set_field b.(31) 0 (Obj.repr (List.init list_len (fun i -> i)));
  Obj.set_field b.(31) 1 (Obj.repr 31);
  roots := [| b.(0) |]

let check_chain () =
  let blk = ref !roots.(0) in
  for _ = 1 to 31 do
    blk := Obj.field !blk 0
  done;
  check "intra-extent chain survives"
    ((Obj.obj (Obj.field !blk 1) : int) = 31);
  let l : int list = Obj.obj (Obj.field !blk 0) in
  check "long heap list via extent survives"
    (List.length l = list_len
     && List.fold_left ( + ) 0 l = list_len * (list_len - 1) / 2)

let () =
  create_chain ();
  Gc.full_major ();
  Gc.full_major ();
  check "chained extent not freed" (freed_count () = 4);
  check_chain ();
  roots := [||];
  expect_freed "chained extent freed" 5
