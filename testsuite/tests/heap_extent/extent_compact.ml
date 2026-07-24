(* TEST
 modules = "extent_stubs.c";
*)

(* Compaction with heap extents: extent blocks are not moved, but
   pointers held in their fields must be updated when the blocks they
   point to are evacuated (compact_update_extent). The payload strings
   are deliberately allocated into mostly-garbage pools so that
   compaction does evacuate them, and we check both that the contents
   are intact and that the blocks really moved.

   Extent blocks are only ever bound to locals inside functions, never
   in top-level units (in native code those locals stay live to the
   end of the program); [roots] is the only persistent root. *)

external make_extent : int array -> Obj.t array = "heap_extent_make"
external freed_count : unit -> int = "heap_extent_freed_count"
external address_of : Obj.t -> nativeint = "heap_extent_block_address"

let roots : Obj.t array ref = ref [||]

let gc_until cond =
  let rec go n =
    if not (cond ()) && n > 0 then (Gc.full_major (); go (n - 1))
  in
  go 20

let check msg b =
  Printf.printf "%s: %s\n%!" msg (if b then "ok" else "FAILED")

let n_blocks = 64

let payload i = Printf.sprintf "extent payload %06d" i

(* Each extent block points to a fresh heap string, promoted to the
   major heap among 15 same-sized strings that then become garbage:
   after a major collection the payloads sit in mostly-empty pools,
   which compaction evacuates. Returns the payloads' addresses before
   compaction. *)
let setup () =
  let b = make_extent (Array.make n_blocks 2) in
  roots := b;
  let garbage = Array.make (n_blocks * 16) "" in
  Array.iteri
    (fun i blk ->
      for j = 0 to 14 do
        garbage.((i * 16) + j) <-
          Printf.sprintf "extent garbage %06d" ((i * 16) + j)
      done;
      let p = payload i in
      garbage.((i * 16) + 15) <- p;
      Obj.set_field blk 0 (Obj.repr p))
    b;
  (* An array of refs reachable only through the extent. *)
  Obj.set_field b.(0) 1 (Obj.repr (Array.init 1000 (fun i -> ref i)));
  (* An extent-internal pointer: block 1 points at block 2. *)
  Obj.set_field b.(1) 1 b.(2);
  (* Promote payloads and the garbage around them into the pools
     together, then drop the garbage, leaving the pools sparse. *)
  Gc.minor ();
  Array.fill garbage 0 (Array.length garbage) "";
  Gc.full_major ();
  Array.map (fun blk -> address_of (Obj.field blk 0)) b

let check_payloads msg index_of =
  let ok = ref true in
  Array.iteri
    (fun i blk ->
      if not (String.equal (Obj.obj (Obj.field blk 0)) (payload (index_of i)))
      then ok := false)
    !roots;
  check msg !ok

let check_moved before =
  let moved = ref 0 in
  Array.iteri
    (fun i blk -> if address_of (Obj.field blk 0) <> before.(i) then incr moved)
    !roots;
  check "compaction moved blocks pointed to from extent" (!moved > 0)

let check_keep msg =
  let keep : int ref array = Obj.obj (Obj.field !roots.(0) 1) in
  let ok = ref (Array.length keep = 1000) in
  Array.iteri (fun i r -> if !r <> i then ok := false) keep;
  check msg !ok

let check_internal_pointer () =
  let via = Obj.field !roots.(1) 1 in
  check "extent-internal pointer intact after compaction"
    (String.equal (Obj.obj (Obj.field via 0)) (payload 2))

(* Keep only the even-numbered blocks. *)
let drop_odd_blocks () =
  let b = !roots in
  roots := Array.init (n_blocks / 2) (fun i -> b.(2 * i))

let () =
  let before = setup () in
  Gc.compact ();
  check_payloads "strings via extent intact after compaction" (fun i -> i);
  check_moved before;
  check_keep "keep refs reachable via extent after compaction";
  check_internal_pointer ();
  check "extent not freed by compaction" (freed_count () = 0);
  (* Compact again with half the extent blocks dead: compaction walks
     the extent, skipping free blocks, and updates the survivors. *)
  drop_odd_blocks ();
  Gc.full_major ();
  Gc.full_major ();
  Gc.compact ();
  check_payloads "survivor strings intact after compaction with free blocks"
    (fun i -> 2 * i);
  check_keep "keep refs reachable after second compaction";
  check "partially-dead extent not freed by compaction" (freed_count () = 0);
  (* Drop everything; the extent dies and is freed; a final compaction
     runs with no extents present. *)
  roots := [||];
  gc_until (fun () -> freed_count () = 1);
  check "extent freed after compaction tests" (freed_count () = 1);
  Gc.compact ();
  check "compaction after extent freed" true
