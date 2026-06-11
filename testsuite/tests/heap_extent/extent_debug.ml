(* TEST
 modules = "extent_stubs.c";
 set OCAMLRUNPARAM = "V=1";
 flags = "-runtime-variant=d";
 { bytecode; }
 { native; }
*)

(* Heap extents under the debug runtime with heap verification
   enabled (OCAMLRUNPARAM=V=1): exercises the CAMLasserts in
   caml_add_extent and extent_sweep, and the verify_extent /
   verify_swept consistency checks (extent_words, extent_blocks),
   after partial sweeps, consolidation and compaction.

   Extent blocks are only ever bound to locals inside functions, never
   in top-level units (in native code those locals stay live to the
   end of the program); [roots] is the only persistent root. *)

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

let create () =
  let b = make_extent [| 1; 2; 0; 3; 4; 0; 5 |] in
  Array.iteri (fun i blk ->
      if Obj.size blk > 0 then
        Obj.set_field blk 0 (Obj.repr (ref (i * 11)))) b;
  roots := b

let drop_alternate () =
  let b = !roots in
  roots := Array.init 3 (fun i -> b.(i * 2))

let check_survivors () =
  let ok = ref true in
  Array.iteri
    (fun i blk ->
      if Obj.size blk > 0
         && !(Obj.obj (Obj.field blk 0) : int ref) <> i * 2 * 11
      then ok := false)
    !roots;
  check "survivors intact under debug runtime" !ok

let () =
  create ();
  Gc.full_major ();
  Gc.full_major ();
  check "live extent verified" (freed_count () = 0);
  drop_alternate ();
  Gc.full_major ();
  Gc.full_major ();
  check "partially-dead extent verified" (freed_count () = 0);
  check_survivors ();
  Gc.compact ();
  check_survivors ();
  roots := [||];
  gc_until (fun () -> freed_count () = 1);
  check "extent freed under debug runtime" (freed_count () = 1)

let () =
  roots := make_custom_extent 4;
  Gc.full_major ();
  roots := [||];
  gc_until (fun () -> finalised_count () = 4 && freed_count () = 2);
  check "custom extent finalised and freed under debug runtime"
    (finalised_count () = 4 && freed_count () = 2)
