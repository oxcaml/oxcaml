(* TEST
 modules = "extent_stubs.c";
*)

(* Thousands of extents: registration, sweeping and freeing must scale
   beyond the common case of a handful of extents. *)

external make_extent : int array -> Obj.t array = "heap_extent_make"
external freed_count : unit -> int = "heap_extent_freed_count"

let n = 4096

let gc_until cond =
  let rec go i =
    if not (cond ()) && i > 0 then (Gc.full_major (); go (i - 1))
  in
  go 50

let check msg b =
  Printf.printf "%s: %s\n%!" msg (if b then "ok" else "FAILED")

(* The whole test lives in a function so that no extent block is
   reachable from a top-level unit's frame (in native code those stay
   live to the end of the program). *)
let run () =
  let exts =
    Array.init n (fun i ->
        let b = make_extent [| 1; 2 |] in
        (* A heap-allocated ref reachable only through the extent, so
           marking must traverse every extent block. *)
        Obj.set_field b.(0) 0 (Obj.repr (ref i));
        b)
  in
  Gc.full_major ();
  check "no extents freed while live" (freed_count () = 0);
  let ok = ref true in
  for i = 0 to n - 1 do
    if !(Obj.obj (Obj.field exts.(i).(0) 0) : int ref) <> i then ok := false
  done;
  check "all blocks intact" !ok;
  (* Drop every other extent. *)
  for i = 0 to n - 1 do
    if i mod 2 = 0 then exts.(i) <- [||]
  done;
  gc_until (fun () -> freed_count () = n / 2);
  check "half the extents freed" (freed_count () = n / 2);
  let ok = ref true in
  for i = 0 to n - 1 do
    if i mod 2 = 1 then
      if !(Obj.obj (Obj.field exts.(i).(0) 0) : int ref) <> i then ok := false
  done;
  check "surviving extents intact" !ok;
  (* Drop the rest. *)
  for i = 0 to n - 1 do
    exts.(i) <- [||]
  done;
  ignore (Sys.opaque_identity exts)

let () =
  run ();
  gc_until (fun () -> freed_count () = n);
  check "all extents freed" (freed_count () = n)
