(* TEST *)

let drop x = ignore (Sys.opaque_identity x)

let () =
  (* This pacing bug is easier to reproduce with more major slices, so use
     a small minor heap size to test for it. *)
  Gc.set { (Gc.get ()) with minor_heap_size = 64 * 1024 }

let () =
  let heap_junk = List.init 10_000 ref in
  (* approx 1 GiB of off-heap garbage

     Creating this much off-heap garbage (many times larger than the heap)
     incurs a huge amount of GC work. However, after 2 cycles it should be
     cancelled. *)
  for _ = 1 to 5 do
    drop (Bigarray.Array1.create Char C_layout (100 * 1024 * 1024))
  done;
  (* approx 1 GiB of minor garbage, triggering many major slices. *)
  for _ = 1 to 1024 * 1024 do
    drop (Bytes.create 1024)
  done;
  let cyc =
    (* Make this test more robust by not printing the exact count of cycles
       when small. (Without the fix, this test does >1000 cycles, so the
       difference between 5 and 10 cycles doesn't matter).  *)
    match (Gc.quick_stat ()).major_collections with
    | n when n <= 10  -> "<=10"
    | n -> string_of_int n
  in
  Printf.printf "%s cycles\n" cyc;
  Sys.opaque_identity heap_junk |> ignore
