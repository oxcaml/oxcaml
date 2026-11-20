(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* [Gc.Ramp.up] suspends the collection work for off-heap (dependent)
   memory, such as the data of large bigarrays, and [Gc.Ramp.down]
   resumes it. *)

let () =
  (* Fix the minor heap size, as it determines which bigarrays are
     allocated directly in the major heap. *)
  Gc.set { (Gc.get ()) with minor_heap_size = 256 * 1024 }

let cycles () = (Gc.quick_stat ()).major_collections

(* The number of major cycles completed while running [f]. *)
let cycles_during f =
  let before = cycles () in
  let res = f () in
  res, cycles () - before

(* 1 GiB of live off-heap memory, many times larger than the heap. *)
let alloc_off_heap () =
  List.init 64 (fun _ ->
    Bigarray.Array1.create Char C_layout (16 * 1024 * 1024))

(* 256 MiB of minor garbage, giving the GC many slices in which to do
   any outstanding work. *)
let run_slices () =
  for _ = 1 to 256 * 1024 do
    ignore (Sys.opaque_identity (Bytes.create 1024))
  done

(* Each case below starts with [Gc.full_major ()], so that work left
   over from the previous case is not counted. *)

let () =
  Gc.full_major ();
  let (live, work), ramp_cycles =
    cycles_during (fun () -> Gc.Ramp.up alloc_off_heap)
  in
  let (), idle_cycles = cycles_during run_slices in
  Printf.printf "ramp-up suspends off-heap work: %b\n"
    (ramp_cycles + idle_cycles <= 1);
  Gc.Ramp.down work;
  let (), resumed_cycles = cycles_during run_slices in
  Printf.printf "ramp-down resumes off-heap work: %b\n"
    (resumed_cycles > idle_cycles);
  ignore (Sys.opaque_identity live)

let () =
  (* If the callback raises, the suspended work is resumed. *)
  Gc.full_major ();
  let live = ref [] in
  let (), ramp_cycles =
    cycles_during (fun () ->
      match Gc.Ramp.up (fun () -> live := alloc_off_heap (); raise Exit) with
      | exception Exit -> ()
      | _ -> assert false)
  in
  let (), resumed_cycles = cycles_during run_slices in
  Printf.printf "exception: suspended %b, resumed %b\n"
    (ramp_cycles <= 1) (resumed_cycles > 0);
  ignore (Sys.opaque_identity live)

let () =
  (* Suspended work can be accumulated and resumed together. *)
  Gc.full_major ();
  let live = ref [] in
  let work = ref Gc.Ramp.zero in
  let (), ramp_cycles =
    cycles_during (fun () ->
      for _ = 1 to 2 do
        let l, w = Gc.Ramp.up alloc_off_heap in
        live := l :: !live;
        work := Gc.Ramp.add !work w
      done)
  in
  Gc.Ramp.down Gc.Ramp.zero;
  let (), idle_cycles = cycles_during run_slices in
  Gc.Ramp.down !work;
  let (), resumed_cycles = cycles_during run_slices in
  Printf.printf "accumulated: suspended %b, resumed %b\n"
    (ramp_cycles + idle_cycles <= 1) (resumed_cycles > idle_cycles);
  ignore (Sys.opaque_identity live)

let () =
  (* The old names are synonyms. *)
  let (), (work : Gc.suspended_collection_work) = Gc.ramp_up ignore in
  Gc.ramp_down (Gc.Ramp.add work Gc.Ramp.zero)

let () =
  (* Without a ramp-up, the same allocation drives the GC hard. *)
  Gc.full_major ();
  let live, plain_cycles = cycles_during alloc_off_heap in
  Printf.printf "no ramp-up: %b\n" (plain_cycles > 8);
  ignore (Sys.opaque_identity live)
