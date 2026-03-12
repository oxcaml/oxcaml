(* TEST
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 include systhreads;
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

module M = Gc.Memprof

let start ~counts =
  let alloc_minor _ =
    let id = (Domain.self_index() :> int) in
    counts.(id) <- (counts.(id) + 1); None
  in
  M.start ~sampling_rate:1. { M.null_tracker with alloc_minor }

let alloc_some () =
  let rec f n =
    if n = 0 then [] else (ref 0) :: (f (n-1))
  in
  ignore (Sys.opaque_identity (f 100))

let print_counts ~counts =
  Array.iteri (fun i n -> Printf.printf "Domain %d allocated %d words.\n" i n) counts;
  flush stdout

(* `trigger ()` is `(wait, go)`, where `wait()` will wait until `go()` is called.
    and resets the trigger (so go() can be meaningfully called more than once). *)

let trigger () =
  let t = Atomic.make false in
  ((fun () -> (while not (Atomic.get t) do Thread.yield () done; Atomic.set t false)),
   (fun () -> Atomic.set t true))

let () =
  let counts = [| 0 ; 0 |] in
  let wait, go = trigger () in
  let r = ref None in
  let d = Domain.spawn (fun () ->
    wait ();
    M.participate (Option.get (!r)) ;
    alloc_some ())
  in
  r := Some (start ~counts);
  go ();
  ignore (alloc_some ()) ;
  Domain.join d ;
  M.stop ();
  assert (counts.(0) >= 200);
  assert (counts.(0) < 205); (* some headroom for e.g. closures in bytecode *)
  assert (counts.(1) >= 200);
  assert (counts.(1) < 205) (* some headroom for e.g. closures in bytecode *)

let () =
  let counts = [| 0; 0 |] in
  let wait, go = trigger () in
  let d = Domain.spawn (fun () ->
    wait ();
    alloc_some ())
  in
  let mp = start ~counts in
  M.participate_globally mp;
  go ();
  ignore (alloc_some ());
  Domain.join d;
  M.stop ();
  assert (counts.(0) >= 200);
  assert (counts.(0) < 205); (* some headroom for e.g. closures in bytecode *)
  assert (counts.(1) >= 200);
  assert (counts.(1) < 210) (* some headroom for e.g. closures in bytecode + Domain.spawn *)
