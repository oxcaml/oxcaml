(* TEST
 flags = "-g -alert -do_not_spawn_domains";
 modules = "swapgil_stubs.c";
 include systhreads;
 hassysthreads;
 multidomain;
 runtime5;
 hasunix;
 native;
*)

external swap_gil_setup : unit -> unit @@ portable = "swap_gil_setup"

external blocking_section : unit -> unit @@ portable = "blocking_section"

type c_thread
external create_c_thread : (unit -> unit) -> c_thread @@ portable = "create_c_thread"
external join_c_thread : c_thread -> unit @@ portable = "join_c_thread"

external swap_gil : unit -> unit @@ portable = "swap_gil"

let (threadfn @ portable) ~counter () =
  for i = 1 to 1_000 do
    Atomic.incr counter;
    let junk = Sys.opaque_identity (ref (Atomic.get counter)) in
    ignore junk;
    match i mod 100, i mod 10 with
    | _, 0 -> Thread.yield ()
    | _, 1 -> blocking_section ()
    | 22, _ -> Gc.minor ()
    | _, 3 -> swap_gil ()
    | _ -> ()
  done

let go () =
  let counter = Atomic.make 0 in
  let open Either in
  let threads =
    List.init 40 (fun i ->
      if i land 1 = 0 then
        Left (Thread.Portable.create (threadfn ~counter) ())
      else
        Right (create_c_thread (threadfn ~counter)))
  in
  List.iter (function Left th -> Thread.join th | Right ct -> join_c_thread ct) threads;
  Atomic.get counter

let () =
  swap_gil_setup ();
  let a = Domain.Safe.spawn (fun () -> swap_gil_setup (); go ()) in
  let b = Domain.Safe.spawn (fun () -> swap_gil_setup (); go ()) in
  let c = go () in
  let a = Domain.join a and b = Domain.join b in
  Printf.printf "%d %d %d\n" a b c
