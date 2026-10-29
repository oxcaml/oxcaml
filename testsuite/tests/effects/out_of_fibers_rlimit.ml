(* TEST
   modules = "out_of_fibers_rlimit_.c";
   (* Pin the fiber stack size: the test needs a fiber stack to be much
      larger than the address space it leaves spare, and the default size
      follows the C library's default thread stack, which is only 128KiB
      on musl. 4194304 words is 32MiB. *)
   ocamlrunparam += ",Xfiber_stack_size=4194304";
   (* Idle every promoted continuation, however few stacks exist: the
      wake test needs its parked fibers idled. *)
   ocamlrunparam += ",Xmax_guarded_stacks=0";
   no-stack-checks;
   no-address-sanitizer;
   no-tsan;
   linux;
   native;
*)

(* Genuine fiber-allocation failure: cap the address space, then nest live
   fibers -- each reserving a guarded stack of several megabytes -- until
   the runtime cannot allocate another and raises [Out_of_fibers]. Every
   fiber in the nest is running, so none of them can be idled. Capping the
   address space here makes the failure deterministic, rather than relying
   on the kernel's mapping limit (vm.max_map_count), which varies between
   machines. *)

external cap_address_space : int -> bool = "test_cap_address_space"
external uncap_address_space : unit -> bool = "test_uncap_address_space"

type _ Effect.t += E : unit Effect.t

let handler =
  { Effect.Deep.retc = Fun.id;
    exnc = raise;
    effc =
      (fun (type a) (e : a Effect.t) ->
        match e with
        | E -> Some (fun (_ : (a, _) Effect.Deep.continuation) -> 0)
        | _ -> None) }

let rec nest n =
  if n = 0
  then 0
  else Effect.Deep.match_with (fun () -> 1 + nest (n - 1)) () handler

(* Enough address space for about sixteen more fibers, whatever stack size
   is configured ([Gc.Tweak.get] reports it before the rounding done by the
   runtime, so this is a slight over-estimate), with a floor that leaves the
   GC room to work. *)
let headroom_mib =
  let fiber_bytes = Gc.Tweak.get "fiber_stack_size" * (Sys.word_size / 8) in
  max 64 (16 * fiber_bytes / 1024 / 1024)

(* Waking an idled continuation needs a guarded stack too, and that
   allocation can fail just as the one above does. Park fibers so that a
   collection idles them, return the stacks they release to the OS, and
   only then cap the address space: resuming must raise rather than abort,
   and must leave the continuation resumable once there is room again. *)
let test_wake () =
  let saved : (unit, unit) Effect.Deep.continuation list ref = ref [] in
  for _ = 1 to 4 do
    Effect.Deep.match_with (fun () -> Effect.perform E; 1) ()
      { retc = (fun _ -> ());
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E ->
              Some
                (fun (k : (a, unit) Effect.Deep.continuation) ->
                  saved := k :: !saved)
            | _ -> None) }
  done;
  Gc.minor ();    (* promote the continuations, so their stacks are idled *)
  Gc.compact ();  (* and give the stacks they released back to the OS *)
  (* Less than one fiber stack, but enough for the collector to work. *)
  if not (cap_address_space 8) then print_endline "could not cap"
  else begin
    (match !saved with
     | k :: _ ->
       (match Effect.Deep.continue k () with
        | () -> print_endline "woke despite the cap"
        | exception Effect.Out_of_fibers -> print_endline "wake: Out_of_fibers")
     | [] -> assert false);
    (* With room again, the continuation we failed to wake is still there. *)
    if not (uncap_address_space ()) then print_endline "could not uncap"
    else
      match !saved with
      | _ :: k :: _ ->
        Effect.Deep.continue k ();
        print_endline "wake: ok"
      | _ -> assert false
  end

let () =
  Printexc.record_backtrace false;
  if not (cap_address_space headroom_mib)
  then print_endline "could not cap the address space"
  else begin
    (match nest 1_000_000 with
     | n -> Printf.printf "no allocation failure after %d fibers\n" n
     | exception Effect.Out_of_fibers -> print_endline "Out_of_fibers");
    (* The failure must leave the runtime usable: the stacks of the
       abandoned nest are released as it unwinds. *)
    Printf.printf "ok %d\n" (nest 1);
    if uncap_address_space () then test_wake ()
    else print_endline "could not uncap"
  end
