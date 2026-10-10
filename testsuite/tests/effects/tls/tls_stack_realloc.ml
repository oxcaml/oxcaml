(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* Growing a stack moves its TLS state to the new stack. Fibers start with
   small stacks, so deep recursion reallocates them repeatedly. The state
   must still be found from the owner after a switch away and back, which
   recomputes the cache from the (reallocated) owner. *)

open Effect
open Effect.Deep

type _ Effect.t += Suspend : unit Effect.t

let k = Domain.TLS.new_key (fun () -> "init")
let get () = Domain.TLS.get k
let set v = Domain.TLS.set k v

let depth = 100_000

(* Non-tail recursion, so that [depth] frames are live at the bottom. *)
let rec deep n f = if n = 0 then f () else 1 + deep (n - 1) f

let suspend_handler () =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (e : a Effect.t) ->
      match e with
      | Suspend -> Some (fun (k' : (a, _) continuation) ->
          assert (get () = "thread");
          continue k' ())
      | _ -> None);
    tickc = This (fun () -> Continue) }

let non_preemptible f =
  match_with f ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) -> None) }

let () =
  set "thread";

  (* The owner itself grows, then reads and writes at the bottom. *)
  Preemptible.match_with (fun () ->
      set "P";
      ignore (deep depth (fun () ->
          assert (get () = "P");
          set "P-deep";
          (* Switch away and back from the bottom of the deep stack. *)
          perform Suspend;
          assert (get () = "P-deep");
          0));
      assert (get () = "P-deep");
      perform Suspend;
      assert (get () = "P-deep"))
    () (suspend_handler ());
  assert (get () = "thread");

  (* A non-owner inside the owner grows; its writes reach the owner. *)
  Preemptible.match_with (fun () ->
      set "P";
      non_preemptible (fun () ->
          ignore (deep depth (fun () ->
              assert (get () = "P");
              set "N-deep";
              perform Suspend;
              assert (get () = "N-deep");
              0)));
      assert (get () = "N-deep");
      perform Suspend;
      assert (get () = "N-deep"))
    () (suspend_handler ());
  assert (get () = "thread");

  (* The thread's main stack grows. *)
  ignore (deep depth (fun () ->
      assert (get () = "thread");
      set "thread-deep";
      Preemptible.match_with (fun () -> assert (get () = "init")) ()
        (suspend_handler ());
      assert (get () = "thread-deep");
      0));
  assert (get () = "thread-deep");
  non_preemptible (fun () -> assert (get () = "thread-deep"));

  print_endline "OK"
