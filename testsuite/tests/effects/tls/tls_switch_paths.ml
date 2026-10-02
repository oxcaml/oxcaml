(* TEST
   flags += "-alert -unsafe_multidomain -alert -unsafe_effects";
   { bytecode; }
   { native; }
*)

(* The cached TLS state is correct on every stack-switch path that crosses
   a TLS owner (a preemptible fiber, P below):
   - an effect performed by a non-owner fiber inside P and handled outside
     P, which takes several perform/reperform hops;
   - an effect performed by a non-owner inside P and handled inside P;
   - an unhandled effect, which switches back to the performer;
   - discontinue and discontinue_with_backtrace into P;
   - exceptions escaping P (to [exnc]) and escaping a non-owner inside P;
   - [retc] running on P's parent. *)

open Effect
open Effect.Deep

type _ Effect.t += Outer : unit Effect.t | Inner : unit Effect.t
type _ Effect.t += Suspend : unit Effect.t | Unknown : unit Effect.t

let k = Domain.TLS.new_key (fun () -> "init")
let get () = Domain.TLS.get k
let set v = Domain.TLS.set k v

let forward_all ~expect =
  { retc = Fun.id; exnc = raise;
    effc = (fun (type a) (_ : a Effect.t) ->
      (* The handler of a fiber runs on the fiber's parent. *)
      assert (get () = expect);
      None) }

(* thread -> F0 (non-owner, handles [Outer]) -> P (owner, forwards)
   -> N (non-owner, handles [Inner], forwards the rest). *)
let multi_hop () =
  set "thread";
  match_with (fun () ->
      Preemptible.match_with (fun () ->
          set "P";
          match_with (fun () ->
              assert (get () = "P");
              (* Handled outside P: N -> P (no owner left), P -> F0
                 (owner left), F0 -> thread (no owner left). *)
              perform Outer;
              assert (get () = "P");
              (* Handled inside P: N -> P, no owner left. *)
              perform Inner;
              assert (get () = "P-inner"))
            ()
            { retc = Fun.id; exnc = raise;
              effc = (fun (type a) (e : a Effect.t) ->
                (* Runs on P. *)
                assert (get () = "P" || get () = "P-inner");
                match e with
                | Inner -> Some (fun (k' : (a, _) continuation) ->
                    set "P-inner";
                    continue k' ())
                | _ -> None) };
          assert (get () = "P-inner"))
        ()
        { retc = Fun.id; exnc = raise;
          effc = (fun (type a) (_ : a Effect.t) ->
            (* Runs on F0, which shares the thread's state. *)
            assert (get () = "thread");
            None);
          tickc = This (fun () -> Continue) };
      assert (get () = "outer-handler"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (e : a Effect.t) ->
        match e with
        | Outer -> Some (fun (k' : (a, _) continuation) ->
            (* Runs on the thread. *)
            assert (get () = "thread");
            set "outer-handler";
            continue k' ())
        | _ -> None) };
  assert (get () = "outer-handler")

(* An unhandled effect is raised as [Effect.Unhandled] in the performer,
   after switching back to it from the top of the chain. *)
let unhandled () =
  set "thread";
  Preemptible.match_with (fun () ->
      set "P";
      (* Performed by the owner. *)
      (match perform Unknown with
       | () -> assert false
       | exception Effect.Unhandled _ -> assert (get () = "P"));
      (* Performed by a non-owner inside the owner, through intermediate
         handlers that forward it. *)
      match_with (fun () ->
          match perform Unknown with
          | () -> assert false
          | exception Effect.Unhandled _ -> assert (get () = "P"))
        ()
        (forward_all ~expect:"P");
      assert (get () = "P"))
    ()
    { retc = Fun.id; exnc = raise;
      effc = (fun (type a) (_ : a Effect.t) ->
        assert (get () = "thread");
        None);
      tickc = This (fun () -> Continue) };
  assert (get () = "thread")

type resume = Continue_ | Discontinue | Discontinue_with_backtrace

let suspend_then resume =
  { Preemptible.retc = Fun.id; exnc = raise;
    effc = (fun (type a) (e : a Effect.t) ->
      match e with
      | Suspend -> Some (fun (k' : (a, _) continuation) ->
          assert (get () = "thread");
          set "handler";
          match resume with
          | Continue_ -> continue k' ()
          | Discontinue -> discontinue k' Exit
          | Discontinue_with_backtrace ->
            discontinue_with_backtrace k' Exit (Printexc.get_callstack 1))
      | _ -> None);
    tickc = This (fun () -> Continue) }

let discontinue_paths () =
  set "thread";
  Preemptible.match_with (fun () ->
      set "P";
      (match perform Suspend with
       | () -> assert false
       | exception Exit -> assert (get () = "P"));
      ())
    () (suspend_then Discontinue);
  assert (get () = "handler");
  set "thread";
  Preemptible.match_with (fun () ->
      set "P";
      (match perform Suspend with
       | () -> assert false
       | exception Exit -> assert (get () = "P"));
      ())
    () (suspend_then Discontinue_with_backtrace);
  assert (get () = "handler")

let exception_paths () =
  set "thread";
  (* An exception escaping P runs [exnc] on the parent. *)
  let r =
    Preemptible.match_with (fun () ->
        set "P";
        raise Exit)
      ()
      { retc = (fun () -> "returned"); exnc = (fun e ->
          assert (e = Exit);
          assert (get () = "thread");
          "raised");
        effc = (fun (type a) (_ : a Effect.t) -> None);
        tickc = This (fun () -> Continue) }
  in
  assert (r = "raised");
  assert (get () = "thread");
  (* [retc] also runs on the parent. *)
  let r =
    Preemptible.match_with (fun () -> set "P")
      ()
      { retc = (fun () -> assert (get () = "thread"); "returned");
        exnc = raise;
        effc = (fun (type a) (_ : a Effect.t) -> None);
        tickc = This (fun () -> Continue) }
  in
  assert (r = "returned");
  (* An exception escaping a non-owner inside P lands back in P. *)
  Preemptible.match_with (fun () ->
      set "P";
      (match
         match_with (fun () -> set "N"; raise Exit) ()
           { retc = Fun.id;
             exnc = (fun e -> assert (get () = "N"); raise e);
             effc = (fun (type a) (_ : a Effect.t) -> None) }
       with
       | () -> assert false
       | exception Exit -> assert (get () = "N"));
      (* The non-owner wrote through to P's state. *)
      set "P-again";
      perform Suspend;
      assert (get () = "P-again"))
    () (suspend_then Continue_);
  assert (get () = "handler")

let () =
  multi_hop ();
  unhandled ();
  discontinue_paths ();
  exception_paths ();
  print_endline "OK"
