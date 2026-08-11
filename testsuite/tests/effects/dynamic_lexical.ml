(* TEST
   ocamlrunparam += ",Xfiber_stack_size=1048576";
   { bytecode; }
   { native; }
*)

(* Tests for lexically-pinned dynamic bindings: a fiber's lexical parent
   (set via [caml_dynamic_use_scope]) makes dynamic lookups see the
   bindings that were visible at a fork point, even when a scheduler runs the
   fiber under an unrelated parent, while dynamics unbound at the fork point
   still resolve against the chain the fiber actually runs on. *)

[@@@alert "-unsafe_effects"]

open Modes

type scope
external freeze_scope : unit -> scope = "caml_dynamic_freeze_scope"
external use_scope : scope -> unit = "caml_dynamic_use_scope"
external set_root : unit -> unit = "caml_dynamic_set_root"

let print_null = function
  | This x -> Int.to_string x
  | Null -> "null"

let get d = print_null (Dynamic.get d)

let with_temp d v ~f =
  (Dynamic.with_temporarily d v ~f:(fun () ->
    { Many.many = { Aliased.aliased = { Global.global = f () }}}))
    .many.aliased.global

let d_lex = Dynamic.make ()    (* bound around fork points *)
let d_worker = Dynamic.make () (* bound only on the scheduler's chain *)
let d_both = Dynamic.make ()   (* bound at the fork point and by the scheduler *)
let d_child = Dynamic.make ()  (* bound inside a forked child *)
let d_inter = Dynamic.make ()  (* bound in an intermediate fiber *)

type _ Effect.t += Fork : scope * (unit -> unit) * (unit -> unit) -> unit Effect.t
type _ Effect.t += Spawn_unpinned : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t

let fork_join f g =
  let self = freeze_scope () in
  Effect.perform (Fork (self, f, g))

let next_worker = ref 100
let reset () = next_worker := 100

(* A mini scheduler. Forked tasks run in fresh fibers parented under the
   scheduler itself (as if stolen by another worker), with scheduler-local
   bindings for [d_worker] and [d_both] in scope; their lexical parent is
   pinned to the fork point. [Yield] resumes the task under a fresh fiber
   with a different [d_worker] binding, simulating migration.

   Task fibers that are not fork/join children are mounted via
   [handle_task], which marks them as root tasks before the body runs:
   span freezing at fork points must stop at the task base instead of
   running into the scheduler's own chain. *)
let rec handle : (unit -> unit) -> unit = fun f ->
  Effect.Deep.match_with f ()
    { retc = (fun () -> ());
      exnc = (fun e -> raise e);
      effc = (fun (type a) (e : a Effect.t) :
                ((a, unit) Effect.Deep.continuation -> unit) option ->
        match e with
        | Fork (parent, g1, g2) ->
          Some (fun k ->
            let w = !next_worker in
            next_worker := !next_worker + 100;
            with_temp d_worker w ~f:(fun () ->
              with_temp d_both (w + 50) ~f:(fun () ->
                run_task parent g1;
                run_task parent g2));
            Effect.Deep.continue k ())
        | Spawn_unpinned g ->
          Some (fun k ->
            handle_task g;
            Effect.Deep.continue k ())
        | Yield ->
          Some (fun k ->
            handle_task (fun () ->
              with_temp d_worker 999 ~f:(fun () ->
                Effect.Deep.continue k ())))
        | _ -> None) }

and handle_task : (unit -> unit) -> unit = fun f ->
  handle (fun () ->
    set_root ();
    f ())

and run_task : scope -> (unit -> unit) -> unit = fun parent g ->
  handle (fun () ->
    use_scope parent;
    g ())

let passthrough f =
  Effect.Deep.match_with f ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = (fun _ -> None) }

let () =
  reset ();
  print_endline "# Test 1: pinning across reparenting";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      with_temp d_both 2 ~f:(fun () ->
        fork_join
          (fun () ->
            Printf.printf "child1 d_lex [expect 1]: %s\n" (get d_lex);
            Printf.printf "child1 d_worker [expect 100]: %s\n" (get d_worker);
            Printf.printf "child1 d_both [expect 2]: %s\n" (get d_both);
            passthrough (fun () ->
              Printf.printf "child1 subfiber d_lex [expect 1]: %s\n"
                (get d_lex)))
          (fun () ->
            Printf.printf "child2 d_lex [expect 1]: %s\n" (get d_lex));
        Effect.perform (Spawn_unpinned (fun () ->
          Printf.printf "unpinned d_lex [expect null]: %s\n" (get d_lex)));
        Printf.printf "parent d_lex after join [expect 1]: %s\n" (get d_lex);
        Printf.printf "parent d_worker after join [expect null]: %s\n"
          (get d_worker))))

let () =
  reset ();
  print_endline "\n# Test 2: shadowing inside the child";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      fork_join
        (fun () ->
          with_temp d_lex 5 ~f:(fun () ->
            Printf.printf "child d_lex shadowed [expect 5]: %s\n" (get d_lex));
          Printf.printf "child d_lex unshadowed [expect 1]: %s\n" (get d_lex))
        (fun () -> ());
      Printf.printf "parent d_lex after join [expect 1]: %s\n" (get d_lex)))

let () =
  reset ();
  print_endline "\n# Test 3: nested fork_join";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      fork_join
        (fun () ->
          with_temp d_child 3 ~f:(fun () ->
            fork_join
              (fun () ->
                Printf.printf "grandchild d_lex [expect 1]: %s\n" (get d_lex);
                Printf.printf "grandchild d_child [expect 3]: %s\n"
                  (get d_child);
                Printf.printf "grandchild d_worker [expect 200]: %s\n"
                  (get d_worker))
              (fun () -> ()));
          Printf.printf "child d_worker after nested join [expect 100]: %s\n"
            (get d_worker))
        (fun () -> ())))

let () =
  reset ();
  print_endline "\n# Test 4: intermediate fiber captured with the fork";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      passthrough (fun () ->
        with_temp d_inter 7 ~f:(fun () ->
          fork_join
            (fun () ->
              Printf.printf "child d_inter [expect 7]: %s\n" (get d_inter);
              Printf.printf "child d_lex [expect 1]: %s\n" (get d_lex);
              Printf.printf "child d_worker [expect 100]: %s\n" (get d_worker))
            (fun () -> ());
          Printf.printf "forker d_inter after join [expect 7]: %s\n"
            (get d_inter)))))

let () =
  reset ();
  print_endline "\n# Test 5: suspend and resume under a new parent";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      fork_join
        (fun () ->
          Printf.printf "child d_lex before yield [expect 1]: %s\n" (get d_lex);
          Printf.printf "child d_worker before yield [expect 100]: %s\n"
            (get d_worker);
          Effect.perform Yield;
          Printf.printf "child d_lex after yield [expect 1]: %s\n" (get d_lex);
          Printf.printf "child d_worker after yield [expect 999]: %s\n"
            (get d_worker))
        (fun () ->
          Printf.printf "child2 d_worker [expect 100]: %s\n" (get d_worker))))

let () =
  reset ();
  print_endline "\n# Test 6: child stack growth";
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      fork_join
        (fun () ->
          let rec burn n =
            if n = 0
            then (match Dynamic.get d_lex with This v -> v | Null -> -1)
            else begin
              let r = burn (n - 1) in
              ignore (Sys.opaque_identity (r + n));
              r
            end
          in
          Printf.printf "child d_lex after deep recursion [expect 1]: %d\n"
            (burn 10_000))
        (fun () -> ())))

(* Test 7 exercises the root flag with a parent task that never suspends:
   fiber T (a root task, mounted on the main stack) stays live below the
   pinned child C, which runs under a separate "worker" fiber S. The child
   must inherit T's bindings, and a dynamic bound both on S (its worker)
   and on the main stack (beyond the task base) must resolve to S: without
   the flag, the detour would run past T and find the main stack's binding
   first. The deep recursion inside T checks that the flag survives stack
   growth. *)

let () =
  reset ();
  print_endline "\n# Test 7: root flag stops detours at the task base";
  with_temp d_worker 111 ~f:(fun () ->
    passthrough (fun () ->
      (* fiber T: a root task, live below the child throughout *)
      set_root ();
      let rec grow n =
        if n = 0
        then 0
        else begin
          let r = grow (n - 1) in
          ignore (Sys.opaque_identity (r + n));
          r
        end
      in
      ignore (grow 10_000);
      let t = freeze_scope () in
      with_temp d_lex 1 ~f:(fun () ->
        passthrough (fun () ->
          (* fiber S: the child's own "worker" *)
          with_temp d_worker 222 ~f:(fun () ->
            passthrough (fun () ->
              (* fiber C: a fork child pinned to the live fiber T *)
              use_scope t;
              Printf.printf "child d_lex from live parent [expect 1]: %s\n"
                (get d_lex);
              Printf.printf
                "child d_worker from own worker [expect 222]: %s\n"
                (get d_worker))));
        Printf.printf "parent d_worker [expect 111]: %s\n" (get d_worker))))

(* A fiber created inside a fork child can escape the scheduler entirely
   (e.g. a generator): it carries no lexical edge, so once resumed outside
   it resolves against its new parent chain, while its own bindings travel
   with it. *)

type _ Effect.t += Escape : unit Effect.t

let () =
  reset ();
  print_endline "\n# Test 8: fiber escaping the scheduler is reparented";
  let stash : (unit, unit) Effect.Deep.continuation option ref =
    ref None
  in
  handle_task (fun () ->
    with_temp d_lex 1 ~f:(fun () ->
      fork_join
        (fun () ->
          Effect.Deep.match_with
            (fun () ->
              with_temp d_child 42 ~f:(fun () ->
                Printf.printf "generator inside task d_lex [expect 1]: %s\n"
                  (get d_lex);
                Effect.perform Escape;
                Printf.printf "escaped generator d_lex [expect 9]: %s\n"
                  (get d_lex);
                Printf.printf
                  "escaped generator own binding [expect 42]: %s\n"
                  (get d_child)))
            ()
            { retc = (fun () -> ());
              exnc = (fun e -> raise e);
              effc = (fun (type a) (e : a Effect.t) :
                        ((a, unit) Effect.Deep.continuation -> unit) option ->
                match e with
                | Escape -> Some (fun k -> stash := Some k)
                | _ -> None) })
        (fun () -> ())));
  match !stash with
  | Some k -> with_temp d_lex 9 ~f:(fun () -> Effect.Deep.continue k ())
  | None -> assert false

(* The fork-point handle is the fiber's stable dynamic-state node, so it
   survives stack reallocation: growing the fork point's stack after taking
   the handle (bytecode fibers start tiny and grow by realloc) must not
   invalidate the edge or the bindings reached through it. *)

let () =
  reset ();
  print_endline "\n# Test 9: handle survives fork-point stack growth";
  with_temp d_worker 111 ~f:(fun () ->
    passthrough (fun () ->
      (* fiber T: the fork point, growing after its handle is taken *)
      set_root ();
      with_temp d_lex 1 ~f:(fun () ->
        let t = freeze_scope () in
        let rec grow n =
          if n = 0
          then 0
          else begin
            let r = grow (n - 1) in
            ignore (Sys.opaque_identity (r + n));
            r
          end
        in
        ignore (grow 10_000);
        passthrough (fun () ->
          (* fiber S: the child's own "worker" *)
          with_temp d_worker 222 ~f:(fun () ->
            passthrough (fun () ->
              (* fiber C: pinned to T after T's stack moved *)
              use_scope t;
              Printf.printf "child d_lex after growth [expect 1]: %s\n"
                (get d_lex);
              Printf.printf "child d_worker [expect 222]: %s\n"
                (get d_worker)))))))

(* Freezing again from the same fork point must revalidate the chain: after
   the fork point is captured out of one intermediate fiber and resumed
   under another chain, its own lookups must track the live chain (plain
   links are ignored by the spine), and a second freeze must hand new
   children the new chain rather than the one frozen for the first scope,
   whose fibers are gone. *)

type _ Effect.t += Capture : unit Effect.t

let () =
  reset ();
  print_endline "\n# Test 10: re-freeze after reparenting the fork point";
  let stash : (unit, unit) Effect.Deep.continuation option ref = ref None in
  let run_child scope f =
    passthrough (fun () ->
      (* a fresh unrelated fiber standing in for a worker *)
      passthrough (fun () ->
        use_scope scope;
        f ()))
  in
  passthrough (fun () ->
    (* fiber T: the root task *)
    set_root ();
    passthrough (fun () ->
      (* fiber A: binds d_inter for the first scope, dies before the second *)
      with_temp d_inter 7 ~f:(fun () ->
        Effect.Deep.match_with
          (fun () ->
            passthrough (fun () ->
              (* fiber F: the fork point *)
              let s1 = freeze_scope () in
              run_child s1 (fun () ->
                Printf.printf "child1 d_inter [expect 7]: %s\n" (get d_inter));
              (* re-freezing an unchanged chain is a no-op *)
              let s1b = freeze_scope () in
              run_child s1b (fun () ->
                Printf.printf "child1b d_inter [expect 7]: %s\n"
                  (get d_inter));
              Printf.printf "F d_inter before capture [expect 7]: %s\n"
                (get d_inter);
              Effect.perform Capture;
              (* resumed under T with d_inter rebound; fiber A is gone *)
              Printf.printf "F d_inter after resume [expect 8]: %s\n"
                (get d_inter);
              let s2 = freeze_scope () in
              run_child s2 (fun () ->
                Printf.printf "child2 d_inter [expect 8]: %s\n"
                  (get d_inter))))
          ()
          { retc = (fun () -> ());
            exnc = (fun e -> raise e);
            effc = (fun (type a) (e : a Effect.t) :
                      ((a, unit) Effect.Deep.continuation -> unit) option ->
              match e with
              | Capture -> Some (fun k -> stash := Some k)
              | _ -> None) }));
    match !stash with
    | Some k -> with_temp d_inter 8 ~f:(fun () -> Effect.Deep.continue k ())
    | None -> assert false)
