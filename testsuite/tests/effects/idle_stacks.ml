(* TEST
   { bytecode; }
   { native; }
*)

(* Continuations that survive a minor collection have their stacks copied
   out of their mappings ("idled") and copied back onto fresh mappings
   when resumed ("woken"): exercise suspension, promotion and resumption
   with garbage-collection pressure in between. Under stack checks the
   same code runs without idling. *)

open Effect
open Effect.Deep

type _ Effect.t += E : int -> int Effect.t
type _ Effect.t += F : int Effect.t

exception Boom of int

(* Force minor collections, so that suspended continuations are promoted
   and their stacks idled. *)
let churn n =
  let r = ref [] in
  for i = 1 to n do
    r := [ i ] :: !r
  done;
  ignore (Sys.opaque_identity !r)

(* Suspend many fibers at once (enough to pass the default bound on
   guarded stacks), promote them all, then wake and finish each. *)
let test_mass () =
  let pending = Queue.create () in
  let results = ref 0 in
  let spawn i =
    match_with
      (fun () ->
        let v = perform (E i) in
        let w = perform (E (v + 1)) in
        results := !results + w)
      ()
      { retc = (fun () -> ());
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E n ->
              Some
                (fun (k : (a, _) continuation) ->
                  Queue.push (fun () -> continue k (n * 2)) pending)
            | _ -> None) }
  in
  for i = 1 to 1500 do
    spawn i
  done;
  churn 100_000;
  Gc.minor ();
  let steps = ref 0 in
  while not (Queue.is_empty pending) do
    let f = Queue.pop pending in
    f ();
    incr steps;
    if !steps mod 500 = 0 then churn 20_000
  done;
  (* Each fiber contributes 4i + 2. *)
  Printf.printf "mass: %d\n" !results

(* Discontinue continuations whose stacks have been idled: the exception
   must unwind through the relocated exception chain to the handler
   inside the fiber. *)
let test_discontinue () =
  let saved : (int * (int, unit) continuation) list ref = ref [] in
  let count = ref 0 in
  let spawn i =
    match_with
      (fun () ->
        try
          ignore (perform (E i) : int);
          assert false
        with Boom n -> count := !count + n)
      ()
      { retc = (fun () -> ());
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E n ->
              Some (fun (k : (a, _) continuation) -> saved := (n, k) :: !saved)
            | _ -> None) }
  in
  for i = 1 to 100 do
    spawn i
  done;
  churn 100_000;
  Gc.full_major ();
  List.iter (fun (i, k) -> discontinue k (Boom i)) !saved;
  Printf.printf "discontinue: %d\n" !count

(* A continuation capturing a chain of two fibers (the effect crosses an
   inner handler), promoted and then woken: exercises waking a whole
   chain, including the links between its stacks. Also compact, to
   exercise releasing cached mappings. *)
let test_nested () =
  let saved : ((int, unit) continuation * int) option ref = ref None in
  let result = ref 0 in
  match_with
    (fun () ->
      match_with
        (fun () ->
          let v = perform (E 7) in
          let w = perform F in
          result := v + w)
        ()
        { retc = Fun.id;
          exnc = raise;
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | F -> Some (fun (k : (a, _) continuation) -> continue k 100)
              | _ -> None) })
    ()
    { retc = Fun.id;
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E n -> Some (fun (k : (a, _) continuation) -> saved := Some (k, n))
          | _ -> None) };
  churn 100_000;
  Gc.compact ();
  (match !saved with
  | Some (k, n) -> continue k (n * 3)
  | None -> assert false);
  Printf.printf "nested: %d\n" !result

(* Reading the call stack of a promoted continuation must work without
   waking it. *)
let rec deep n = if n = 0 then perform (E 0) else 1 + deep (n - 1)

let test_callstack () =
  let saved : (int, unit) continuation option ref = ref None in
  match_with
    (fun () -> ignore (deep 20 : int))
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  match !saved with
  | Some k ->
    let bt = get_callstack k 100 in
    Printf.printf "callstack: %b\n" (Printexc.raw_backtrace_length bt > 0);
    continue k 0
  | None -> assert false

(* An effect cannot be captured across the C boundary installed by
   [Sys.with_async_exns] (callbacks mask the stack's handlers), so idle
   stacks never contain asynchronous-exception trap frames and the
   domain's async handler never points into one. Guard that invariant,
   and check the fiber still suspends and wakes normally afterwards. *)
let test_async_boundary () =
  let saved : (int, unit) continuation option ref = ref None in
  match_with
    (fun () ->
      (try
         Sys.with_async_exns (fun () -> ignore (perform (E 1) : int));
         assert false
       with Effect.Unhandled _ -> ());
      let v = perform (E 2) in
      Printf.printf "async_boundary: %d\n" v)
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  match !saved with Some k -> continue k 20 | None -> assert false

(* A dynamic binding made in the fiber lives in its stack's dynamic
   table, which travels with the stack when it is idled and woken. *)
let test_dynamic () =
  let saved : (int, unit) continuation option ref = ref None in
  let d : int Dynamic.t = Dynamic.make () in
  match_with
    (fun () ->
      (* [with_temporarily] is local-returning; unit mode-crosses. *)
      let () =
        Dynamic.with_temporarily d 33 ~f:(fun () ->
            let v = perform (E 3) in
            let bound = match Dynamic.get d with This n -> n | Null -> 0 in
            Printf.printf "dynamic: %d\n" (v + bound))
      in
      ())
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  match !saved with Some k -> continue k 9 | None -> assert false

(* Local allocations live across a suspension: the fiber's arenas and
   local-allocation state travel with the stack when idled and woken. *)
let test_locals () =
  let saved : (int, unit) continuation option ref = ref None in
  match_with
    (fun () ->
      let local_ r = ref 17 in
      let v = perform (E 5) in
      r := !r + v;
      Printf.printf "locals: %d\n" !r)
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  match !saved with Some k -> continue k 25 | None -> assert false

(* A three-fiber chain: the effect reperforms through two inner handlers
   that do not handle it, so waking relocates a chain of three stacks
   (and, with frame pointers, the links between them). *)
let test_chain3 () =
  let saved : (int, unit) continuation option ref = ref None in
  let result = ref 0 in
  let pass_through body =
    match_with body ()
      { retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | F -> Some (fun (k : (a, _) continuation) -> continue k 0)
            | _ -> None) }
  in
  match_with
    (fun () ->
      pass_through (fun () ->
          pass_through (fun () -> result := perform (E 6))))
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  (match !saved with Some k -> continue k 777 | None -> assert false);
  Printf.printf "chain3: %d\n" !result

(* A deep nest of live running fibers, well past the soft bound on
   guarded stacks. The bound-triggered minor-collection requests must
   back off (nothing here is reclaimable), not fire per fiber creation:
   that would cost a collection per fiber, each scanning the whole
   chain. Count collections rather than time. *)
let test_live_nest_gc_count () =
  let rec nest n =
    if n = 0
    then 0
    else
      match_with (fun () -> 1 + nest (n - 1)) ()
        { retc = Fun.id;
          exnc = raise;
          effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | F -> Some (fun (_ : (a, _) continuation) -> assert false)
              | _ -> None) }
  in
  let before = (Gc.quick_stat ()).minor_collections in
  let depth = nest 5000 in
  let minors = (Gc.quick_stat ()).minor_collections - before in
  Printf.printf "live_nest: %d %s\n" depth
    (if minors < 50 then "ok"
     else Printf.sprintf "too many minor collections (%d)" minors)

(* Deterministic reperform-idling: pass-through handlers force a minor
   collection while forwarding, so the continuation is promoted and its
   stacks idled mid-perform -- after one hop with [gc_inner] (idling a
   single stack), after two hops with only [gc_outer] (idling a
   two-stack chain, whose in-place wake also relocates the cross-fiber
   links). The outer handler parks the continuation, which is resumed
   only after further collections. *)
let test_reperform_gc ~gc_inner ~gc_outer label =
  let saved : (int, unit) continuation option ref = ref None in
  let result = ref 0 in
  let pass_through gc body =
    match_with body ()
      { retc = Fun.id;
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | F -> Some (fun (k : (a, _) continuation) -> continue k 0)
            | _ ->
              if gc then Gc.minor ();
              None) }
  in
  match_with
    (fun () ->
      pass_through gc_outer (fun () ->
          pass_through gc_inner (fun () -> result := perform (E 8))))
    ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E _ -> Some (fun (k : (a, _) continuation) -> saved := Some k)
          | _ -> None) };
  churn 100_000;
  Gc.full_major ();
  (match !saved with Some k -> continue k 888 | None -> assert false);
  Printf.printf "%s: %d\n" label !result

(* Reperform with GC pressure: the effect is forwarded through a middle
   fiber that does not handle it, and the forwarding handler allocates,
   so a minor collection can idle the continuation between the perform
   and the reperform, invalidating the last-fiber pointer being
   forwarded. caml_reperform must detect this and wake the chain in
   place rather than use the stale pointer. *)
let test_reperform_idle () =
  let iters = 20_000 in
  let sink = ref [||] in
  let total = ref 0 in
  let rec perform_all n =
    if n > 0 then begin
      total := !total + perform (E n);
      perform_all (n - 1)
    end
  in
  let middle () =
    match_with perform_all iters
      { retc = (fun () -> ());
        exnc = raise;
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | F -> Some (fun (_ : (a, _) continuation) -> assert false)
            | _ ->
              (* Allocate in the forwarding window: small arrays, so they
                 come from the minor heap and dominate the loop's minor
                 allocation, making minor collections land here. *)
              for _ = 1 to 4 do
                sink := Array.make 100 0
              done;
              None) }
  in
  match_with middle ()
    { retc = (fun () -> ());
      exnc = raise;
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E n -> Some (fun (k : (a, _) continuation) -> continue k n)
          | _ -> None) };
  ignore (Sys.opaque_identity !sink);
  Printf.printf "reperform_idle: %d\n" !total

let () =
  test_mass ();
  test_discontinue ();
  test_nested ();
  test_callstack ();
  test_async_boundary ();
  test_dynamic ();
  test_locals ();
  test_chain3 ();
  test_live_nest_gc_count ();
  test_reperform_gc ~gc_inner:true ~gc_outer:true "reperform_gc_both";
  test_reperform_gc ~gc_inner:false ~gc_outer:true "reperform_gc_outer";
  test_reperform_idle ()
