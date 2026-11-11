(* TEST
 include unix;
 include runtime_events;
 hasunix;
 no-tsan;
 native;
*)

(* Measure the rates at which the major GC marks and sweeps, the minor GC
   promotes, and the compactor compacts.

   The measurement is taken entirely from OUTSIDE the measured process:
   a child runs the workload with runtime events enabled, and the parent
   watches the child's event ring, accumulating phase spans and the
   per-slice work counters.  `gcrate watch DIR PID SECONDS` attaches the
   same watcher to any process started with OCAML_RUNTIME_EVENTS_START=1.
   Rates are whole-run averages over the watched window.

   Build a live heap of mixed structure, then churn it, while consuming my own
   runtime-events stream, accumulating time spent inside EV_MAJOR_SWEEP and
   EV_MAJOR_MARK spans, and per-slice sweep and mark work from the
   EV_C_MAJOR_SLICE_*_WORK counters. Finally measure
   compaction three times: at steady state (live data, garbage-laden heap),
   immediately again (live data, compacted heap), and after dropping the data
   (near-empty), timing the EV_COMPACT span and its sub-phases.

   With no arguments (testsuite mode): small heap, a few seconds, terse
   output checking the rates clear generous de-minimis thresholds.  With
   arguments (manual mode): gcrate LIVE-GIB SECONDS [SEED] prints a full
   report.  Steady-state major heap is roughly 2.4x LIVE-GIB at the default
   space_overhead of 80. *)

module RE = Runtime_events

let manual = Array.length Sys.argv > 1
let watch_mode = manual && Sys.argv.(1) = "watch"

let usage =
  "usage: gcrate [LIVE-GIB [SECONDS [SEED]]]\n\
   \n\
   Estimates the rates at which the OCaml major GC marks and sweeps,\n\
   the minor GC evacuates survivors,\n\
   and the compactor compacts. Builds LIVE-GIB GiB of live\n\
   mixed-structure heap, churns it for SECONDS seconds (default 300),\n\
   and then compacts twice, drops the roots, and compacts one more time.\n\
   \n\
   With no arguments: self-test mode, as run from the testsuite - 32 MiB\n\
   live heap, 3 s churn, terse pass/fail output against de-minimis rate\n\
   thresholds.\n"

let () =
  if manual then
    match Sys.argv.(1) with
    | "help" | "-help" | "--help" | "-h" ->
      print_string usage;
      exit 0
    | _ -> ()

let arg conv i default =
  if (not watch_mode) && Array.length Sys.argv > i then
    match conv Sys.argv.(i) with
    | Some v -> v
    | None ->
      Printf.eprintf "gcrate: bad argument %S (try --help)\n" Sys.argv.(i);
      exit 2
  else default

let live_gib = arg float_of_string_opt 1 0.03125 (* test mode: 32 MiB *)
let duration = arg float_of_string_opt 2 (if manual then 300.0 else 3.0)
let seed = arg int_of_string_opt 3 42

let target_words = int_of_float (live_gib *. (1024. *. 1024. *. 1024. /. 8.))
let slot_words = 2000
let nslots = max 64 (target_words / slot_words)

(* De-minimis thresholds for testsuite mode, 20-40x below rates measured on
   an idle x86-64 dev box in 2026 (mark ~200 Mwords/s, sweep ~1600 Mwords/s,
   roughly independent of heap size): only catastrophic regressions or broken
   measurement plumbing should fail.  Debug/instrumented runtimes are slower
   still. *)
let variant_factor =
  match Sys.runtime_variant () with "" -> 1.0 | _ -> 0.1

let min_mark_rate = 10e6 *. variant_factor
let min_sweep_rate = 40e6 *. variant_factor
let min_compact_rate = 2e6 *. variant_factor (* live words / compact time *)
let min_promote_rate = 2e6 *. variant_factor (* promoted words / minor time *)

(* ---------------- runtime-events consumption ---------------- *)

let phase_total : (RE.runtime_phase, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

let phase_open : (RE.runtime_phase, int) Hashtbl.t = Hashtbl.create 64

let counter_total : (RE.runtime_counter, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

let lost_total = ref 0

let ns_of ts = Int64.to_int (RE.Timestamp.to_int64 ts)

let tally tbl key v =
  let count, total =
    match Hashtbl.find_opt tbl key with
    | Some ct -> ct
    | None ->
      let ct = (ref 0, ref 0) in
      Hashtbl.add tbl key ct;
      ct
  in
  incr count;
  total := !total + v

let runtime_begin _ring ts phase = Hashtbl.replace phase_open phase (ns_of ts)

let runtime_end _ring ts phase =
  match Hashtbl.find_opt phase_open phase with
  | None -> () (* span began before we started consuming *)
  | Some t0 ->
    Hashtbl.remove phase_open phase;
    tally phase_total phase (ns_of ts - t0)

let runtime_counter _ring _ts counter v = tally counter_total counter v

let lost_events _ring n = lost_total := !lost_total + n

let callbacks =
  RE.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter
    ~lost_events ()

let cursor = ref None

let poll () =
  match !cursor with
  | None -> ()
  | Some c -> ignore (RE.read_poll c callbacks None)

let span phase =
  match Hashtbl.find_opt phase_total phase with
  | Some (c, ns) -> (!c, !ns)
  | None -> (0, 0)

(* ---------------- representative heap ---------------- *)

(* Shared immortal strings, like interned symbols/atoms. *)
let interned =
  let st = Random.State.make [| 0xbeef |] in
  Array.init 4096 (fun _ ->
      String.init
        (8 + Random.State.int st 24)
        (fun _ -> Char.chr (97 + Random.State.int st 26)))

type node =
  | Leaf
  | Str of string
  | Floats of floatarray
  | Ints of int array
  | Cons of node * node
  | Pair of node * node
  | Rec of record

and record =
  { id : int;
    name : string;
    mutable hits : int;
    mutable score : float; (* boxed *)
    kids : node array
  }

(* Builders track an approximate word budget so a "slot" lands near
   [slot_words] total. *)
type builder =
  { rng : Random.State.t;
    mutable budget : int
  }

let charge b w = b.budget <- b.budget - w

(* Uniform in [1, 2*mean-1]; mean is [mean]. *)
let rand_len b mean = 1 + Random.State.int b.rng ((2 * mean) - 1)

let pick_interned b =
  interned.(Random.State.int b.rng (Array.length interned))

(* Smart constructors: each charges the builder for the words it
   allocates (header included) as it constructs.  [build_leaf]
   allocates nothing but still charges a word, so that budgets always
   make progress; [build_interned] shares an existing string and
   charges nothing. *)

let build_leaf b = charge b 1; Leaf

let build_interned b = Str (pick_interned b)

let build_cons b x xs = charge b 3; Cons (x, xs)

let build_pair b x y = charge b 3; Pair (x, y)

let build_string b =
  let len = rand_len b 40 in
  charge b (2 + (len / 8));
  Str (String.make len (Char.chr (97 + Random.State.int b.rng 26)))

let build_floats b =
  let len = rand_len b 24 in
  charge b (1 + len);
  Floats (Float.Array.init len (fun i -> float_of_int i *. 1.5))

let build_ints b =
  let len = rand_len b 16 in
  charge b (1 + len);
  Ints (Array.init len (fun i -> i * 7))

let build_rec b ~id ~name ~score ~kids =
  charge b (6 + 2 + 1 + Array.length kids)
    (* record + boxed float + kids array *);
  Rec { id; name; hits = 0; score; kids }

let rec build b depth =
  if b.budget <= 0 || depth > 14 then build_leaf b
  else
    match Random.State.int b.rng 100 with
    | n when n < 25 ->
      (* a run of list cells holding small elements *)
      let len = rand_len b 24 in
      let tail = ref Leaf in
      for _ = 1 to len do
        let elt =
          match Random.State.int b.rng 10 with
          | 0 | 1 | 2 | 3 -> build_interned b
          | 4 -> build_string b
          | 5 -> build_pair b Leaf Leaf
          | _ -> Leaf
        in
        tail := build_cons b elt !tail
      done;
      !tail
    | n when n < 40 -> build_string b
    | n when n < 50 -> build_floats b
    | n when n < 57 -> build_ints b
    | n when n < 87 ->
      let nkids = 1 + Random.State.int b.rng 7 in
      build_rec b
        ~id:(Random.State.int b.rng 1000000)
        ~name:(pick_interned b)
        ~score:(Random.State.float b.rng 1.0)
        ~kids:(Array.init nkids (fun _ -> build b (depth + 1)))
    | _ ->
      build_pair b (build b (depth + 1)) (build b (depth + 1))

let build_slot rng budget =
  let b = { rng; budget } in
  let acc = ref Leaf in
  while b.budget > 0 do
    acc := build_cons b (build b 0) !acc
  done;
  !acc

let rec first_record node depth =
  if depth > 8 then None
  else
    match node with
    | Rec r -> Some r
    | Cons (a, b) | Pair (a, b) -> (
      match first_record a (depth + 1) with
      | Some _ as s -> s
      | None -> first_record b (depth + 1))
    | Leaf | Str _ | Floats _ | Ints _ -> None

(* Shallow walk: consumes freshly built ephemeral data so it can't be
   optimized away, and touches it like a real mutator would. *)
let rec checksum node depth acc =
  if depth > 4 then acc
  else
    match node with
    | Leaf -> acc + 1
    | Str s -> acc + String.length s
    | Floats f -> acc + Float.Array.length f
    | Ints a -> acc + Array.length a
    | Cons (a, b) | Pair (a, b) ->
      checksum b (depth + 1) (checksum a (depth + 1) acc)
    | Rec r -> acc + r.id

(* System load (1/5/15 min), for judging interference from other work on
   the machine.  Linux-only; reports "n/a" elsewhere. *)
let loadavg () =
  match open_in "/proc/loadavg" with
  | exception Sys_error _ -> "n/a"
  | ic ->
    let line = try input_line ic with End_of_file -> "" in
    close_in ic;
    (match String.split_on_char ' ' line with
     | a :: b :: c :: _ -> Printf.sprintf "%s/%s/%s" a b c
     | _ -> "n/a")

(* Disable auto-compaction: Gc.full_major runs its last cycle with
   Compaction_auto, which fires once the roots are dropped and would compact
   the heap before the measured Gc.compact sees it. *)
let () = Gc.set { (Gc.get ()) with Gc.max_overhead = 1000000 }

(* ---------------- reporting ---------------- *)

let secs ns = float_of_int ns /. 1e9
let rate words ns = if ns = 0 then 0.0 else float_of_int words /. secs ns

let report_stat prefix (s : Gc.stat) =
  Printf.printf
    "META %s: minor_words=%.0f promoted_words=%.0f major_words=%.0f \
     minor_collections=%d major_collections=%d heap_words=%d \
     top_heap_words=%d compactions=%d forced_major_collections=%d\n"
    prefix s.minor_words s.promoted_words s.major_words s.minor_collections
    s.major_collections s.heap_words s.top_heap_words s.compactions
    s.forced_major_collections

let report_rate what words ns =
  Printf.printf "%-8s %16d words in %9.3f s -> %7.1f Mwords/s (%.2f GiB/s)\n"
    what words (secs ns)
    (rate words ns /. 1e6)
    (rate words ns *. 8. /. (1024. *. 1024. *. 1024.))

(* Compaction measurement.  [mapped_before] estimates the mapped heap
   extent: sweeping parks fully-empty pools on an OS freelist that
   heap_words no longer counts, so we carry forward the previous
   compaction's heap_after (pools mapped then stay mapped until a
   compaction releases them). *)
type compaction =
  { clabel : string;
    live_before : int;
    heap_before : int;
    heap_after : int;
    runs : int; (* Gc.stat compactions delta: 1 if it really ran *)
    wall : float
  }

(* Compaction timing on the workload side is wall-clock: compaction is
   a stop-the-world phase, so this matches the EV_COMPACT span that the
   watcher sees, without needing event access from inside. *)
let measure_compaction clabel =
  if manual then Printf.printf "# compacting (%s)\n%!" clabel;
  Gc.full_major ();
  let b = Gc.quick_stat () in
  let t0 = Unix.gettimeofday () in
  Gc.compact ();
  let t1 = Unix.gettimeofday () in
  let a = Gc.quick_stat () in
  { clabel;
    live_before = b.live_words;
    heap_before = b.heap_words;
    heap_after = a.heap_words;
    runs = a.compactions - b.compactions;
    wall = t1 -. t0
  }

let report_compaction c =
  Printf.printf
    "META compaction %s: live_before=%d heap_before=%d heap_after=%d \
     runs=%d wall=%.3f\n"
    c.clabel c.live_before c.heap_before c.heap_after c.runs c.wall

let check name ok detail =
  if ok then Printf.printf "%s: ok\n" name
  else Printf.printf "%s: FAIL (%s)\n" name detail

(* ---------------- workload: the watched child ---------------- *)

let run_workload () =
  let rng = Random.State.make [| seed |] in
  if manual then
    Printf.printf "# building %.3f GiB live: %d slots of ~%d words\n%!"
      live_gib nslots slot_words;
  let t0 = Unix.gettimeofday () in
  let slots = Array.make nslots Leaf in
  let recs = Array.make nslots None in
  let tick = max 1 (nslots / 10) in
  for i = 0 to nslots - 1 do
    let s = build_slot rng slot_words in
    slots.(i) <- s;
    recs.(i) <- first_record s 0;
    if i mod tick = 0 then begin
      if manual && i / tick < 10 then
        Printf.printf "# build %d%%\n%!" (i / tick * 10)
    end
  done;
  if manual then
    Printf.printf "# build done in %.1f s\n%!" (Unix.gettimeofday () -. t0);
  Gc.full_major ();
  Gc.full_major ();
  let stat0 = Gc.quick_stat () in
  let wall0 = Unix.gettimeofday () in
  let cpu0 = Sys.time () in
  let iters = ref 0 in
  let sink = ref 0 in
  let deadline = wall0 +. duration in
  let next_decade = ref 1 in
  if manual then
    Printf.printf "# churn 0%% (heap %d Mwords)\n%!"
      ((Gc.quick_stat ()).heap_words / 1048576);
  (try
     while true do
       incr iters;
       (* replace one slot: ~slot_words of allocation that survives *)
       let i = Random.State.int rng nslots in
       let s = build_slot rng slot_words in
       slots.(i) <- s;
       recs.(i) <- first_record s 0;
       (* ~4x that in short-lived garbage *)
       for _ = 1 to 4 do
         sink := checksum (build_slot rng slot_words) 0 !sink
       done;
       (* mutation traffic on old records *)
       for _ = 1 to 8 do
         match recs.(Random.State.int rng nslots) with
         | None -> ()
         | Some r ->
           r.hits <- r.hits + 1;
           r.score <- r.score +. 0.5
       done;
       if !iters land 255 = 0 then begin
         let now = Unix.gettimeofday () in
         if now > deadline then raise Exit;
         if manual && !next_decade <= 9
            && now >= wall0 +. (duration *. float_of_int !next_decade /. 10.)
         then begin
           Printf.printf "# churn %d%% (heap %d Mwords)\n%!"
             (!next_decade * 10)
             ((Gc.quick_stat ()).heap_words / 1048576);
           incr next_decade
         end
       end
     done
   with Exit -> ());
  let wall1 = Unix.gettimeofday () in
  let cpu1 = Sys.time () in
  let stat1 = Gc.quick_stat () in
  let c1 = measure_compaction "steady" in
  let c2 = measure_compaction "compacted" in
  Array.fill slots 0 nslots Leaf;
  Array.fill recs 0 nslots None;
  let c3 = measure_compaction "emptied" in
  if manual then begin
    Printf.printf
      "META config: live_gib=%.3f duration=%.1f seed=%d slots=%d \
       slot_words=%d minor_heap_size=%d space_overhead=%d\n"
      live_gib duration seed nslots slot_words (Gc.get ()).minor_heap_size
      (Gc.get ()).space_overhead;
    Printf.printf "META churn: wall=%.3f cpu=%.3f iters=%d sink=%d\n"
      (wall1 -. wall0) (cpu1 -. cpu0) !iters !sink;
    report_stat "stat0" stat0;
    report_stat "stat1" stat1;
    report_compaction c1;
    report_compaction c2;
    report_compaction c3
  end
  else begin
    check "heap built" (stat1.heap_words >= target_words)
      (Printf.sprintf "heap_words=%d < target=%d" stat1.heap_words
         target_words);
    check "compaction runs"
      (c1.runs = 1 && c2.runs = 1 && c3.runs = 1
       && c1.heap_after < c1.heap_before
       && c3.live_before * 4 < c1.live_before)
      (Printf.sprintf "runs=%d,%d,%d heap=%d->%d live=%d->%d" c1.runs c2.runs
         c3.runs c1.heap_before c1.heap_after c1.live_before c3.live_before);
    check "compaction rate"
      (rate c1.live_before (int_of_float (c1.wall *. 1e9))
       >= min_compact_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f"
         (rate c1.live_before (int_of_float (c1.wall *. 1e9)) /. 1e6)
         (min_compact_rate /. 1e6))
  end

(* ---------------- watcher ---------------- *)

(* Attach to the ring of [pid] under [dir] and poll it until
   [still_running] turns false, then drain. *)
let watch_target dir pid ~still_running =
  let rec attach tries =
    match RE.create_cursor (Some (dir, pid)) with
    | c -> c
    | exception _ when tries > 0 -> Unix.sleepf 0.05; attach (tries - 1)
  in
  let c = attach 100 in
  cursor := Some c;
  let rec loop () =
    poll ();
    if still_running () then begin Unix.sleepf 0.02; loop () end
  in
  loop ();
  poll ();
  poll ();
  RE.free_cursor c;
  cursor := None

let report_watch ~test =
  let counter_tally c =
    match Hashtbl.find_opt counter_total c with
    | Some (n, t) -> (!n, !t)
    | None -> (0, 0)
  in
  let slices, sweep_work = counter_tally RE.EV_C_MAJOR_SLICE_SWEEP_WORK in
  let _, mark_work = counter_tally RE.EV_C_MAJOR_SLICE_MARK_WORK in
  let _, ephe_mark_work =
    counter_tally RE.EV_C_MAJOR_SLICE_EPHE_MARK_WORK in
  let _, ephe_sweep_work =
    counter_tally RE.EV_C_MAJOR_SLICE_EPHE_SWEEP_WORK in
  let _, ev_total = counter_tally RE.EV_C_MAJOR_SLICE_WORK_DONE in
  let phase_work = sweep_work + mark_work + ephe_mark_work
                   + ephe_sweep_work in
  (* Both minor counters are emitted in bytes. *)
  let minor_colls, promoted_bytes = counter_tally RE.EV_C_MINOR_PROMOTED in
  let promoted = promoted_bytes / 8 in
  let _, minor_allocated_bytes = counter_tally RE.EV_C_MINOR_ALLOCATED in
  let minor_allocated = minor_allocated_bytes / 8 in
  let sweep_spans, sweep_ns = span RE.EV_MAJOR_SWEEP in
  let mark_spans, mark_ns = span RE.EV_MAJOR_MARK in
  let minor_spans, minor_ns = span RE.EV_MINOR in
  let compact_spans, compact_ns = span RE.EV_COMPACT in
  if test then begin
    check "slices measured"
      (slices >= 10 && sweep_work > 0 && mark_work > 0 && sweep_spans > 0
       && mark_spans > 0 && !lost_total = 0)
      (Printf.sprintf
         "slices=%d sweep_words=%d mark_words=%d sweep_spans=%d \
          mark_spans=%d lost=%d"
         slices sweep_work mark_work sweep_spans mark_spans !lost_total);
    check "work cross-check"
      (ev_total > 0 && abs (phase_work - ev_total) * 20 <= ev_total)
      (Printf.sprintf "phases=%d work_done=%d" phase_work ev_total);
    check "sweep rate"
      (rate sweep_work sweep_ns >= min_sweep_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f"
         (rate sweep_work sweep_ns /. 1e6) (min_sweep_rate /. 1e6));
    check "mark rate"
      (rate mark_work mark_ns >= min_mark_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f"
         (rate mark_work mark_ns /. 1e6) (min_mark_rate /. 1e6));
    check "minor collections measured"
      (minor_spans >= 10 && minor_ns > 0 && promoted > 0)
      (Printf.sprintf "spans=%d ns=%d promoted=%d" minor_spans minor_ns
         promoted);
    check "promotion rate"
      (rate promoted minor_ns >= min_promote_rate)
      (Printf.sprintf "%.1f Mwords/s < %.1f"
         (rate promoted minor_ns /. 1e6) (min_promote_rate /. 1e6));
    check "compactions seen" (compact_spans = 3)
      (Printf.sprintf "compact_spans=%d" compact_spans)
  end
  else begin
    Printf.printf "\nwatched: slices=%d ephe_mark=%d ephe_sweep=%d \
                   lost_events=%d\n"
      slices ephe_mark_work ephe_sweep_work !lost_total;
    Printf.printf "work cross-check: per-phase sum %d vs work_done %d\n"
      phase_work ev_total;
    report_rate "sweep" sweep_work sweep_ns;
    report_rate "mark" mark_work mark_ns;
    Printf.printf
      "minor: %d collections in %.3f s (mean pause %.0f us), %d words \
       allocated, %d promoted (%.1f%% survival)\n"
      minor_colls (secs minor_ns)
      (if minor_spans = 0 then 0.
       else secs minor_ns *. 1e6 /. float_of_int minor_spans)
      minor_allocated promoted
      (if minor_allocated = 0 then 0.
       else 100. *. float_of_int promoted /. float_of_int minor_allocated);
    report_rate "promotion (minor live evacuation)" promoted minor_ns;
    Printf.printf "compaction: %d runs in %.3f s total\n"
      compact_spans (secs compact_ns);
    if mark_work > 0 && mark_ns > 0 && sweep_ns > 0 then
      Printf.printf
        "sweep/mark rate ratio: %.2f (runtime assumes \
         percent_sweep_per_mark=120, i.e. 1.20)\n"
        (rate sweep_work sweep_ns /. rate mark_work mark_ns)
  end

(* ---------------- dispatch ---------------- *)

let () =
  if watch_mode then begin
    (* gcrate watch DIR PID SECONDS: attach to any process started with
       OCAML_RUNTIME_EVENTS_START=1. *)
    let dir = Sys.argv.(2) in
    let pid = int_of_string Sys.argv.(3) in
    let secs = float_of_string Sys.argv.(4) in
    let deadline = Unix.gettimeofday () +. secs in
    let load0 = loadavg () in
    watch_target dir pid
      ~still_running:(fun () -> Unix.gettimeofday () < deadline);
    Printf.printf "META loadavg: %s -> %s\n" load0 (loadavg ());
    report_watch ~test:false
  end
  else begin
    let load0 = loadavg () in
    match Unix.fork () with
    | 0 ->
      (* Keep the ring file after exit so the watcher can drain it. *)
      Unix.putenv "OCAML_RUNTIME_EVENTS_PRESERVE" "1";
      RE.start ();
      run_workload ();
      exit 0
    | child ->
      let finished = ref false in
      let still_running () =
        !finished = false
        && (match Unix.waitpid [ Unix.WNOHANG ] child with
            | 0, _ -> true
            | _ -> finished := true; false)
      in
      watch_target "." child ~still_running;
      if not !finished then ignore (Unix.waitpid [] child);
      (try Sys.remove (string_of_int child ^ ".events")
       with Sys_error _ -> ());
      if manual then
        Printf.printf "META loadavg: %s -> %s\n" load0 (loadavg ());
      report_watch ~test:(not manual)
  end
