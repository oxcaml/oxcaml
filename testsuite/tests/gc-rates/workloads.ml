(* Synthetic workloads for gcrate.ml to measure.  Each runs in a child
   process, watched from outside; in testsuite mode ([manual] false)
   they print terse checks on themselves, and otherwise progress and
   META lines. *)

let check name ok detail =
  if ok then Printf.printf "%s: ok\n" name
  else Printf.printf "%s: FAIL (%s)\n" name detail

let secs ns = float_of_int ns /. 1e9
let rate words ns = if ns = 0 then 0.0 else float_of_int words /. secs ns

(* Debug and instrumented runtimes are much slower, so testsuite
   thresholds on rates are scaled by this. *)
let variant_factor =
  match Sys.runtime_variant () with "" -> 1.0 | _ -> 0.1

(* A single domain builds [live_gib] GiB of live heap of mixed structure,
   churns it for [seconds], and then compacts three times: at steady
   state (live data, garbage-laden heap), immediately again (live data,
   compacted heap), and after dropping the data (near-empty).
   Steady-state major heap is roughly 2.4x [live_gib] at the default
   space_overhead of 80. *)
module Heap = struct
  let slot_words = 2000

  (* De-minimis threshold for testsuite mode: live words / compact time. *)
  let min_compact_rate = 2e6 *. variant_factor

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

  let report_stat prefix (s : Gc.stat) =
    Printf.printf
      "META %s: minor_words=%.0f promoted_words=%.0f major_words=%.0f \
       minor_collections=%d major_collections=%d heap_words=%d \
       top_heap_words=%d compactions=%d forced_major_collections=%d\n"
      prefix s.minor_words s.promoted_words s.major_words s.minor_collections
      s.major_collections s.heap_words s.top_heap_words s.compactions
      s.forced_major_collections

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
  let measure_compaction ~manual clabel =
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

  let run ~manual ~live_gib ~seconds:duration ~seed () =
    (* Disable auto-compaction: Gc.full_major runs its last cycle with
       Compaction_auto, which fires once the roots are dropped and would
       compact the heap before the measured Gc.compact sees it. *)
    Gc.set { (Gc.get ()) with Gc.max_overhead = 1000000 };
    let target_words =
      int_of_float (live_gib *. (1024. *. 1024. *. 1024. /. 8.)) in
    let nslots = max 64 (target_words / slot_words) in
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
    let c1 = measure_compaction ~manual "steady" in
    let c2 = measure_compaction ~manual "compacted" in
    Array.fill slots 0 nslots Leaf;
    Array.fill recs 0 nslots None;
    let c3 = measure_compaction ~manual "emptied" in
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
end

(* Three classes of domain run for [seconds], as how a domain behaves
   outside a stop-the-world section affects how promptly it enters one
   and how much work it finds there:
   - allocators build the same mixed structures as [Heap], dropping
     most at once, keeping some in a table of their own, publishing some
     to a table shared by all allocators, and adopting some from it, so
     that there are pointers between the domains' heaps;
   - spinners compute without allocating;
   - idlers block on a condition variable, so that their backup threads
     take part in sections on their behalf.
   The main domain sleeps until the time is up. *)
module Domains = struct
  (* Domains are spawned in this order, so a domain's index (which is also
     its ring in the event stream) tells us its class. *)
  let class_of_index ~allocators ~spinners i =
    if i = 0 then "main"
    else if i <= allocators then "allocator"
    else if i <= allocators + spinners then "spinner"
    else "idler"

  let classes ~allocators ~spinners ~idlers =
    List.filter_map
      (fun (cls, n) -> if n > 0 then Some cls else None)
      [ "main", 1; "allocator", allocators; "spinner", spinners;
        "idler", idlers ]

  (* An allocator keeps about [own_slots * Heap.slot_words] words live. *)
  let own_slots = 512
  let shared_slots = 128

  let stop = Atomic.make false
  let misplaced = Atomic.make 0

  let check_index expected =
    if Domain.self_index () <> expected then Atomic.incr misplaced

  (* Returns the number of slots built. *)
  let allocator ~seed shared index () =
    check_index index;
    let rng = Random.State.make [| seed; index |] in
    let own = Array.make own_slots Heap.Leaf in
    let recs = Array.make own_slots None in
    let keep i s =
      own.(i) <- s;
      recs.(i) <- Heap.first_record s 0
    in
    let built = ref 0 in
    let sink = ref 0 in
    while not (Atomic.get stop) do
      let s = Heap.build_slot rng Heap.slot_words in
      incr built;
      (match Random.State.int rng 100 with
       | n when n < 8 -> keep (Random.State.int rng own_slots) s
       | n when n < 10 ->
         Atomic.set shared.(Random.State.int rng shared_slots) s
       | _ ->
         (* short-lived: consumed, then garbage *)
         sink := Heap.checksum s 0 !sink);
      if Random.State.int rng 100 < 2 then
        keep (Random.State.int rng own_slots)
          (Atomic.get shared.(Random.State.int rng shared_slots));
      (* mutation traffic on old records *)
      for _ = 1 to 8 do
        match recs.(Random.State.int rng own_slots) with
        | None -> ()
        | Some r ->
          r.hits <- r.hits + 1;
          r.score <- r.score +. 0.5
      done
    done;
    ignore (Sys.opaque_identity !sink);
    !built

  let spinner index () =
    check_index index;
    let x = ref 1 in
    while not (Atomic.get stop) do
      for _ = 1 to 1000 do
        x := ((!x * 1103515245) + 12345) land max_int
      done
    done;
    !x

  let idler mutex cond index () =
    check_index index;
    Mutex.lock mutex;
    while not (Atomic.get stop) do
      Condition.wait cond mutex
    done;
    Mutex.unlock mutex;
    0

  let run ~manual ~allocators ~spinners ~idlers ~seconds:duration ~seed () =
    (* Keep compaction, with its long sections, out of the measurement. *)
    Gc.set { (Gc.get ()) with Gc.max_overhead = 1000000 };
    let shared =
      Array.init shared_slots (fun _ -> Atomic.make Heap.Leaf) in
    let mutex = Mutex.create () in
    let cond = Condition.create () in
    let spawn n first body =
      List.init n (fun i -> Domain.spawn (body (first + i)))
    in
    let wall0 = Unix.gettimeofday () in
    let allocs = spawn allocators 1 (allocator ~seed shared) in
    let spins = spawn spinners (1 + allocators) spinner in
    let idles = spawn idlers (1 + allocators + spinners) (idler mutex cond) in
    Unix.sleepf duration;
    Atomic.set stop true;
    Mutex.lock mutex;
    Condition.broadcast cond;
    Mutex.unlock mutex;
    let built = List.map Domain.join allocs in
    List.iter (fun d -> ignore (Domain.join d)) (spins @ idles);
    let wall1 = Unix.gettimeofday () in
    if manual then begin
      Printf.printf
        "META config: allocators=%d spinners=%d idlers=%d duration=%.1f \
         seed=%d minor_heap_size=%d space_overhead=%d\n"
        allocators spinners idlers duration seed (Gc.get ()).minor_heap_size
        (Gc.get ()).space_overhead;
      Printf.printf "META run: wall=%.3f slots=%d misplaced=%d\n"
        (wall1 -. wall0)
        (List.fold_left ( + ) 0 built)
        (Atomic.get misplaced)
    end
    else begin
      check "domains placed"
        (Atomic.get misplaced = 0)
        (Printf.sprintf "%d domains not at their expected index"
           (Atomic.get misplaced));
      check "allocators ran"
        (List.for_all (fun n -> n > 0) built)
        (String.concat "," (List.map string_of_int built))
    end
end
