(* TEST
 modules = "workloads.ml";
 include unix;
 include runtime_events;
 hasunix;
 no-tsan;
 native-compiler;
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
 {
   run;
   check-program-output;
 }{
   multicore;
   arguments = "--self-test-domains";
   output = "domains.output";
   reference = "${test_source_directory}/gcrate.domains.reference";
   run;
   check-program-output;
 }
*)

(* Measure the rates at which the major GC marks and sweeps, the minor GC
   promotes, and the compactor compacts, and how the time domains spend
   in stop-the-world sections divides between work and waiting.

   The measurement is taken entirely from OUTSIDE the measured process,
   by watching its runtime-events ring and accumulating phase spans and
   counters.  All figures are whole-run totals over the watched window;
   times are summed over domains ("domain-time").

   `gcrate CMD ARG...` is a wrapper in the style of time(1): it runs CMD
   with runtime events enabled, passes its exit status through, and
   reports on stderr.  `gcrate --watch PID` attaches to a process already
   running with OCAML_RUNTIME_EVENTS_START=1.

   `gcrate --bench LIVE-GIB` and `gcrate --domains A,S,I` watch the
   synthetic workloads in workloads.ml, run in a forked child, which
   also prints its own progress and statistics.

   With no arguments (testsuite mode): the single-domain workload, small
   and for a few seconds, with terse output checking that the
   measurements clear generous de-minimis thresholds.
   `gcrate --self-test-domains` does the same for the multi-domain
   workload, which needs a runtime that can spawn domains. *)

module RE = Runtime_events

let check = Workloads.check
let secs = Workloads.secs
let rate = Workloads.rate

(* The name of the program using this module, for messages. *)
let me = Filename.remove_extension (Filename.basename Sys.executable_name)

(* Where reports go: they must keep out of the way of a wrapped
   command's output. *)
let out = ref stdout

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

(* De-minimis thresholds for testsuite mode, 20-40x below rates measured on
   an idle x86-64 dev box in 2026 (mark ~200 Mwords/s, sweep ~1600 Mwords/s,
   roughly independent of heap size): only catastrophic regressions or broken
   measurement plumbing should fail.  Debug/instrumented runtimes are slower
   still. *)
let min_mark_rate = 10e6 *. Workloads.variant_factor
let min_sweep_rate = 40e6 *. Workloads.variant_factor
(* promoted words / minor time *)
let min_promote_rate = 2e6 *. Workloads.variant_factor

(* ---------------- runtime-events consumption ---------------- *)

(* Span counts and durations, summed over all domains. *)
let phase_total : (RE.runtime_phase, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

(* Spans are tracked per ring (that is, per domain). *)
let phase_open : (int * RE.runtime_phase, int) Hashtbl.t = Hashtbl.create 64

let counter_total : (RE.runtime_counter, int ref * int ref) Hashtbl.t =
  Hashtbl.create 64

(* Counters emitted within an EV_MAJOR_MARK_OPPORTUNISTIC span, which
   describe the opportunistic slices of that span and not a major slice. *)
let opportunistic_total : (RE.runtime_counter, int ref * int ref) Hashtbl.t =
  Hashtbl.create 8

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

let span phase =
  match Hashtbl.find_opt phase_total phase with
  | Some (c, ns) -> (!c, !ns)
  | None -> (0, 0)

let counter_tally ?(tbl = counter_total) c =
  match Hashtbl.find_opt tbl c with
  | Some (n, t) -> (!n, !t)
  | None -> (0, 0)

(* ---------------- domain lifetimes ---------------- *)

(* Total domain-time: how long each domain existed within the watched
   window, summed over domains, as a yardstick for the other domain-times.
   A domain is taken to exist from the first event seen on its ring until
   its EV_DOMAIN_TERMINATE, or else the last event seen on any ring; a
   blocked domain still exists. *)
let domain_since : (int, int) Hashtbl.t = Hashtbl.create 64

(* Rings whose domain has terminated. A few events can follow the
   termination, so only a spawn brings such a ring back to life. *)
let domain_gone : (int, unit) Hashtbl.t = Hashtbl.create 64
let domains_seen = ref 0
let domains_ended_ns = ref 0
let last_ns = ref 0

let domain_seen ?(spawned = false) ring ns =
  if ns > !last_ns then last_ns := ns;
  if spawned then Hashtbl.remove domain_gone ring;
  if not (Hashtbl.mem domain_since ring || Hashtbl.mem domain_gone ring)
  then begin
    Hashtbl.add domain_since ring ns;
    incr domains_seen
  end

let domain_ended ring ns =
  match Hashtbl.find_opt domain_since ring with
  | None -> ()
  | Some t0 ->
    Hashtbl.remove domain_since ring;
    Hashtbl.replace domain_gone ring ();
    domains_ended_ns := !domains_ended_ns + (ns - t0)

let total_domain_ns () =
  Hashtbl.fold (fun _ t0 acc -> acc + (!last_ns - t0)) domain_since
    !domains_ended_ns

(* ---------------- stop-the-world accounting ---------------- *)

(* What a stop-the-world section was for, judged from the spans a domain
   opens inside it.  A major-cycle section embeds a minor collection and
   may embed a compaction, so the constructors are in increasing order
   of precedence.  A forced major cycle (Gc.full_major, Gc.compact) does
   all the cycle's remaining marking and sweeping inside its section,
   where a natural cycle only ends there. *)
type cause =
  | Other | Minor | Phase_change | Major_cycle | Forced_major | Compact

let cause_name = function
  | Other -> "other"
  | Minor -> "minor"
  | Phase_change -> "phase-change"
  | Major_cycle -> "major-cycle"
  | Forced_major -> "forced-major"
  | Compact -> "compact"

let causes =
  [ Minor; Phase_change; Major_cycle; Forced_major; Compact; Other ]

(* One domain's passage through one section. *)
type passage =
  { mutable inside : bool;
    mutable leader : bool;
    mutable cause : cause;
    mutable waits_open : int;
    mutable entry : int; (* EV_STW_API_BARRIER *)
    mutable barrier : int; (* EV_STW_GLOBAL_BARRIER *)
    mutable leave : int; (* EV_MINOR_LEAVE_BARRIER *)
    mutable opportunistic : int (* major GC work done while waiting *)
  }

(* Domain-time totals, in ns, for one (class, cause). *)
type row =
  { mutable sections : int; (* counted by their leaders *)
    mutable passages : int;
    mutable total : int;
    mutable r_entry : int;
    mutable r_barrier : int;
    mutable r_leave : int;
    mutable r_opportunistic : int
  }

let new_row () =
  { sections = 0; passages = 0; total = 0; r_entry = 0; r_barrier = 0;
    r_leave = 0; r_opportunistic = 0 }

let passages : (int, passage) Hashtbl.t = Hashtbl.create 64
let rows : (string * cause, row) Hashtbl.t = Hashtbl.create 64

(* Maps a ring (domain index) to a workload-defined class of domain. *)
let class_of_ring = ref (fun (_ : int) -> "all")

let passage ring =
  match Hashtbl.find_opt passages ring with
  | Some p -> p
  | None ->
    let p =
      { inside = false; leader = false; cause = Other; waits_open = 0;
        entry = 0; barrier = 0; leave = 0; opportunistic = 0 }
    in
    Hashtbl.add passages ring p;
    p

let row cls cause =
  match Hashtbl.find_opt rows (cls, cause) with
  | Some r -> r
  | None ->
    let r = new_row () in
    Hashtbl.add rows (cls, cause) r;
    r

let is_section : RE.runtime_phase -> bool = function
  | EV_STW_LEADER | EV_STW_HANDLER -> true
  | _ -> false

let is_wait : RE.runtime_phase -> bool = function
  | EV_STW_API_BARRIER | EV_STW_GLOBAL_BARRIER | EV_MINOR_LEAVE_BARRIER -> true
  | _ -> false

let stw_begin ring (phase : RE.runtime_phase) =
  let p = passage ring in
  if is_section phase then begin
    p.inside <- true;
    p.leader <- (phase = EV_STW_LEADER);
    p.cause <- Other;
    p.waits_open <- 0;
    p.entry <- 0;
    p.barrier <- 0;
    p.leave <- 0;
    p.opportunistic <- 0
  end
  else if p.inside then begin
    if is_wait phase then p.waits_open <- p.waits_open + 1;
    let cause =
      match phase with
      | EV_MINOR -> Minor
      | EV_MAJOR_GC_PHASE_CHANGE -> Phase_change
      | EV_MAJOR_GC_STW -> Major_cycle
      | EV_MAJOR_FINISH_CYCLE -> Forced_major
      | EV_COMPACT -> Compact
      | _ -> Other
    in
    p.cause <- max p.cause cause
  end

let stw_end ring (phase : RE.runtime_phase) ns =
  let p = passage ring in
  if p.inside then begin
    if is_wait phase then p.waits_open <- max 0 (p.waits_open - 1);
    match phase with
    | EV_STW_API_BARRIER -> p.entry <- p.entry + ns
    | EV_STW_GLOBAL_BARRIER -> p.barrier <- p.barrier + ns
    | EV_MINOR_LEAVE_BARRIER -> p.leave <- p.leave + ns
    | EV_MAJOR_MARK_OPPORTUNISTIC when p.waits_open > 0 ->
      p.opportunistic <- p.opportunistic + ns
    | EV_STW_LEADER | EV_STW_HANDLER ->
      p.inside <- false;
      let r = row (!class_of_ring ring) p.cause in
      if p.leader then r.sections <- r.sections + 1;
      r.passages <- r.passages + 1;
      r.total <- r.total + ns;
      r.r_entry <- r.r_entry + p.entry;
      r.r_barrier <- r.r_barrier + p.barrier;
      r.r_leave <- r.r_leave + p.leave;
      r.r_opportunistic <- r.r_opportunistic + p.opportunistic
    | _ -> ()
  end

(* ---------------- callbacks and watcher ---------------- *)

let runtime_begin ring ts phase =
  domain_seen ring (ns_of ts);
  Hashtbl.replace phase_open (ring, phase) (ns_of ts);
  stw_begin ring phase

let runtime_end ring ts phase =
  domain_seen ring (ns_of ts);
  match Hashtbl.find_opt phase_open (ring, phase) with
  | None -> () (* span began before we started consuming *)
  | Some t0 ->
    Hashtbl.remove phase_open (ring, phase);
    let ns = ns_of ts - t0 in
    tally phase_total phase ns;
    stw_end ring phase ns

let runtime_counter ring ts counter v =
  domain_seen ring (ns_of ts);
  let tbl =
    if Hashtbl.mem phase_open (ring, RE.EV_MAJOR_MARK_OPPORTUNISTIC)
    then opportunistic_total
    else counter_total
  in
  tally tbl counter v

let lifecycle ring ts (event : RE.lifecycle) _data =
  let ns = ns_of ts in
  domain_seen ~spawned:(event = EV_DOMAIN_SPAWN) ring ns;
  if event = EV_DOMAIN_TERMINATE then domain_ended ring ns

let lost_events _ring n = lost_total := !lost_total + n

let callbacks =
  RE.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter
    ~lifecycle ~lost_events ()

let cursor = ref None

let poll () =
  match !cursor with
  | None -> ()
  | Some c -> ignore (RE.read_poll c callbacks None)

(* Attach to the ring of [pid] under [dir] and poll it until
   [still_running] turns false, then drain.  Returns false if there was
   no ring to attach to.

   The target may still be creating its ring: the file can be missing, or
   present but with its metadata not yet written, which a cursor reports
   as a corrupt stream on the first poll.  Back off and reopen in both
   cases, for as long as the target runs and a little longer. *)
let watch_target dir pid ~still_running =
  let try_attach () =
    match RE.create_cursor (Some (dir, pid)) with
    | exception Failure _ -> None
    | c ->
      cursor := Some c;
      match poll () with
      | () -> Some c
      | exception Failure msg when msg = "Runtime_events: corrupt stream" ->
        cursor := None;
        RE.free_cursor c;
        None
  in
  let rec attach tries =
    match try_attach () with
    | Some _ as c -> c
    | None when tries = 0 -> None
    | None ->
      Unix.sleepf 0.05;
      attach (if still_running () then tries else tries - 1)
  in
  match attach 3 with
  | None -> false
  | Some c ->
    let rec loop () =
      poll ();
      if still_running () then begin Unix.sleepf 0.02; loop () end
    in
    loop ();
    poll ();
    poll ();
    RE.free_cursor c;
    cursor := None;
    true

let no_ring dir pid =
  Printf.eprintf
    "%s: no runtime-events ring for process %d in %s (not an OCaml 5 \
     program?)\n"
    me pid dir

(* ---------------- GC rate report ---------------- *)

(* One row of the table of rates. *)
let report_rate what words ns =
  Printf.fprintf !out "%-13s %16d %9.3f %9.1f %8.2f\n"
    what words (secs ns)
    (rate words ns /. 1e6)
    (rate words ns *. 8. /. (1024. *. 1024. *. 1024.))

(* [compactions] is the number of compactions the workload runs. *)
let report_gc ~test ~compactions =
  let _, sweep_work = counter_tally RE.EV_C_MAJOR_SLICE_SWEEP_WORK in
  let _, mark_work = counter_tally RE.EV_C_MAJOR_SLICE_MARK_WORK in
  let _, ephe_mark_work =
    counter_tally RE.EV_C_MAJOR_SLICE_EPHE_MARK_WORK in
  let _, ephe_sweep_work =
    counter_tally RE.EV_C_MAJOR_SLICE_EPHE_SWEEP_WORK in
  (* The per-phase counters are only emitted when non-zero, so count
     slices by the total, which every slice emits. *)
  let slices, ev_total = counter_tally RE.EV_C_MAJOR_SLICE_WORK_DONE in
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
    check "compactions seen" (compact_spans = compactions)
      (Printf.sprintf "compact_spans=%d" compact_spans)
  end
  else begin
    Printf.fprintf !out "\nwatched: slices=%d ephe_mark=%d ephe_sweep=%d \
                        lost_events=%d\n"
      slices ephe_mark_work ephe_sweep_work !lost_total;
    Printf.fprintf !out
      "work cross-check: per-phase sum %d vs work_done %d\n"
      phase_work ev_total;
    Printf.fprintf !out "%-13s %16s %9s %9s %8s\n"
      "" "words" "seconds" "Mwords/s" "GiB/s";
    report_rate "sweep" sweep_work sweep_ns;
    report_rate "mark" mark_work mark_ns;
    report_rate "promotion" promoted minor_ns;
    (* Opportunistic work is in neither rate above: its span is not
       divided between sweeping and marking. *)
    let opp tbl_counter = snd (counter_tally ~tbl:opportunistic_total
                                 tbl_counter) in
    let opp_spans, opp_ns = span RE.EV_MAJOR_MARK_OPPORTUNISTIC in
    Printf.fprintf !out
      "opportunistic: %d sweep + %d mark words (work_done %d) in %d spans, \
       %.3f s\n"
      (opp RE.EV_C_MAJOR_SLICE_SWEEP_WORK) (opp RE.EV_C_MAJOR_SLICE_MARK_WORK)
      (opp RE.EV_C_MAJOR_SLICE_WORK_DONE) opp_spans (secs opp_ns);
    Printf.fprintf !out
      "minor: %d collections in %.3f s (mean pause %.0f us), %d words \
       allocated, %d promoted (%.1f%% survival)\n"
      minor_colls (secs minor_ns)
      (if minor_spans = 0 then 0.
       else secs minor_ns *. 1e6 /. float_of_int minor_spans)
      minor_allocated promoted
      (if minor_allocated = 0 then 0.
       else 100. *. float_of_int promoted /. float_of_int minor_allocated);
    Printf.fprintf !out "compaction: %d runs in %.3f s total\n"
      compact_spans (secs compact_ns);
    if mark_work > 0 && mark_ns > 0 && sweep_ns > 0 then
      Printf.fprintf !out
        "sweep/mark rate ratio: %.2f (runtime assumes \
         percent_sweep_per_mark=120, i.e. 1.20)\n"
        (rate sweep_work sweep_ns /. rate mark_work mark_ns)
  end

(* ---------------- stop-the-world report ---------------- *)

let sum_rows keep =
  let acc = new_row () in
  Hashtbl.iter
    (fun key r ->
       if keep key then begin
         acc.sections <- acc.sections + r.sections;
         acc.passages <- acc.passages + r.passages;
         acc.total <- acc.total + r.total;
         acc.r_entry <- acc.r_entry + r.r_entry;
         acc.r_barrier <- acc.r_barrier + r.r_barrier;
         acc.r_leave <- acc.r_leave + r.r_leave;
         acc.r_opportunistic <- acc.r_opportunistic + r.r_opportunistic
       end)
    rows;
  acc

let waits r = r.r_entry + r.r_barrier + r.r_leave

(* Time spent waiting and not doing opportunistic major GC work. *)
let idle r = max 0 (waits r - r.r_opportunistic)

(* With a single domain there is no waiting to report. *)
let print_row ~single label r =
  if r.passages = 0 then ()
  else if single then
    Printf.fprintf !out "%-13s %8d %9.3f\n" label r.sections (secs r.total)
  else
    Printf.fprintf !out
      "%-13s %8d %9d %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %6.1f%%\n"
      label r.sections r.passages (secs r.total) (secs r.r_entry)
      (secs r.r_barrier) (secs r.r_leave) (secs r.r_opportunistic)
      (secs (r.total - waits r))
      (100. *. float_of_int (idle r) /. float_of_int (max 1 r.total))

(* [classes] lists the classes of domain the workload runs. *)
let report_stw ~test ~classes =
  let all = sum_rows (fun _ -> true) in
  if test then begin
    let minor = sum_rows (fun (_, cause) -> cause = Minor) in
    let missing =
      List.filter
        (fun cls -> (sum_rows (fun (c, _) -> c = cls)).passages = 0)
        classes
    in
    check "stw sections measured"
      (minor.sections >= 10 && missing = [])
      (Printf.sprintf "minor sections=%d, classes not seen: %s" minor.sections
         (String.concat "," missing));
    let api, _ = span RE.EV_STW_API_BARRIER in
    let global, _ = span RE.EV_STW_GLOBAL_BARRIER in
    let leave, _ = span RE.EV_MINOR_LEAVE_BARRIER in
    check "barrier waits measured" (api > 0 && global > 0 && leave > 0)
      (Printf.sprintf "api=%d global=%d leave=%d" api global leave);
    check "stw accounting" (all.total > 0 && waits all <= all.total)
      (Printf.sprintf "total=%d waits=%d" all.total (waits all))
  end
  else begin
    let single = !domains_seen <= 1 in
    let print_row = print_row ~single in
    if single then
      Printf.fprintf !out
        "\nStop-the-world sections: time in seconds (lost_events=%d)\n\
         %-13s %8s %9s\n"
        !lost_total "" "sections" "total"
    else
      Printf.fprintf !out
        "\nStop-the-world sections: domain-time in seconds \
         (lost_events=%d)\n\
         %-13s %8s %9s %9s %9s %9s %9s %9s %9s %7s\n"
        !lost_total "" "sections" "passages" "total" "entry" "barrier"
        "leave" "opport." "work" "idle";
    List.iter
      (fun cause ->
         print_row (cause_name cause) (sum_rows (fun (_, c) -> c = cause)))
      causes;
    print_row "total" all;
    if List.length classes > 1 then begin
      Printf.fprintf !out "by class of domain:\n";
      List.iter
        (fun cls -> print_row cls (sum_rows (fun (c, _) -> c = cls)))
        classes
    end;
    let domain_ns = total_domain_ns () in
    let percent ns =
      100. *. float_of_int ns /. float_of_int (max 1 domain_ns) in
    if single then
      Printf.fprintf !out "total time %.3f s: %.1f%% in sections\n"
        (secs domain_ns) (percent all.total)
    else begin
      Printf.fprintf !out
        "total domain-time %.3f s over %d domains: %.1f%% in sections, \
         %.1f%% idle in them\n"
        (secs domain_ns) !domains_seen (percent all.total)
        (percent (idle all));
      Printf.fprintf !out
        "entry: waiting to enter a section; barrier: at a barrier inside \
         one; leave: leaving a minor GC.\n\
         opport.: major GC work done while waiting, included in the wait \
         times. work = total - waits.\n\
         idle%% = (waits - opport.) / total\n"
    end
  end

(* ---------------- running ---------------- *)

(* Fork [workload] as a child with runtime events enabled, watch it until
   it exits, then call [report]. *)
let run ~manual ~workload ~report =
  let load0 = loadavg () in
  (* The child must not inherit buffered output. *)
  flush stdout;
  flush stderr;
  match Unix.fork () with
  | 0 ->
    (* Keep the ring file after exit so the watcher can drain it. *)
    Unix.putenv "OCAML_RUNTIME_EVENTS_PRESERVE" "1";
    RE.start ();
    workload ();
    exit 0
  | child ->
    let finished = ref false in
    let still_running () =
      !finished = false
      && (match Unix.waitpid [ Unix.WNOHANG ] child with
          | 0, _ -> true
          | _ -> finished := true; false)
    in
    let attached = watch_target "." child ~still_running in
    if not !finished then ignore (Unix.waitpid [] child);
    (try Sys.remove (string_of_int child ^ ".events")
     with Sys_error _ -> ());
    if not attached then begin
      no_ring "." child;
      exit 1
    end;
    if manual then
      Printf.printf "META loadavg: %s -> %s\n" load0 (loadavg ());
    report ()

(* Attach to process [pid], which must have been started with
   OCAML_RUNTIME_EVENTS_START=1 and have its ring in [dir], until it
   exits or [seconds] have passed, then call [report]. *)
let watch_pid pid ~dir ~seconds ~report =
  let deadline =
    match seconds with
    | Some secs -> Unix.gettimeofday () +. secs
    | None -> infinity
  in
  let alive () =
    match Unix.kill pid 0 with
    | () -> true
    | exception Unix.Unix_error (Unix.EPERM, _, _) -> true
    | exception Unix.Unix_error _ -> false
  in
  let load0 = loadavg () in
  if not
       (watch_target dir pid
          ~still_running:(fun () ->
            Unix.gettimeofday () < deadline && alive ()))
  then begin
    no_ring dir pid;
    exit 1
  end;
  Printf.printf "META loadavg: %s -> %s\n" load0 (loadavg ());
  report ()

(* Run [cmd] with runtime events enabled, watch it, call [report], and
   exit with its status. *)
let run_command cmd ~report =
  let prog = List.hd cmd in
  (* The ring file goes in a private directory, unless the caller chose
     one, and is kept past the command's exit so that it can be drained;
     it is left behind only if the caller asked for that. *)
  let dir, private_dir =
    match Sys.getenv_opt "OCAML_RUNTIME_EVENTS_DIR" with
    | Some dir -> (dir, false)
    | None -> (Filename.temp_dir "gcrate" "", true)
  in
  let preserve = Sys.getenv_opt "OCAML_RUNTIME_EVENTS_PRESERVE" <> None in
  let ours = String.starts_with ~prefix:"OCAML_RUNTIME_EVENTS_" in
  let env =
    Array.of_list
      ("OCAML_RUNTIME_EVENTS_START=1"
       :: "OCAML_RUNTIME_EVENTS_PRESERVE=1"
       :: ("OCAML_RUNTIME_EVENTS_DIR=" ^ dir)
       :: List.filter (fun v -> not (ours v))
            (Array.to_list (Unix.environment ())))
  in
  let remove file =
    try Sys.remove (Filename.concat dir file) with Sys_error _ -> ()
  in
  (* Rings in [dir] other than the command's own: its OCaml descendants
     inherit the environment and so leave theirs here too. *)
  let other_rings pid =
    if private_dir then
      List.filter
        (fun f ->
          Filename.check_suffix f ".events"
          && f <> string_of_int pid ^ ".events")
        (Array.to_list (Sys.readdir dir))
    else []
  in
  let cleanup pid =
    if not preserve then remove (string_of_int pid ^ ".events");
    if private_dir then begin
      List.iter remove (other_rings pid);
      try Sys.rmdir dir with Sys_error _ -> ()
    end
  in
  let pid =
    try
      Unix.create_process_env prog (Array.of_list cmd) env Unix.stdin
        Unix.stdout Unix.stderr
    with Unix.Unix_error (e, _, _) ->
      Printf.eprintf "%s: cannot run %s: %s\n" me prog
        (Unix.error_message e);
      cleanup 0;
      exit 127
  in
  (* Like time(1), leave terminal interrupts to the command, and report on
     whatever it did before it stopped. *)
  Sys.Safe.set_signal Sys.sigint Sys.Signal_ignore;
  Sys.Safe.set_signal Sys.sigquit Sys.Signal_ignore;
  let load0 = loadavg () in
  let t0 = Unix.gettimeofday () in
  let status = ref None in
  let still_running () =
    !status = None
    && (match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ -> true
        | _, st -> status := Some st; false)
  in
  let attached = watch_target dir pid ~still_running in
  let status =
    match !status with
    | Some st -> st
    | None -> snd (Unix.waitpid [] pid)
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  let others = other_rings pid in
  cleanup pid;
  if attached then begin
    report ();
    Printf.fprintf !out "elapsed: %.3f s\n" elapsed;
    Printf.fprintf !out "META loadavg: %s -> %s\n" load0 (loadavg ())
  end
  else no_ring dir pid;
  if others <> [] then
    Printf.eprintf
      "%s: ignored %d other process(es) with runtime events, started \
       by %s\n"
      me (List.length others) prog;
  flush !out;
  match status with
  | Unix.WEXITED n -> exit n
  | Unix.WSIGNALED signal ->
    (* Die the same way, so that our own caller sees the signal. *)
    Sys.Safe.set_signal signal Sys.Signal_default;
    Unix.kill (Unix.getpid ()) signal;
    exit 255
  | Unix.WSTOPPED _ -> exit 255

(* ---------------- command line ---------------- *)

let usage =
  {|usage: gcrate [--] CMD [ARG...]
       gcrate --watch PID [--dir DIR] [--seconds SECONDS]
       gcrate --bench LIVE-GIB [--seconds SECONDS] [--seed SEED]
       gcrate --domains A,S,I [--seconds SECONDS] [--seed SEED]
       gcrate [--self-test-domains]

Estimates the rates at which the OCaml major GC marks and sweeps,
the minor GC evacuates survivors, and the compactor compacts, and
the time domains spend working and waiting in stop-the-world
sections, from the runtime events of a watched process.

CMD [ARG...]
    Run an OCaml program with runtime events enabled, like time(1):
    the report goes to stderr and the exit status is CMD's.  Options
    are only recognised before CMD; -- ends them explicitly.
--watch PID
    Attach to process PID, which must have been started with
    OCAML_RUNTIME_EVENTS_START=1, until it exits.
--bench LIVE-GIB
    Run the built-in single-domain workload: build LIVE-GIB GiB of
    live mixed-structure heap, churn it, then compact twice, drop the
    roots, and compact once more.
--domains A,S,I
    Run the built-in multi-domain workload: A domains allocate, S
    domains compute without allocating, and I domains are blocked.
    The stop-the-world table is also broken down by class of domain.
--dir DIR
    With --watch: the directory holding PID's ring file (default
    $OCAML_RUNTIME_EVENTS_DIR, or the current directory).
--seconds SECONDS
    With --watch: stop watching after SECONDS.  With --bench or
    --domains: run for SECONDS (default 300 and 30).
--seed SEED
    With --bench or --domains: random seed (default 42).
-h, --help
    Print this message.

With no arguments: self-test, as run from the testsuite - the
single-domain workload, small and for a few seconds, with terse
pass/fail output against de-minimis thresholds.  With
--self-test-domains: the same for the multi-domain workload.
|}

let usage_error msg =
  Printf.eprintf "gcrate: %s (try --help)\n" msg;
  exit 2

type mode =
  | Self_test
  | Self_test_domains
  | Bench of float
  | Domains of int * int * int
  | Watch of int
  | Command of string list

(* Options stop at the first argument which is not one, so that the
   wrapped command keeps its own. *)
let mode, opt_dir, opt_seconds, opt_seed =
  let bench = ref None and domains = ref None and watch = ref None in
  let self_test_domains = ref false in
  let dir = ref None and seconds = ref None and seed = ref None in
  let set r conv opt v =
    match conv v with
    | Some x -> r := Some x
    | None -> usage_error (Printf.sprintf "bad argument %S to %s" v opt)
  in
  let counts v =
    match List.map int_of_string_opt (String.split_on_char ',' v) with
    | [ Some a; Some s; Some i ] when a >= 0 && s >= 0 && i >= 0 ->
      Some (a, s, i)
    | _ -> None
  in
  let rec parse = function
    | [] -> []
    | [ "--" ] -> usage_error "no command after --"
    | "--" :: cmd -> cmd
    | ("-h" | "--help") :: _ ->
      print_string usage;
      exit 0
    | "--self-test-domains" :: rest -> self_test_domains := true; parse rest
    | ("--bench" as o) :: v :: rest ->
      set bench float_of_string_opt o v; parse rest
    | ("--domains" as o) :: v :: rest -> set domains counts o v; parse rest
    | ("--watch" as o) :: v :: rest ->
      set watch int_of_string_opt o v; parse rest
    | "--dir" :: v :: rest -> dir := Some v; parse rest
    | ("--seconds" as o) :: v :: rest ->
      set seconds float_of_string_opt o v; parse rest
    | ("--seed" as o) :: v :: rest ->
      set seed int_of_string_opt o v; parse rest
    | [ ("--bench" | "--domains" | "--watch" | "--dir" | "--seconds"
        | "--seed") as o ] ->
      usage_error (o ^ " needs an argument")
    | o :: _ when String.starts_with ~prefix:"-" o ->
      usage_error ("unknown option " ^ o)
    | cmd -> cmd
  in
  let cmd = parse (List.tl (Array.to_list Sys.argv)) in
  let mode =
    match cmd, !bench, !domains, !watch with
    | _ when !self_test_domains ->
      if cmd = [] && !bench = None && !domains = None && !watch = None
      then Self_test_domains
      else usage_error "--self-test-domains takes nothing else"
    | [], None, None, None -> Self_test
    | [], Some live_gib, None, None -> Bench live_gib
    | [], None, Some (a, s, i), None -> Domains (a, s, i)
    | [], None, None, Some pid -> Watch pid
    | _ :: _, None, None, None -> Command cmd
    | _ ->
      usage_error "give only one of CMD, --watch, --bench and --domains"
  in
  let only opt given ok =
    if given && not ok then
      usage_error (opt ^ " does not apply here")
  in
  let watching, workload =
    match mode with
    | Watch _ -> (true, false)
    | Bench _ | Domains _ -> (false, true)
    | Self_test | Self_test_domains | Command _ -> (false, false)
  in
  only "--dir" (!dir <> None) watching;
  only "--seconds" (!seconds <> None) (watching || workload);
  only "--seed" (!seed <> None) workload;
  (mode, !dir, !seconds, Option.value !seed ~default:42)

(* ---------------- dispatch ---------------- *)

let report_all () =
  report_gc ~test:false ~compactions:0;
  report_stw ~test:false ~classes:[]

let run_heap ~manual ~live_gib ~seconds =
  run ~manual
    ~workload:(Workloads.Heap.run ~manual ~live_gib ~seconds ~seed:opt_seed)
    ~report:(fun () ->
      report_gc ~test:(not manual) ~compactions:3;
      if manual then report_stw ~test:false ~classes:[])

let run_domains ~manual ~allocators ~spinners ~idlers ~seconds =
  class_of_ring := Workloads.Domains.class_of_index ~allocators ~spinners;
  let classes = Workloads.Domains.classes ~allocators ~spinners ~idlers in
  run ~manual
    ~workload:
      (Workloads.Domains.run ~manual ~allocators ~spinners ~idlers ~seconds
         ~seed:opt_seed)
    ~report:(fun () ->
      if manual then report_gc ~test:false ~compactions:0;
      report_stw ~test:(not manual) ~classes)

let () =
  match mode with
  | Command cmd ->
    out := stderr;
    run_command cmd ~report:report_all
  | Watch pid ->
    let dir =
      match opt_dir, Sys.getenv_opt "OCAML_RUNTIME_EVENTS_DIR" with
      | Some dir, _ | None, Some dir -> dir
      | None, None -> "."
    in
    watch_pid pid ~dir ~seconds:opt_seconds ~report:report_all
  | Bench live_gib ->
    run_heap ~manual:true ~live_gib
      ~seconds:(Option.value opt_seconds ~default:300.0)
  | Domains (allocators, spinners, idlers) ->
    run_domains ~manual:true ~allocators ~spinners ~idlers
      ~seconds:(Option.value opt_seconds ~default:30.0)
  | Self_test ->
    run_heap ~manual:false ~live_gib:0.03125 (* 32 MiB *) ~seconds:3.0
  | Self_test_domains ->
    run_domains ~manual:false ~allocators:2 ~spinners:1 ~idlers:1
      ~seconds:2.0
