module Monitor = struct
  exception Recoverable of exn

  let recoverable e = Recoverable e

  type t =
    { goto : exn -> unit;
      frames : (exn -> exn) list  (** Exception decorators, innermost first *)
    }

  (** The exception currently jumping to a root, if any. *)
  let escaping : exn option ref = Local_store.s_ref None

  (* When omnidirectionality is disabled, jobs just raise normally. *)
  let disabled = { goto = raise; frames = [] }

  let none = { goto = raise; frames = [] }

  let current_ref : t ref = Local_store.s_ref none

  let current () =
    let t = !current_ref in
    if not (Repr.phys_equal t none)
    then t
    else if not (Language_extension.is_enabled Omnidirectionality)
    then disabled
    else
      Misc.fatal_error
        "Typing_scheduler.Monitor.current: current only defined under a root"

  let with_current t body =
    let saved = !current_ref in
    current_ref := t;
    (* This is [Fun.protect] inlined (no closures) *)
    match body () with
    | result ->
      current_ref := saved;
      result
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      current_ref := saved;
      Printexc.raise_with_backtrace exn bt

  let try_ body ~with_ =
    try
      if not (Language_extension.is_enabled Omnidirectionality)
      then body ()
      else
        let t = current () in
        with_current { t with frames = with_ :: t.frames } body
    with e -> (
      (* [with_] cannot match on the [Goto] exception *)
      match with_ e with
      | Recoverable e | e -> raise e)

  let root body =
    if not (Language_extension.is_enabled Omnidirectionality)
    then body ()
    else
      let exception Goto of exn in
      let goto exn =
        let exn = Goto exn in
        escaping := Some exn;
        raise exn
      in
      try with_current { frames = []; goto } body
      with Goto exn ->
        escaping := None;
        raise exn

  let run_job t job =
    if not (Language_extension.is_enabled Omnidirectionality)
    then job ()
    else
      try with_current t job
      with exn -> (
        match !escaping with
        | Some exn' when exn == exn' ->
          raise exn (* an inner job's escape; ignore it *)
        | _ -> (
          match
            List.fold_left (fun exn decorate -> decorate exn) exn t.frames
          with
          | Recoverable _ -> ()
          | exn -> t.goto exn))
end

module Job = struct
  type t = unit -> unit

  module With_monitor = struct
    type nonrec t =
      { job : t;
        monitor : Monitor.t
      }

    let create job ~monitor = { job; monitor }

    let run { monitor; job } = Monitor.run_job monitor job
  end

  module With_skip = struct
    type t =
      { job : With_monitor.t;
        mutable skip : bool
      }

    let create job = { job; skip = false }

    let run t = if not t.skip then With_monitor.run t.job
  end
end

type t = { jobs : Job.With_skip.t Queue.t }

module Change = struct
  (** To support backtracking in the scheduler, we need to backtrack on [add]
      and [take] operations:
      - For [add], we use a [skip] bit on jobs. The scheduler will clear up any
        skipped garbage jobs on [run]
      - For [take], we log the [job] taken from the scheduler.
      - For [clear], we log the jobs that were removed. *)
  type nonrec t =
    | Add of Job.With_skip.t
    | Take of t * Job.With_skip.t
    | Clear of t * Job.With_skip.t Queue.t

  let undo = function
    | Add job -> job.skip <- true
    | Take (t, job) ->
      (* [job]'s effects should be undone, thus requeuing is ok. *)
      Queue.add job t.jobs
    | Clear (t, jobs) ->
      (* Jobs added since are skipped; put the cleared ones before them. *)
      Queue.transfer t.jobs jobs;
      Queue.transfer jobs t.jobs
end

include With_backtracking.Make (Change)

let create () = { jobs = Queue.create () }

let add ?(monitor = Monitor.current ()) t job =
  if
    Language_extension.(
      is_at_least Omnidirectionality Omnidirectionality.Scheduler_policy.Delayed)
  then (
    let job = Job.(job |> With_monitor.create ~monitor |> With_skip.create) in
    log (Add job);
    Queue.add job t.jobs)
  else Monitor.run_job monitor job

let take t =
  let next_job = Queue.take t.jobs in
  (* Ignore skipped jobs *)
  if not next_job.skip then log (Take (t, next_job));
  next_job

let run t =
  while not (Queue.is_empty t.jobs) do
    let next_job = take t in
    Job.With_skip.run next_job
  done

let clear t =
  if not (Queue.is_empty t.jobs)
  then (
    let jobs = Queue.create () in
    Queue.transfer t.jobs jobs;
    log (Clear (t, jobs)))
