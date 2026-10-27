module Monitor = struct
  (* Decorators, innermost first. *)
  type t = (exn -> exn) list

  let stack : t ref = Local_store.s_ref []

  let current () = !stack

  let run_in t body ~decorate =
    let saved = !stack in
    stack := t;
    match body () with
    | v ->
      stack := saved;
      v
    | exception e ->
      stack := saved;
      raise (decorate e)

  let try_ body ~with_ =
    if not (Language_extension.is_enabled Omnidirectionality)
    then try body () with e -> raise (with_ e)
    else run_in (with_ :: current ()) body ~decorate:with_

  (* Decorators of [t] still on the stack see the result again, and pass it
     through by the contract on [try_]. *)
  let run_job t job =
    run_in t job ~decorate:(fun e ->
        List.fold_left (fun e decorate -> decorate e) e t)
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
      - For [take], we log the [job] taken from the scheduler. *)
  type nonrec t =
    | Add of Job.With_skip.t
    | Take of t * Job.With_skip.t

  let undo = function
    | Add job -> job.skip <- true
    | Take (t, job) ->
      (* [job]'s effects should be undone, thus requeuing is ok. *)
      Queue.add job t.jobs
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
