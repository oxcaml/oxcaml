module Job = struct
  type t = unit -> unit

  module With_skip = struct
    type nonrec t =
      { job : t;
        mutable skip : bool
      }

    let create job = { job; skip = false }

    let run t = if not t.skip then t.job ()
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

let add t job =
  let job = Job.With_skip.create job in
  log (Add job);
  Queue.add job t.jobs

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
