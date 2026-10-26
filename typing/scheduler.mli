(** A queue of pending jobs, used to run suspended computations for
    omnidirectional type inference. *)

module Job : sig
  type t = unit -> unit
end

(** Monitors provide error handling context to delayed jobs.

    Monitors decorate errors; they *do not* handle them. An exception escaping a
    job is decorated by the job's whole monitor, then propagates from wherever
    the scheduler runs the job, through any [try]s there. *)
module Monitor : sig
  type t

  (** The [with_]s of the enclosing [try_]s. Inside a job, this starts from the
      job's monitor. Captured by {!Scheduler.add} and {!Ivar.upon}. *)
  val current : unit -> t

  (** [try_ body ~with_] runs [body] with [with_] pushed onto the [current]
      monitor: [with_] decorates exceptions escaping [body], or escaping a job
      registered while [body] runs.

      [with_] may also see exceptions from unrelated jobs. It must not raise,
      must return exceptions it doesn't recognise unchanged, and must not match
      its own output. A decorator that keeps the constructor (e.g. adding a
      [Diff] to a trace) must be nested in a [try_] that converts it (e.g. into
      [Unify]), or it decorates twice. *)
  val try_ : (unit -> 'a) -> with_:(exn -> exn) -> 'a
end

type t

val create : unit -> t

(** [add ?monitor t job] enqueues [job] at the back of [t], to run in [monitor]
    (by default, [Monitor.current ()]). *)
val add : ?monitor:Monitor.t -> t -> Job.t -> unit

(** [run t] runs jobs until [t] is empty, including any jobs enqueued by the
    jobs themselves.

    [run t] may be called re-entrantly (e.g. from within a job). The inner call
    drains the shared queue, including jobs enqueued before the job that called
    it, so the outer call may find [t] empty when the job returns.

    If a job raises, the exception is decorated by the job's monitor (see
    {!Monitor}) and propagates, and the remaining jobs stay in [t]. *)
val run : t -> unit

include With_backtracking.S
