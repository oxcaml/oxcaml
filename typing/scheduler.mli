(** A queue of pending jobs, used to run suspended computations for
    omnidirectional type inference. *)

module Job : sig
  type t = unit -> unit
end

(** Monitors provide error handling contexts to jobs.

    A monitor is the stack of [with_]s of the enclosing {!try_}s, under a
    {!root}. An exception escaping a job is decorated by the job's whole
    monitor, then goes straight to the monitor's root: handlers between the
    scheduler and the root do not see it. *)
module Monitor : sig
  type t

  (** The current monitor. Inside a job, it starts from the job's monitor.
      Captured by {!Scheduler.add} and {!Ivar.upon}.

      Safety: Only defined under a monitor {!root}, unless omnidirectionality is
      disabled. *)
  val current : unit -> t

  (** [root body] runs [body] in a fresh, empty monitor. An exception escaping a
      job registered under [body] is re-raised from here as an ordinary
      exception.

      Jobs registered under [body] must run before it returns. *)
  val root : (unit -> 'a) -> 'a

  (** [try_ body ~with_] runs [body] with [with_] pushed onto the current
      monitor. [with_] decorates exceptions escaping [body].

      [with_] must not raise, and must return exceptions it doesn't recognise
      unchanged. *)
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

    If a job raises, the exception escapes to the job's {!Monitor.root} (see
    {!Monitor}), and the remaining jobs stay in [t]. *)
val run : t -> unit

include With_backtracking.S
