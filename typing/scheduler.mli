(** A queue of pending jobs, used to run suspended computations for
    omnidirectional type inference.

    The scheduling policy is set by [-extension omnidirectionality]:
    - [Delayed] ([-extension omnidirectionality_delayed]): jobs are queued and
      only run by {!run}, in FIFO order.
    - [Eager], or the extension disabled: jobs run immediately in {!add}, so
      jobs added by a job run before that job's later siblings (depth-first,
      rather than FIFO). *)

module Job : sig
  type t = unit -> unit
end

type t

val create : unit -> t

(** [add t job] enqueues [job] at the back of [t] under the [Delayed] policy.
    Otherwise, it runs [job] immediately, before returning. *)
val add : t -> Job.t -> unit

(** [run t] runs jobs until [t] is empty, including any jobs enqueued by the
    jobs themselves. Only [Delayed] enqueues jobs, so otherwise this is a no-op.

    [run t] may be called re-entrantly (e.g. from within a job). The inner call
    drains the shared queue, including jobs enqueued before the job that called
    it, so the outer call may find [t] empty when the job returns.

    If a job raises, the exception propagates and the remaining jobs stay in
    [t]. *)
val run : t -> unit

include With_backtracking.S
