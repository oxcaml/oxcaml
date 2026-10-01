(** A queue of pending jobs, used to run suspended computations for
    omnidirectional type inference. *)

module Job : sig
  type t = unit -> unit
end

type t

val create : unit -> t

(** [add t job] enqueues [job] at the back of [t]. *)
val add : t -> Job.t -> unit

(** [run t] runs jobs until [t] is empty, including any jobs enqueued by
    the jobs themselves.

    [run t] may be called re-entrantly (e.g. from within a job). The inner
    call drains the shared queue, including jobs enqueued before the job
    that called it, so the outer call may find [t] empty when the job
    returns.

    If a job raises, the exception propagates and the remaining jobs
    stay in [t]. *)
val run : t -> unit

(** [clear t] discards all pending jobs. If called from within a job, the
    enclosing [run t] returns once that job does. *)
val clear : t -> unit

include With_backtracking.S
