(** Write-once variables whose readers are suspended until the variable is
    filled.

    Ivars can be merged, after which they behave as a single ivar. All mutations
    are reported to the log installed by [set_log], so they can be undone with
    [Change.undo]. Jobs enqueued on the scheduler are logged separately, via
    [Scheduler.set_log].

    Handlers may run in any order. *)

type 'a t

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [is_empty t] returns [true] if [t] is empty. *)
val is_empty : 'a t -> bool

(** [peek t] is [Some v] if [t] has been filled with [v], [None] otherwise. *)
val peek : 'a t -> 'a option

(** @raise Invalid_argument if [t] is empty. *)
val peek_exn : 'a t -> 'a

module Fill_result : sig
  type 'a t =
    | Ok
    | Already_full of 'a  (** Carries the existing contents. *)
end

(** [fill t v ~scheduler] fills [t] with [v] and enqueues the [run] of each
    waiting handler on [scheduler]. Does not run [scheduler]. If [t] is already
    full, [t] is unchanged. *)
val fill : 'a t -> 'a -> scheduler:Scheduler.t -> 'a Fill_result.t

(** [upon t ~run ~cancel ~scheduler] registers a handler on [t]. Each handler is
    either run or cancelled, exactly once:
    - [run v] once [t] is filled with [v]. If [t] is already full, it is
      enqueued immediately.
    - [cancel ()] if the handler is detached by {!cancel_all}.

    Safety: [cancel] must fill [t]. *)
val upon :
  'a t ->
  run:('a -> unit) ->
  cancel:(unit -> unit) ->
  scheduler:Scheduler.t ->
  unit

(** [cancel_all t ~scheduler] detaches all handlers waiting on [t] and enqueues
    each [cancel]. Does nothing if [t] is full. *)
val cancel_all : 'a t -> scheduler:Scheduler.t -> unit

(** [merge t1 t2 ~f ~scheduler] makes [t1] and [t2] the same ivar. If both are
    empty, their handlers are combined. If exactly one is full, the other's
    handlers are enqueued as if by {!fill}. If both are full, [f] combines their
    values. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> scheduler:Scheduler.t -> unit

include With_backtracking.S
