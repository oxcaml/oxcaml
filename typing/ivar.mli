(** Write-once variables whose readers are suspended until the variable is
    filled.

    Ivars can be merged, after which they behave as a single ivar. Handlers may
    run in any order. *)

type 'a t

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [is_empty t] returns [true] if [t] is empty. *)
val is_empty : 'a t -> bool

(** [has_handlers t] is [true] iff [t] is empty and has handlers attached. *)
val has_handlers : 'a t -> bool

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
    run, cancelled or dropped, at most once:
    - [run v] once [t] is filled with [v]. If [t] is already full, it is
      enqueued immediately.
    - [cancel ()] if [t] is cancelled by {!cancel_all}.
    - neither, if [t] is dropped by {!drop_all}.

    Both run in the monitor current when [upon] is called (see
    {!Scheduler.Monitor}), not the one current when [t] is filled or cancelled.
*)
val upon :
  'a t ->
  run:('a -> unit) ->
  cancel:(unit -> unit) ->
  scheduler:Scheduler.t ->
  unit

(** [cancel_all ts ~scheduler] detaches the handlers of every ivar in [ts], then
    enqueues each [cancel]. The ivars stay empty. *)
val cancel_all : 'a t list -> scheduler:Scheduler.t -> unit

(** [drop_all ts] detaches the handlers of every ivar in [ts]. *)
val drop_all : 'a t list -> unit

(** [merge t1 t2 ~f ~scheduler] makes [t1] and [t2] the same ivar. If both are
    empty, their handlers are combined. If exactly one is full, the other's
    handlers are enqueued as if by {!fill}. If both are full, [f] combines their
    values. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> scheduler:Scheduler.t -> unit

(** All mutations are reported to the log installed by [set_log], so they can be
    undone with [Change.undo]. Jobs enqueued on the scheduler are logged
    separately, via [Scheduler.set_log]. *)
include With_backtracking.S
(** @inline *)
