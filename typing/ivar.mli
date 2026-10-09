(** Write-once variables whose readers are suspended until the variable is
    filled.

    Ivars can be merged, after which they behave as a single ivar. All mutations
    are reported to the log installed by [set_log], so they can be undone with
    [Change.undo]. Jobs enqueued on the scheduler are logged separately, via
    [Scheduler.set_log].

    Handlers may run in any order. *)

type 'a t

(** An ivar of any content type. *)
type packed = Packed : 'a t -> packed

(** [create ~in_global_pool ()] returns an empty ivar, added to the
    {!Global_pool} iff [in_global_pool]. *)
val create : in_global_pool:bool -> unit -> 'a t

(** [create_full v] returns an ivar filled with [v]. It is not added to the
    {!Global_pool}. *)
val create_full : 'a -> 'a t

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
    - [cancel ()] if the handler is detached by {!cancel_all}. *)
val upon :
  'a t ->
  run:('a -> unit) ->
  cancel:(unit -> unit) ->
  scheduler:Scheduler.t ->
  unit

(** [upon_all t packeds ~scheduler] fills [t] once every ivar of [packeds] is
    full. If they already are, [t] is filled now. *)
val upon_all : unit t -> packed list -> scheduler:Scheduler.t -> unit

(** [drop_all_handlers ()] detaches the handlers of every empty ivar, without
    running or cancelling them. This is for when type checking is abandoned
    (e.g. after an error), so that ivars no longer contain closures and can be
    marshalled (e.g. into a [.cmt] file). *)
val drop_all_handlers : unit -> unit

(** [cancel_all t ~scheduler] detaches all handlers waiting on [t] and enqueues
    each [cancel]. Does nothing if [t] is full. *)
val cancel_all : 'a t -> scheduler:Scheduler.t -> unit

(** [merge t1 t2 ~f ~scheduler] makes [t1] and [t2] the same ivar. If both are
    empty, their handlers are combined. If exactly one is full, the other's
    handlers are enqueued as if by {!fill}. If both are full, [f] combines their
    values. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> scheduler:Scheduler.t -> unit


(** A pool of ivars that must be filled or cancelled, e.g. before the end of
    some scope. *)
module Global_pool : sig
  (** [add t] adds [t] to the pool. *)
  val add : packed -> unit

  (** [exists_empty ()] is [true] iff an ivar in the pool is empty. *)
  val exists_empty : unit -> bool

  (** [take ()] empties the pool, returning the ivars that were in it, most
      recently added first. *)
  val take : unit -> packed list
end

include With_backtracking.S
