(** Write-once variables whose readers are suspended until the variable is
    filled.

    Ivars can be merged, after which they behave as a single ivar. Handlers may
    run in any order.

    {2 The global pool}

    An ivar created with [~in_global_pool:true] is registered in the
    {!Global_pool}, recording that it must eventually be filled or cancelled.
    The pool does not decide when that is: the client does, by draining it with
    {!Global_pool.take}.

    There is a single, global pool. It does not distinguish ivars by when or
    where they were created: {!Global_pool.take} returns every ivar registered
    since the previous [take]. Each entry carries a guard, which the client uses
    to decide whether the ivar must be cancelled now (see {!Global_pool.entry}).
*)

type 'a t

(** [create ~in_global_pool ?blocks ?created_at ()] returns an empty ivar, added
    to the {!Global_pool} iff [in_global_pool], with guard [blocks] (by default,
    [fun ~level:_ -> true]) and creation level [created_at] (by default, [0]).
    See {!Global_pool.entry}. *)
val create :
  in_global_pool:bool ->
  ?blocks:(level:int -> bool) ->
  ?created_at:int ->
  unit ->
  'a t

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

(** [cancel_all t ~scheduler] detaches all handlers waiting on [t] and enqueues
    each [cancel]. Does nothing if [t] is full. *)
val cancel_all : 'a t -> scheduler:Scheduler.t -> unit

(** [merge t1 t2 ~f ~scheduler] makes [t1] and [t2] the same ivar. If both are
    empty, their handlers are combined. If exactly one is full, the other's
    handlers are enqueued as if by {!fill}. If both are full, [f] combines their
    values. *)
val merge : 'a t -> 'a t -> f:('a -> 'a -> 'a) -> scheduler:Scheduler.t -> unit

(** An ivar of any content type. *)
type packed = Packed : 'a t -> packed

(** The global pool of ivars that must be filled or cancelled, described at the
    top of this module. *)
module Global_pool : sig
  (** An ivar in the pool. [blocks ~level] is [true] if the ivar must be
      cancelled before generalizing the region at [level], i.e. if a suspended
      constraint waiting on it captures something that would be generalized.
      [created_at] is the level at which the entry was created, used to decide
      which ivars to default at top level. *)
  type entry =
    { ivar : packed;
      blocks : level:int -> bool;
      created_at : int
    }

  (** [take ()] empties the pool, returning the entries that were in it. Like
      all mutations, this is logged, so backtracking restores the pool. *)
  val take : unit -> entry list

  (** [add entry] puts [entry] (e.g. taken by [take]) back into the pool. *)
  val add : entry -> unit
end

(** All mutations are reported to the log installed by [set_log], so they can be
    undone with [Change.undo]. Jobs enqueued on the scheduler are logged
    separately, via [Scheduler.set_log]. *)
include With_backtracking.S
(** @inline *)
