module Union_find = Union_find.Make ()

module Callback = struct
  type 'a t =
    { run : 'a -> unit;
      cancel : unit -> unit
    }

  let create ~run ~cancel = { run; cancel }

  let schedule_run t ~value ~scheduler =
    Scheduler.add scheduler (fun () -> t.run value)

  let schedule_cancel t ~scheduler = Scheduler.add scheduler t.cancel
end

module Cell = struct
  type 'a t =
    | Empty of 'a Callback.t list
    | Full of 'a

  let empty = Empty []

  module After_merge = struct
    type 'a t =
      | Noop
      | Schedule of 'a * 'a Callback.t list

    let run t ~scheduler =
      match t with
      | Noop -> ()
      | Schedule (value, callbacks) ->
        List.iter (Callback.schedule_run ~value ~scheduler) callbacks
  end

  let merge (t1 : 'a t) (t2 : 'a t) ~(f : 'a -> 'a -> 'a) :
      'a t * 'a After_merge.t =
    match t1, t2 with
    | Empty callbacks1, Empty callbacks2 ->
      Empty (callbacks2 @ callbacks1), Noop
    | Empty callbacks, (Full value as t) | (Full value as t), Empty callbacks ->
      t, Schedule (value, callbacks)
    | Full value1, Full value2 -> Full (f value1 value2), Noop
end

type 'a t = 'a Cell.t Union_find.t

type packed = Packed : 'a t -> packed

type entry =
  { ivar : packed;
    blocks : level:int -> bool;
    created_at : int
  }

let global_pool : entry list ref = Local_store.s_ref []

module Change = struct
  type t =
    | Union_find of Union_find.Change.t
    | Global_pool_add
    | Global_pool_take of entry list

  let undo = function
    | Union_find change -> Union_find.Change.undo change
    | Global_pool_add -> global_pool := List.tl !global_pool
    | Global_pool_take pool -> global_pool := pool
end

module Log = With_backtracking.Make (Change)

let set_log log =
  Log.set_log log;
  Union_find.set_log (fun change -> log (Change.Union_find change))

let cell (t : 'a t) : 'a Cell.t = Union_find.get t

let set_cell (t : 'a t) (cell : 'a Cell.t) = Union_find.set t cell

let is_empty t = match cell t with Empty _ -> true | Full _ -> false

let peek t = match cell t with Empty _ -> None | Full value -> Some value

let peek_exn t =
  match cell t with
  | Empty _ -> invalid_arg "Ivar.peek_exn: empty ivar cell"
  | Full value -> value

module Fill_result = struct
  type 'a t =
    | Ok
    | Already_full of 'a
end

let fill t value ~scheduler : _ Fill_result.t =
  match cell t with
  | Full value' -> Already_full value'
  | Empty callbacks ->
    set_cell t (Full value);
    List.iter (Callback.schedule_run ~value ~scheduler) callbacks;
    Ok

let upon t ~run ~cancel ~scheduler =
  match cell t with
  | Full value -> Scheduler.add scheduler (fun () -> run value)
  | Empty callbacks ->
    set_cell t (Empty (Callback.create ~run ~cancel :: callbacks))

let cancel_all t ~scheduler =
  match cell t with
  | Full _value -> ()
  | Empty callbacks ->
    set_cell t Cell.empty;
    List.iter (Callback.schedule_cancel ~scheduler) callbacks

let merge t1 t2 ~f ~scheduler =
  if not (Union_find.same t1 t2)
  then (
    let cell, after = Cell.merge (cell t1) (cell t2) ~f in
    Union_find.union t1 t2;
    set_cell t1 cell;
    Cell.After_merge.run after ~scheduler)

module Global_pool = struct
  type nonrec entry = entry =
    { ivar : packed;
      blocks : level:int -> bool;
      created_at : int
    }

  let add entry =
    Log.log Global_pool_add;
    global_pool := entry :: !global_pool

  let take () =
    let pool = !global_pool in
    Log.log (Global_pool_take pool);
    global_pool := [];
    pool
end

let always_blocks ~level:_ = true

let create ~in_global_pool ?(blocks = always_blocks) ?(created_at = 0) () =
  let t = Union_find.create Cell.empty in
  if in_global_pool then Global_pool.add { ivar = Packed t; blocks; created_at };
  t
