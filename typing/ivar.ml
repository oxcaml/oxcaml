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

  let merge t1 t2 ~f ~scheduler =
    match t1, t2 with
    | Empty callbacks1, Empty callbacks2 -> Empty (callbacks2 @ callbacks1)
    | Empty callbacks, (Full value as t) | (Full value as t), Empty callbacks ->
      List.iter (Callback.schedule_run ~value ~scheduler) callbacks;
      t
    | Full value1, Full value2 -> Full (f value1 value2)
end

type 'a t = 'a Cell.t Union_find.t

let create () = Union_find.create Cell.empty

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
    let cell = Cell.merge (cell t1) (cell t2) ~f ~scheduler in
    Union_find.union t1 t2;
    set_cell t1 cell)

include (Union_find : With_backtracking.S)
