module Union_find = Union_find.Make ()

module Handler = struct
  type 'a t =
    { run : 'a -> unit;
      cancel : unit -> unit;
      monitor : Typing_scheduler.Monitor.t;
      guard : Typing_guard.t
    }

  let create ~run ~cancel ~guard =
    Typing_guard.acquire guard;
    { run; cancel; monitor = Typing_scheduler.Monitor.current (); guard }

  (* [schedule t job] passes [t]'s hold on its guard to [job]: [job] acquires
     it before [t] releases it. Under the eager policy, [job] runs (and may
     raise) before [Typing_scheduler.add] returns. *)
  let schedule t job ~scheduler =
    match
      Typing_scheduler.add ~monitor:t.monitor ~guard:t.guard scheduler job
    with
    | () -> Typing_guard.release t.guard
    | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Typing_guard.release t.guard;
      Printexc.raise_with_backtrace exn bt

  let schedule_run t ~value ~scheduler =
    schedule t (fun () -> t.run value) ~scheduler

  let schedule_cancel t ~scheduler = schedule t t.cancel ~scheduler

  let drop t = Typing_guard.release t.guard
end

module Cell = struct
  type 'a t =
    (* Newest first. Handlers run oldest first, so the first error reported
       is at the earliest use. *)
    | Empty of 'a Handler.t list
    | Full of 'a

  let empty = Empty []

  module After_merge = struct
    type 'a t =
      | Noop
      | Schedule of 'a * 'a Handler.t list

    let run t ~scheduler =
      match t with
      | Noop -> ()
      | Schedule (value, handlers) ->
        List.iter (Handler.schedule_run ~value ~scheduler) (List.rev handlers)
  end

  let merge (t1 : 'a t) (t2 : 'a t) ~(f : 'a -> 'a -> 'a) :
      'a t * 'a After_merge.t =
    match t1, t2 with
    | Empty handlers1, Empty handlers2 -> Empty (handlers1 @ handlers2), Noop
    | Empty handlers, (Full value as t) | (Full value as t), Empty handlers ->
      t, Schedule (value, handlers)
    | Full value1, Full value2 -> Full (f value1 value2), Noop
end

type 'a t = 'a Cell.t Union_find.t

module Change = Union_find.Change
module Log = With_backtracking.Make (Change)

let set_log log =
  Log.set_log log;
  Union_find.set_log log

let cell (t : 'a t) : 'a Cell.t = Union_find.get t

let set_cell (t : 'a t) (cell : 'a Cell.t) = Union_find.set t cell

let is_empty t = match cell t with Empty _ -> true | Full _ -> false

let has_handlers t =
  match cell t with Empty (_ :: _) -> true | Empty [] | Full _ -> false

let peek t = match cell t with Empty _ -> None | Full value -> Some value

let peek_exn t =
  match cell t with
  | Empty _ -> invalid_arg "Typing_ivar.peek_exn: empty ivar cell"
  | Full value -> value

module Fill_result = struct
  type 'a t =
    | Ok
    | Already_full of 'a
end

let fill t value ~scheduler : _ Fill_result.t =
  match cell t with
  | Full value' -> Already_full value'
  | Empty handlers ->
    set_cell t (Full value);
    List.iter (Handler.schedule_run ~value ~scheduler) (List.rev handlers);
    Ok

let upon t ~guard ~run ~cancel ~scheduler =
  match cell t with
  | Full value -> Typing_scheduler.add scheduler ~guard (fun () -> run value)
  | Empty handlers ->
    set_cell t (Empty (Handler.create ~guard ~run ~cancel :: handlers))

let take_handlers t =
  match cell t with
  | Full _ | Empty [] -> []
  | Empty handlers ->
    set_cell t Cell.empty;
    List.rev handlers

let cancel_all ts ~scheduler =
  (* Detach all handlers first: under the eager policy, [cancel] runs now. *)
  List.concat_map take_handlers ts
  |> List.iter (Handler.schedule_cancel ~scheduler)

let drop_all ts =
  List.iter (fun t -> List.iter Handler.drop (take_handlers t)) ts

let merge t1 t2 ~f ~scheduler =
  if not (Union_find.same t1 t2)
  then (
    let cell, after = Cell.merge (cell t1) (cell t2) ~f in
    Union_find.union t1 t2;
    set_cell t1 cell;
    Cell.After_merge.run after ~scheduler)

let create () = Union_find.create Cell.empty
