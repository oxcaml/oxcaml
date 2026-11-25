type t =
  { mutable holds : int;
        (** The number of holds, or [released] once the last is released. *)
    stamp : int  (** See [With_backtracking.Stamp]. *)
  }

let released = -1

module Change = struct
  (** The previous [holds] of a guard. *)
  type nonrec t = Holds of t * int

  let undo (Holds (t, holds)) = t.holds <- holds
end

include With_backtracking.Make (Change)

(* Never mutated, so never logged. *)
let none = { holds = released; stamp = -1 }

let create () = { holds = 0; stamp = With_backtracking.Stamp.fresh () }

let[@inline] set_holds t holds =
  if With_backtracking.Stamp.needs_log t.stamp then log (Holds (t, t.holds));
  t.holds <- holds

let is_live t = t.holds <> released

let acquire t =
  if not (t == none)
  then (
    if t.holds = released
    then Misc.fatal_error "Typing_guard.acquire: released guard";
    set_holds t (t.holds + 1))

let release t =
  if not (t == none)
  then (
    if t.holds <= 0 then Misc.fatal_error "Typing_guard.release: no holds";
    set_holds t (if t.holds = 1 then released else t.holds - 1))

module Set = struct
  type guard = t

  (* Without duplicates. Small, so a list. *)
  type nonrec t = guard list

  let empty = []

  let is_empty t = List.is_empty t

  let mem guard t = List.memq guard t

  let add guard t = if mem guard t then t else guard :: List.filter is_live t

  let iter_live f t = List.iter (fun guard -> if is_live guard then f guard) t

  let is_live t = List.exists is_live t
end
