include With_backtracking_intf

module Stamp = struct
  let last = Local_store.s_ref (-1)

  let latest_snapshot = Local_store.s_ref 0

  let fresh () =
    incr last;
    !last

  let needs_log t = t <= !latest_snapshot

  module Snapshot = struct
    type t = int

    let take () =
      let old = !latest_snapshot in
      latest_snapshot := !last;
      old

    let restore old = latest_snapshot := old
  end
end

module Make (C : Change.S) = struct
  let global_log : (C.t -> unit) ref = ref (fun _ -> ())

  let log change = !global_log change

  let set_log log = global_log := log
end
