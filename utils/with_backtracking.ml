include With_backtracking_intf

module Make (C : Change.S) = struct
  let global_log : (C.t -> unit) ref = ref (fun _ -> ())

  let log change = !global_log change

  let set_log log = global_log := log
end
