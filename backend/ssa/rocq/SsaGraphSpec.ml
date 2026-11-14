(** val terminator_pure_control : Ssa.finished Ssa.Terminator.t -> bool **)

let terminator_pure_control t =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun continuation _ ->
      match continuation with Ssa.Goto _ -> true | _ -> false)
    (fun _ _ -> true)
    (fun _ -> false)
    (fun _ -> false)
    t

(** val terminator_args : Ssa.finished Ssa.Terminator.t -> Ssa.finished
    Ssa.Value.t list list **)

let terminator_args term =
  (fun f_continue
    f_switch
    f_call
    f_invalid
    (t : Ssa.finished Ssa.Terminator.t)
  ->
    match t with
    | Ssa.Terminator.Continue { continuation; args } ->
      f_continue continuation (Array.to_list args)
    | Ssa.Terminator.Switch { index; targets } ->
      f_switch index (Array.to_list targets)
    | Ssa.Terminator.Call { args; _ } -> f_call (Array.to_list args)
    | Ssa.Terminator.Invalid { args; _ } -> f_invalid (Array.to_list args))
    (fun _ args ->
      List.filter_map
        (fun arg ->
          match arg with
          | Ssa.Terminator.Arg v -> Some v
          | Ssa.Terminator.Omitted_since_unused -> None)
        args
      :: [])
    (fun index _ -> (index :: []) :: [])
    (fun args -> args :: [])
    (fun args -> args :: [])
    term
