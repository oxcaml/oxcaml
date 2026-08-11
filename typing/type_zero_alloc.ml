(* Interaction between [zero_alloc] annotations and the allocation axis. *)

type t =
  | Zero_alloc of { strict: bool; arity: int }
  | Default

let val_zero_alloc (v: Zero_alloc.t) =
  (* CR-soon shsong: Another option here is to use [zero_alloc] even when
    [opt = true]. *)
  match Zero_alloc.get v with
  | Check { strict; opt = false; arity; _ } | Assume { strict; arity; _ } ->
    Zero_alloc { strict; arity }
  | Check _ | Default_zero_alloc | Ignore_assert_all -> Default
