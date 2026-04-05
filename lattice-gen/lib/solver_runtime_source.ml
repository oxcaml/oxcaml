let contents =
  {|
module Make (C : Solver_intf.Lattices_mono) = struct
  open Allowance

  module Fmt = Format_doc

  type nonrec 'a obj = 'a C.obj

  type nonrec ('a, 'b, 'd) morph = ('a, 'b, 'd) C.morph
    constraint 'd = 'l * 'r

  module Hint = struct
    module Pinpoint = struct
      type t = unit

      let unknown = ()
    end

    module Morph = struct
      type 'd t = unit constraint 'd = 'l * 'r

      let unknown = ()
      let id = ()
      let left_adjoint pp _ = pp, ()
      let right_adjoint pp _ = pp, ()

      include Magic_allow_disallow (struct
        type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

        let disallow_right : type l r.
            (l * r) t -> (l * disallowed) t = Obj.magic

        let disallow_left : type l r.
            (l * r) t -> (disallowed * r) t = Obj.magic

        let allow_right : type l r.
            (l * allowed) t -> (l * r) t = Obj.magic

        let allow_left : type l r.
            (allowed * r) t -> (l * r) t = Obj.magic
      end)
    end

    module Const = struct
      type 'd t = unit constraint 'd = 'l * 'r

      let unknown = ()
      let max = ()
      let min = ()

      include Magic_allow_disallow (struct
        type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

        let disallow_right : type l r.
            (l * r) t -> (l * disallowed) t = Obj.magic

        let disallow_left : type l r.
            (l * r) t -> (disallowed * r) t = Obj.magic

        let allow_right : type l r.
            (l * allowed) t -> (l * r) t = Obj.magic

        let allow_left : type l r.
            (allowed * r) t -> (l * r) t = Obj.magic
      end)
    end
  end

  module Raw = Solver.Solver_mono (Hint) (C)

  let try_with_log op =
    let log' = ref Raw.empty_changes in
    let log = Some log' in
    match op ~log with
    | Ok _ as ok -> ok
    | Error _ as error ->
      Raw.undo_changes !log';
      error

  let with_log op =
    let log' = ref Raw.empty_changes in
    let log = Some log' in
    op ~log

  let equate_from_submode submode_log m1 m2 ~log =
    match submode_log m1 m2 ~log with
    | Error e -> Error (true, e)
    | Ok () -> (
      match submode_log m2 m1 ~log with
      | Error e -> Error (false, e)
      | Ok () -> Ok ())

  module type OBJ = sig
    val obj : int obj
  end

  module Positive_gen (Obj_desc : OBJ) = struct
    type 'd t = (int, 'l * 'r) Raw.mode constraint 'd = 'l * 'r
    type l = (allowed * disallowed) t
    type r = (disallowed * allowed) t
    type lr = (allowed * allowed) t

    let min : lr = Obj.magic (Raw.min Obj_desc.obj)
    let max : lr = Obj.magic (Raw.max Obj_desc.obj)
    let of_const : type l r. int -> (l * r) t = fun a -> Raw.of_const Obj_desc.obj a
    let to_const_exn m = Raw.to_const_exn Obj_desc.obj m
    let newvar () = Raw.newvar Obj_desc.obj
    let newvar_above m = Raw.newvar_above Obj_desc.obj m
    let newvar_below m = Raw.newvar_below Obj_desc.obj m
    let submode_log a b ~log = Raw.submode () Obj_desc.obj a b ~log

    let submode_raw a b = try_with_log (submode_log a b)

    let equate_raw a b =
      let submode_log a b ~log = Raw.submode () Obj_desc.obj a b ~log in
      match try_with_log (equate_from_submode submode_log a b) with
      | Ok () -> Ok ()
      | Error (forward, error) -> Error (forward, error)

    let join modes = Raw.join Obj_desc.obj modes
    let meet modes = Raw.meet Obj_desc.obj modes
    let print ?verbose ppf m = Raw.print ?verbose Obj_desc.obj ppf m
    let zap_to_floor m = with_log (Raw.zap_to_floor Obj_desc.obj m)
    let zap_to_ceil m = with_log (Raw.zap_to_ceil Obj_desc.obj m)
  end
end
|}
