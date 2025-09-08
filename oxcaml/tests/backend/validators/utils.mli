open Cfg_intf.S
module DLL = Oxcaml_utils.Doubly_linked_list

module Instruction : sig
  type 'a t =
    { mutable desc : 'a;
      mutable arg : Reg.t array;
      mutable res : Reg.t array;
      mutable id : InstructionId.t
    }

  val make : remove_locs:bool -> 'a t -> 'a instruction
end

module Basic : sig
  type t = basic Instruction.t

  val make : remove_locs:bool -> t -> basic instruction
end

module Terminator : sig
  type t = terminator Instruction.t

  val make : remove_locs:bool -> t -> terminator instruction
end

module Block : sig
  type t =
    { start : Label.t;
      mutable body : Basic.t list;
      terminator : Terminator.t;
      exn : Label.t option
    }

  val make : remove_regalloc:bool -> remove_locs:bool -> t -> Cfg.basic_block
end

module Cfg_desc : sig
  type t =
    { mutable fun_args : Reg.t array;
      blocks : Block.t list;
      fun_contains_calls : bool;
      fun_ret_type : Cmm.machtype
    }

  val make : remove_regalloc:bool -> remove_locs:bool -> t -> Cfg_with_infos.t

  val make_pre_regalloc : t -> Cfg_with_infos.t

  val make_post_regalloc : t -> Cfg_with_infos.t
end

val entry_label : Label.t

val move_param_label : Label.t

val call_label : Label.t

val move_tmp_res_label : Label.t

val add_label : Label.t

val return_label : Label.t

val int : Reg.t array

val check :
  string ->
  (unit -> 'a) ->
  validate:('a -> unit) ->
  save:('a -> unit) ->
  exp_std:string ->
  exp_err:string ->
  unit

val new_label : int -> Label.t
