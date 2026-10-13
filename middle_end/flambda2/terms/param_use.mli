
type use =
  | Apply_function of [`Complex] Flambda_arity.t
  (** Used as a function in an indirect function call with the given arity *)
  | Switch_scrutinee

type t = {
  source : Param_projection.t;
  use : use;
}

type uses

type one_param_use = {
  projection : Param_projection.projection;
  use : use;
}

type params_uses = one_param_use list list
(* Collection of uses of each parameters in arity order *)
(* TODO accessor functions using arity *)

val empty : uses

val empty_params_uses : params_uses

val add : source:Param_projection.t -> use:use -> uses -> uses

val approx_equal_params_uses : params_uses -> params_uses -> bool

val convert : [`Complex] Flambda_arity.t -> uses -> params_uses

val print : Format.formatter -> t -> unit

val print_uses : Format.formatter -> uses -> unit

val print_params_uses : Format.formatter -> params_uses -> unit
