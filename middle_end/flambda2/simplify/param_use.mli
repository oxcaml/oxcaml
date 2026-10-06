
type use =
  | Apply_function of [`Complex] Flambda_arity.t

type t = {
  source : Param_projection.t;
  use : use;
}

type uses

val empty : uses

val add : source:Param_projection.t -> use:use -> uses -> uses

val print : Format.formatter -> t -> unit

val print_uses : Format.formatter -> uses -> unit
