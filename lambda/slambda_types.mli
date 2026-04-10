open Lambda

module Or_missing : sig
  type 'a t =
    | Present of 'a
    | Missing

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( |>> ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

type env

type closure =
  { clo_params : Slambdaident.t array;
    clo_body : slambda;
    clo_env : env
  }

type halves =
  { slv_comptime : value Or_missing.t;
    slv_runtime : lambda
  }

and value =
  | SLVhalves of halves
  | SLVlayout of layout
  | SLVrecord of value Or_missing.t array
  | SLVclosure of closure


module Env : sig
  type t = env

  val empty : t

  val add : t -> Slambdaident.t -> value Or_missing.t -> t

  val add_present : t -> Slambdaident.t -> value -> t

  val find : t -> Slambdaident.t -> value Or_missing.t
end
