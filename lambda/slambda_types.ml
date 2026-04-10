open Lambda

module Or_missing = struct
  type 'a t =
    | Present of 'a
    | Missing

  let of_option = function Some a -> Present a | None -> Missing

  let[@inline] map t ~f =
    match t with
    | Present a -> Present ((f [@inlined hint]) a)
    | Missing -> Missing

  let[@inline] bind t ~f =
    match t with Present a -> (f [@inlined hint]) a | Missing -> Missing

  module Syntax = struct
    let[@inline] ( let* ) t f = bind t ~f

    let[@inline] ( |>> ) t f = map t ~f
  end
end

module rec Types : sig
  type closure =
    { clo_params : Slambdaident.t array;
      clo_body : slambda;
      clo_env : Env.t
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
end =
  Types

and Env : sig
  type t

  val empty : t

  val add : t -> Slambdaident.t -> Types.value Or_missing.t -> t

  val add_present : t -> Slambdaident.t -> Types.value -> t

  val find : t -> Slambdaident.t -> Types.value Or_missing.t
end = struct
  module Map = Slambdaident.Map

  type t = Types.value Map.t

  let empty = Map.empty

  let add t id v =
    match (v : Types.value Or_missing.t) with
    | Present v -> Map.add id v t
    | Missing -> (* Possibly unnecessary but be safe anyway *) Map.remove id t

  let add_present t id v = add t id (Present v)

  let find t id = Map.find_opt id t |> Or_missing.of_option
end

type env = Env.t

include Types
