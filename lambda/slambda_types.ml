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

open Format

let rec print_value ppf = function
  | SLVhalves h -> print_halves ppf h
  | SLVlayout l -> fprintf ppf "⟪%a⟫" Printlambda.layout l
  | SLVrecord fields ->
    let print_fields ppf =
      Array.iter
        (fun v -> fprintf ppf "%a;@ " print_or_missing v)
        fields
    in
    fprintf ppf "@[<hv 2>[@ %t]@]" print_fields
  | SLVclosure c -> print_closure ppf c

and print_halves ppf { slv_comptime; slv_runtime } =
  fprintf ppf "@[<hv>@[<2>{ c =@ %a@]@,@[<2>; r =@ ⟪%a⟫@] }@]"
    print_or_missing slv_comptime Printlambda.lambda slv_runtime

and print_or_missing ppf = function
  | Or_missing.Present v -> print_value ppf v
  | Or_missing.Missing -> fprintf ppf "(missing)"

and print_closure ppf { clo_params; clo_body; clo_env = _ } =
  let print_params ppf =
    Array.iter
      (fun id -> fprintf ppf "%a@ " Slambdaident.print id)
      clo_params
  in
  fprintf ppf "@[<2>(closure @[<2>%t->@]@ %a)@]" print_params
    Printlambda.slambda clo_body
