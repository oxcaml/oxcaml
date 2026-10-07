
type use =
  | Apply_function of [`Complex] Flambda_arity.t

type t = {
  source : Param_projection.t;
  use : use;
}

type uses = t list

let empty = []

let add ~source ~use uses = { source; use } :: uses

let print_use ppf = function
  | Apply_function arity ->
      Format.fprintf ppf "Apply_function(%a)" Flambda_arity.print arity

let [@ocamlformat "disable"] print ppf { source; use } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(source@ %a)@]@ \
      @[<hov 1>(use@ %a)@]\
      )@]"
    Param_projection.print source
    print_use use

let print_uses ppf uses =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
    uses
