type use = Apply_function of [`Complex] Flambda_arity.t

type t =
  { source : Param_projection.t;
    use : use
  }

type uses = t list

type one_param_use =
  { projection : Param_projection.projection;
    use : use
  }

type params_uses = one_param_use list list

let empty = []

let empty_params_uses = []

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

let [@ocamlformat "disable"] print_one_param_uses ppf { projection; use } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(projection@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Param_projection.print_projection projection
    print_use use

let print_params_uses ppf uses =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (Format.pp_print_list ~pp_sep:Format.pp_print_space print_one_param_uses))
    uses

let approx_equal_use u1 u2 =
  match u1, u2 with
  | Apply_function a1, Apply_function a2 ->
    Flambda_arity.equal_ignoring_subkinds a1 a2

let approx_equal_one_param_uses { projection = projection1; use = use1 }
    { projection = projection2; use = use2 } =
  Param_projection.compare_projection projection1 projection2 = 0
  && approx_equal_use use1 use2

let approx_equal_params_uses uses1 uses2 =
  List.equal (List.equal approx_equal_one_param_uses) uses1 uses2

let convert arity (uses : uses) : params_uses =
  let params = Array.make (Flambda_arity.num_params arity) [] in
  List.iter
    (fun (t : t) ->
      match t.source.source with
      | My_closure -> ()
      | Param i ->
        params.(i)
          <- { projection = t.source.projection; use = t.use } :: params.(i))
    uses;
  Array.to_list params
