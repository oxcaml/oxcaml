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
    | SLVclosure of Templates.id

  val print_value : Format.formatter -> value -> unit

  val print_halves : Format.formatter -> halves -> unit

  val print_or_missing : Format.formatter -> value Or_missing.t -> unit

  val print_closure : Format.formatter -> closure -> unit
end = struct
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
    | SLVclosure of Templates.id

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
    | SLVclosure c -> Templates.print_id ppf c

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
end

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

and Templates : sig
  type id

  type templates

  type t

  val empty : unit -> t

  val empty_templates : unit -> templates

  val add : t -> cu:Compilation_unit.t -> name:Slambdaident.t option -> Types.closure -> id

  val add_foreign_templates : t -> templates -> unit

  val instantiate :
    t ->
    id ->
    Types.value array ->
    (Types.closure -> Types.value array -> Types.value Or_missing.t) ->
    Types.value Or_missing.t

  val templates : t -> templates

  val instantiations : t -> (Ident.t * lambda) list

  val print_id : Format.formatter -> id -> unit

  val print_templates : Format.formatter -> templates -> unit
end = struct
  type id = string

  type templates = Types.closure Misc.Stdlib.String.Tbl.t

  type t =
    { templates : templates;
      foreign_templates : templates;
      instantiations : lambda Ident.Tbl.t
    }

  let stamp = ref 0

  let empty () = {
    templates = Misc.Stdlib.String.Tbl.create 10;
    foreign_templates = Misc.Stdlib.String.Tbl.create 10;
    instantiations = Ident.Tbl.create 10 }

  let empty_templates () = Misc.Stdlib.String.Tbl.create 0

  let add t ~(cu : Compilation_unit.t) ~name closure =
    let id =
      match name with
      | Some name ->
        Format_doc.asprintf "%a_%s_%i"
          Compilation_unit.print cu (Slambdaident.name name) !stamp
      | None -> Format_doc.asprintf "%a_%i" Compilation_unit.print cu !stamp
    in
    incr stamp;
    Misc.Stdlib.String.Tbl.add t.templates id closure;
    id

  let add_foreign_templates t templates =
    Misc.Stdlib.String.Tbl.add_seq
      t.foreign_templates
      (Misc.Stdlib.String.Tbl.to_seq templates)

  let instantiate t closure_id args f =
    let closure = Misc.Stdlib.String.Tbl.find t.templates closure_id in
    f closure args

  let templates t = t.templates

  let instantiations t = Ident.Tbl.to_list t.instantiations

  let print_id = Format.pp_print_string

  let print_templates ppf templates =
    Format.fprintf ppf "@[<hv>";
    Misc.Stdlib.String.Tbl.iter
      (fun id closure ->
        Format.fprintf ppf "@[<2>(%s@ %a)@]" id Types.print_closure closure)
      templates;
    Format.fprintf ppf "@]";

end

type template_id = Templates.id
type env = Env.t

include Types
