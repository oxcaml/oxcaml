open! Flambda.Import

(* CR-someday mshinwell: share with Fexpr_to_flambda / move to Stdlib *)
let map_accum_left f env l =
  let next (acc, env) x =
    let y, env = f env x in
    y :: acc, env
  in
  let acc, env = List.fold_left next ([], env) l in
  List.rev acc, env

module type Convertible_id = sig
  type t

  type fexpr_id

  include Container_types.S with type t := t

  val desc : string

  val name : t -> string

  val add_tag : string -> int -> string

  val mk_fexpr_id : string -> fexpr_id
end

let default_add_tag name tag = Printf.sprintf "%s_%d" name tag

module Name_map (I : Convertible_id) : sig
  type t

  val empty : t

  val bind : t -> I.t -> I.fexpr_id * t

  val bind_to : t -> I.t -> I.fexpr_id -> t

  val find_exn : t -> I.t -> I.fexpr_id
end = struct
  module String_map = Map.Make (String)

  type t =
    { id_map : I.fexpr_id I.Map.t;
      names : int String_map.t
    }

  let empty = { id_map = I.Map.empty; names = String_map.empty }

  let bind { id_map; names } id =
    let name = I.name id in
    let rec try_name name names =
      match String_map.find_opt name names with
      | None ->
        let fexpr_id = I.mk_fexpr_id name in
        let names = String_map.add name 1 names in
        fexpr_id, names
      | Some count ->
        let names = String_map.add name (count + 1) names in
        let name = I.add_tag name count in
        (* Unlikely but possible that, say, both x and x_1 are used; in this
         * case we'll end up with x_1_1 *)
        try_name name names
    in
    let fexpr_id, names = try_name name names in
    let id_map = I.Map.add id fexpr_id id_map in
    fexpr_id, { id_map; names }

  let bind_to { id_map; names } id fexpr_id =
    let id_map = I.Map.add id fexpr_id id_map in
    { id_map; names }

  let find t id = I.Map.find_opt id t.id_map

  let find_exn t id =
    match find t id with
    | Some fexpr_id -> fexpr_id
    | None ->
      Misc.fatal_errorf "missing %s %a (known names: %a)" I.desc I.print id
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        (String_map.bindings t.names |> List.map fst)
end

module Global_name_map (I : Convertible_id) : sig
  type t

  val create : unit -> t

  val translate : t -> I.t -> I.fexpr_id
end = struct
  module String_tbl = Hashtbl.Make (struct
    include String

    let hash = Hashtbl.hash
  end)

  type t =
    { mutable id_tbl : I.fexpr_id I.Map.t;
      names : int String_tbl.t
    }

  let create () = { id_tbl = I.Map.empty; names = String_tbl.create 10 }

  let translate t id =
    match I.Map.find_opt id t.id_tbl with
    | Some fexpr_id -> fexpr_id
    | None ->
      (* CR-soon lmaurer: Too much duplication with Name_map.bind *)
      let rec try_name name =
        match String_tbl.find_opt t.names name with
        | None ->
          let fexpr_id = I.mk_fexpr_id name in
          String_tbl.add t.names name 1;
          fexpr_id
        | Some count ->
          String_tbl.replace t.names name (count + 1);
          let name = Printf.sprintf "%s_%d" name count in
          (* Unlikely but possible that, say, both x and x_1 are used; in this
           * case we'll end up with x_1_1 *)
          try_name name
      in
      let fexpr_id = try_name (I.name id) in
      t.id_tbl <- I.Map.add id fexpr_id t.id_tbl;
      fexpr_id
end

let nowhere a = { Fexpr.txt = a; loc = Loc_unknown }

module Env : sig
  type t

  val create : unit -> t

  val bind_var : t -> Variable.t -> Fexpr.variable * t

  val bind_bound_var : t -> Bound_var.t -> Fexpr.variable * t

  val bind_symbol : t -> Symbol.t -> Fexpr.symbol * t

  val bind_code_id : t -> Code_id.t -> Fexpr.code_id * t

  val bind_named_continuation : t -> Continuation.t -> Fexpr.continuation_id * t

  val bind_special_continuation :
    t -> Continuation.t -> to_:Fexpr.special_continuation -> t

  val bind_toplevel_region : t -> Variable.t -> t

  val find_var_exn : t -> Variable.t -> Fexpr.variable

  val find_symbol_exn : t -> Symbol.t -> Fexpr.symbol

  val find_code_id_exn : t -> Code_id.t -> Fexpr.code_id

  val find_continuation_exn : t -> Continuation.t -> Fexpr.continuation

  val find_region_exn : t -> Variable.t -> Fexpr.region

  val translate_function_slot : t -> Function_slot.t -> Fexpr.function_slot

  val translate_value_slot : t -> Value_slot.t -> Fexpr.value_slot
end = struct
  module Variable_name_map = Name_map (struct
    include Variable

    type fexpr_id = Fexpr.variable

    let desc = "variable"

    let name v = raw_name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Symbol_name_map = Global_name_map (struct
    include Symbol

    (* We don't need the name map for non-local symbols, so only bother with
     * the ident part of the symbol here *)
    type fexpr_id = string

    let desc = "symbol"

    let name v = linkage_name v |> Linkage_name.to_string

    let add_tag = default_add_tag

    let mk_fexpr_id name = name
  end)

  module Code_id_name_map = Global_name_map (struct
    include Code_id

    type fexpr_id = Fexpr.code_id

    let desc = "code id"

    let name v = Code_id.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Function_slot_name_map = Global_name_map (struct
    include Function_slot

    type fexpr_id = Fexpr.function_slot

    let desc = "function slot"

    let name v = Function_slot.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Value_slot_name_map = Global_name_map (struct
    include Value_slot

    type fexpr_id = Fexpr.value_slot

    let desc = "var within closure"

    let name v = Value_slot.name v

    let add_tag = default_add_tag

    let mk_fexpr_id name = name |> nowhere
  end)

  module Continuation_name_map = Name_map (struct
    include Continuation

    type fexpr_id = Fexpr.continuation

    let desc = "continuation"

    let name c = Continuation.name c

    let add_tag name tag =
      match name with
      | "k" -> Printf.sprintf "k%d" tag
      | _ -> default_add_tag name tag

    let mk_fexpr_id name : Fexpr.continuation = Named (name |> nowhere)
  end)

  type t =
    { variables : Variable_name_map.t;
      symbols : Symbol_name_map.t;
      code_ids : Code_id_name_map.t;
      function_slots : Function_slot_name_map.t;
      vars_within_closures : Value_slot_name_map.t;
      continuations : Continuation_name_map.t;
      toplevel_region : Variable.t option
    }

  let create () =
    { variables = Variable_name_map.empty;
      symbols = Symbol_name_map.create ();
      code_ids = Code_id_name_map.create ();
      function_slots = Function_slot_name_map.create ();
      vars_within_closures = Value_slot_name_map.create ();
      continuations = Continuation_name_map.empty;
      toplevel_region = None
    }

  let bind_var t v =
    let v, variables = Variable_name_map.bind t.variables v in
    v, { t with variables }

  let bind_bound_var t v = bind_var t (v |> Bound_var.var)

  let bind_symbol t s =
    let is_local =
      Compilation_unit.equal
        (Symbol.compilation_unit s)
        (Compilation_unit.get_current_exn ())
    in
    if not is_local
    then
      Misc.fatal_errorf "Cannot bind non-local symbol %a@ Current unit is %a"
        Symbol.print s Compilation_unit.print
        (Compilation_unit.get_current_exn ());
    let s = Symbol_name_map.translate t.symbols s in
    (None, s) |> nowhere, t

  let bind_code_id t c =
    let c = Code_id_name_map.translate t.code_ids c in
    c, t

  let bind_named_continuation t c =
    let c, continuations = Continuation_name_map.bind t.continuations c in
    let c_id = match c with Named c_id -> c_id | Special _ -> assert false in
    c_id, { t with continuations }

  let bind_special_continuation t c ~to_:s =
    let continuations =
      Continuation_name_map.bind_to t.continuations c (Special s)
    in
    { t with continuations }

  let bind_toplevel_region t v = { t with toplevel_region = Some v }

  let find_var_exn t v = Variable_name_map.find_exn t.variables v

  let find_symbol_exn t s =
    let cunit = Symbol.compilation_unit s in
    let is_local =
      Compilation_unit.equal cunit (Compilation_unit.get_current_exn ())
    in
    if is_local
    then (None, Symbol_name_map.translate t.symbols s) |> nowhere
    else
      let cunit =
        let ident =
          Compilation_unit.name cunit |> Compilation_unit.Name.to_string
        in
        let linkage_name = Compilation_unit.full_path_as_string cunit in
        let linkage_name =
          if String.equal ident linkage_name then None else Some linkage_name
        in
        { Fexpr.ident; linkage_name }
      in
      let linkage_name = Symbol.linkage_name s |> Linkage_name.to_string in
      (Some cunit, linkage_name) |> nowhere

  let find_code_id_exn t c = Code_id_name_map.translate t.code_ids c

  let find_continuation_exn t c =
    Continuation_name_map.find_exn t.continuations c

  let find_region_exn t r : Fexpr.region =
    match t.toplevel_region with
    | Some toplevel_region when Variable.equal toplevel_region r -> Toplevel
    | _ -> Named (find_var_exn t r)

  let translate_function_slot t c =
    Function_slot_name_map.translate t.function_slots c

  let translate_value_slot t v =
    Value_slot_name_map.translate t.vars_within_closures v
end
