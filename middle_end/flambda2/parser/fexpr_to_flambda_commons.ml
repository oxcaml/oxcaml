let map_accum_left f env l =
  let next (acc, env) x =
    let y, env = f env x in
    y :: acc, env
  in
  let acc, env = List.fold_left next ([], env) l in
  List.rev acc, env

(* Continuation variables *)
module C = struct
  type t = string

  let compare = String.compare
end

module CM = Map.Make (C)

(* Variables *)
module V = struct
  type t = string

  let compare = String.compare
end

module VM = Map.Make (V)

(* Symbols (globally scoped, so updates are in-place) *)
module S = struct
  type t = string (* only storing local symbols so only need the name *)

  let equal = String.equal

  let hash = Hashtbl.hash
end

module SM = Hashtbl.Make (S)

(* Code ids (globally scoped, so updates are in-place) *)
module D = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module DM = Hashtbl.Make (D)

(* Function slots (globally scoped, so updates are in-place) *)
module U = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module UT = Hashtbl.Make (U)

(* Variables within closures (globally scoped, so updates are in-place) *)
module W = struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end

module WT = Hashtbl.Make (W)

type env =
  { done_continuation : Continuation.t;
    error_continuation : Exn_continuation.t;
    continuations : (Continuation.t * int) CM.t;
    exn_continuations : Continuation.t CM.t;
    toplevel_region : Variable.t;
    variables : Variable.t VM.t;
    symbols : Symbol.t SM.t;
    code_ids : Code_id.t DM.t;
    function_slots : Function_slot.t UT.t;
    vars_within_closures : Value_slot.t WT.t
  }

let init_env () =
  let done_continuation =
    Continuation.create ~sort:Toplevel_return ~name:"done" ()
  in
  let exn_handler = Continuation.create ~name:"error" () in
  let error_continuation =
    Exn_continuation.create ~exn_handler ~extra_args:[]
  in
  let toplevel_region = Variable.create "toplevel" Flambda_kind.region in
  { done_continuation;
    error_continuation;
    continuations = CM.empty;
    exn_continuations = CM.empty;
    toplevel_region;
    variables = VM.empty;
    symbols = SM.create 10;
    code_ids = DM.create 10;
    function_slots = UT.create 10;
    vars_within_closures = WT.create 10
  }

let enter_code env =
  { continuations = CM.empty;
    exn_continuations = CM.empty;
    toplevel_region = env.toplevel_region;
    variables = env.variables;
    done_continuation = env.done_continuation;
    error_continuation = env.error_continuation;
    symbols = env.symbols;
    code_ids = env.code_ids;
    function_slots = env.function_slots;
    vars_within_closures = env.vars_within_closures
  }

let fresh_cont env { Fexpr.txt = name; loc = _ } ~sort ~arity =
  let c = Continuation.create ~sort ~name () in
  c, { env with continuations = CM.add name (c, arity) env.continuations }

let fresh_exn_cont env { Fexpr.txt = name; loc = _ } ~arity =
  let c = Continuation.create ~name () in
  ( c,
    { env with
      continuations = CM.add name (c, arity) env.continuations;
      exn_continuations = CM.add name c env.exn_continuations
    } )

let fresh_var env { Fexpr.txt = name; loc = _ } k =
  let v = Variable.create name ~user_visible:() k in
  let v_duid = Flambda_debug_uid.none in
  (* CR sspies: In the future, try to improve the debug UID propagation here. *)
  v, v_duid, { env with variables = VM.add name v env.variables }

let fresh_or_existing_code_id env { Fexpr.txt = name; loc = _ } =
  match DM.find_opt env.code_ids name with
  | Some code_id -> code_id
  | None ->
    let c =
      Code_id.create ~name ~debug:Debuginfo.none
        (Compilation_unit.get_current_exn ())
    in
    DM.add env.code_ids name c;
    c

let fresh_function_slot env { Fexpr.txt = name; loc = _ } =
  let c =
    Function_slot.create
      (Compilation_unit.get_current_exn ())
      ~name ~is_always_immediate:false Flambda_kind.value
  in
  UT.add env.function_slots name c;
  c

let fresh_or_existing_function_slot env ({ Fexpr.txt = name; loc = _ } as id) =
  match UT.find_opt env.function_slots name with
  | None -> fresh_function_slot env id
  | Some function_slot -> function_slot

let fresh_value_slot env { Fexpr.txt = name; loc = _ } kind =
  let c =
    Value_slot.create
      (Compilation_unit.get_current_exn ())
      ~name ~is_always_immediate:false kind
  in
  WT.add env.vars_within_closures name c;
  c

let fresh_or_existing_value_slot env ({ Fexpr.txt = name; _ } as id) kind =
  match WT.find_opt env.vars_within_closures name with
  | None -> fresh_value_slot env id kind
  | Some value_slot -> value_slot

let print_scoped_location ppf loc =
  match (loc : Lambda.scoped_location) with
  | Loc_unknown -> Format.pp_print_string ppf "Unknown"
  | Loc_known { loc; _ } -> Location.print_loc ppf loc

let compilation_unit { Fexpr.ident; linkage_name } =
  (* CR lmaurer: This ignores the ident when the linkage name is given; is that
     what we want? Why did we have the ability to specify both? *)
  let linkage_name = linkage_name |> Option.value ~default:ident in
  Compilation_unit.of_string linkage_name

let declare_symbol (env : env) ({ Fexpr.txt = cu, name; loc } as symbol) =
  if Option.is_some cu
  then
    Misc.fatal_errorf "Cannot declare non-local symbol %a: %a"
      Print_fexpr.symbol symbol print_scoped_location loc
  else
    match SM.find_opt env.symbols name with
    | Some symbol -> symbol
    | None ->
      let cunit =
        match cu with
        | None -> Compilation_unit.get_current_exn ()
        | Some cu -> compilation_unit cu
      in
      let symbol = Symbol.unsafe_create cunit (Linkage_name.of_string name) in
      SM.replace env.symbols name symbol;
      symbol

let find_with ~descr ~find map { Fexpr.txt = name; loc } =
  match find name map with
  | None ->
    Misc.fatal_errorf "Unbound %s %s: %a" descr name print_scoped_location loc
  | Some a -> a

let get_symbol (env : env) sym =
  match sym with
  | { Fexpr.txt = Some cunit, name; loc = _ } ->
    let cunit = compilation_unit cunit in
    Symbol.unsafe_create cunit (name |> Linkage_name.of_string)
  | { Fexpr.txt = None, _; loc = _ } -> declare_symbol env sym

let find_cont_id env c =
  find_with ~descr:"continuation id" ~find:CM.find_opt env.continuations c

let find_cont env (c : Fexpr.continuation) =
  match c with
  | Special Done -> env.done_continuation, 1
  | Special Error -> Exn_continuation.exn_handler env.error_continuation, 1
  | Named cont_id -> find_cont_id env cont_id

let find_result_cont env (c : Fexpr.result_continuation) :
    Apply_expr.Result_continuation.t =
  match c with
  | Return c -> Return (fst (find_cont env c))
  | Never_returns -> Never_returns

let find_exn_cont_id env c =
  find_with ~descr:"exn_continuation" ~find:CM.find_opt env.exn_continuations c

let find_exn_cont env (c : Fexpr.continuation)
    (extra_args : (Int_ids.Simple.t * Flambda_kind.With_subkind.full_kind) list)
    =
  match c with
  | Special Done -> Misc.fatal_error "done is not an exception continuation"
  | Special Error -> env.error_continuation
  | Named cont_id ->
    let c = find_exn_cont_id env cont_id in
    Exn_continuation.create ~exn_handler:c ~extra_args

let find_var env v =
  find_with ~descr:"variable" ~find:VM.find_opt env.variables v

let find_region env (r : Fexpr.region) =
  match r with Toplevel -> env.toplevel_region | Named v -> find_var env v

let find_code_id env code_id = fresh_or_existing_code_id env code_id
