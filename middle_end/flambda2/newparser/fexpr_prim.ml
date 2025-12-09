type param = Fexpr.prim_param =
  | Labeled of { label : string; value : string Fexpr.located }
  | Positional of string Fexpr.located
  | Flag of string

type t = Fexpr.prim_op = { prim : string; params: param list }

type to_fl_env = Fexpr_to_flambda_commons.env
type of_fl_env = Flambda_to_fexpr_commons.Env.t

type 'p cons0 = to_fl_env -> 'p -> Flambda_primitive.nullary_primitive
type 'p cons1 = to_fl_env -> 'p -> Flambda_primitive.unary_primitive
type 'p cons2 = to_fl_env -> 'p -> Flambda_primitive.binary_primitive
type 'p cons3 = to_fl_env -> 'p -> Flambda_primitive.ternary_primitive
type 'p cons4 = to_fl_env -> 'p -> Flambda_primitive.quaternary_primitive
type 'p consN = to_fl_env -> 'p -> int -> Flambda_primitive.variadic_primitive

type ('p, 't, 'r) lens = {
  of_fl : of_fl_env -> 'p -> 't;
  to_fl : to_fl_env -> 't -> 'r;
}

type ('a, 'b) map_lens = ('a, 'b, 'a) lens
type 'p value_lens = ('p, string Fexpr.located) map_lens
type 'p params_lens = ('p, param list) map_lens
type 'p prim_lens = ('p, t, Simple.t list -> Flambda_primitive.t) lens
type 'p conv = of_fl_env -> 'p -> t

type _ param_cons =
  | Pos : 'p value_lens -> 'p param_cons
  | Lbl : string * 'p value_lens -> 'p param_cons
  | Flg : string -> unit param_cons
  | Opt : 'p param_cons -> 'p option param_cons
  | Def : 'p param_cons * 'p * ('p -> 'p -> bool) -> 'p param_cons
  | Lst : 'p param_cons list -> 'p list param_cons
  | Etr : 'p case_cons list * ('p -> param list) -> 'p param_cons
  | Map : 'a param_cons * ('b, 'a) map_lens -> 'b param_cons
  | Pa0 : unit param_cons
  | Pa2 : 'a param_cons * 'b param_cons -> ('a*'b) param_cons
  | Pa3 : 'a param_cons * 'b param_cons * 'c param_cons -> ('a*'b*'c) param_cons

and 'p case_cons =
  Case : ('p, 'c option, 'p) lens * 'c param_cons -> 'p case_cons

let wrap_loc txt = Fexpr.{ txt; loc = Debuginfo.Scoped_location.Loc_unknown }
let unwrap_loc located = located.Fexpr.txt

let extract_param (env : to_fl_env) (params : param list) (cons : 'p param_cons) : 'p * param list =
  let rec pos conv lp params =
    match params with
    | [] -> None, lp
    | (Positional p) :: rp -> Some (conv env p), lp@rp
    | (Labeled _ | Flag _ as p)::rp -> pos conv (lp@[p]) rp
  in
  let rec lbl l conv lp params =
    match params with
    | [] -> None, lp
    | (Labeled { label; value } as p):: rp ->
        if String.equal l label then
          Some (conv env value), lp@rp
        else lbl l conv (lp@[p]) rp
    | (Positional _ | Flag _ as p)::rp -> lbl l conv (lp@[p]) rp
  in
  let rec flg f lp params =
    match params with
    | [] -> None, lp
    | (Flag flag as p):: rp ->
        if String.equal f flag then
          Some (), lp@rp
        else flg f (lp@[p]) rp
    | (Positional _ | Labeled _ as p)::rp -> flg f (lp@[p]) rp
  in
  let no_param params = Some (), params in
  let rec def : type p. p -> p param_cons -> param list -> p option * param list =
    fun d cons params ->
      match aux cons params with
      | None, params -> Some d, params
      | found, params -> found, params
  and opt : type p. p param_cons -> param list -> p option option * param list =
    fun cons params ->
      let (found : p option), params = aux cons params in
      Some found, params
  and lst : type p. p param_cons list -> param list -> p list option * param list =
    fun consl params ->
      List.fold_left (fun (pl, ps) cons ->
          match pl with
          | None -> None, params
          | Some pl ->
              match aux cons ps with
              | Some p, ps -> Some (p::pl), ps
              | None, _ -> None, params
        )
        (Some [], params) consl
  and etr : type p. p case_cons list -> param list -> p option * param list =
    fun cases params ->
      match cases with
      | [] -> None, params
      | Case (conv, param_cons) :: cases ->
          match aux param_cons params with
          | Some p, params -> Some (conv.to_fl env (Some p)), params
          | None, _ -> etr cases params
  and map : type a b. a param_cons -> (to_fl_env -> a -> b) -> param list -> b option * param list =
    fun cons f params ->
    let p, params = aux cons params in
    Option.map (f env) p, params
  and param2 : type p q. p param_cons -> q param_cons -> param list -> (p*q) option * param list =
    fun pc1 pc2 params ->
    let p1, params = aux pc1 params in
    let p2, params = aux pc2 params in
    Option.bind p1 (fun p1 ->
        Option.bind p2 (fun p2 -> Some (p1,p2))),
    params
  and param3 : type p q r. p param_cons -> q param_cons -> r param_cons -> param list -> (p*q*r) option * param list =
    fun pc1 pc2 pc3 params ->
    let p1, params = aux pc1 params in
    let p2, params = aux pc2 params in
    let p3, params = aux pc3 params in
    Option.bind p1 (fun p1 ->
        Option.bind p2 (fun p2 ->
            Option.bind p3 (fun p3 -> Some (p1,p2,p3)))),
    params
  and aux : type p. p param_cons -> param list -> p option * param list =
    function
    | Pos plens -> pos plens.to_fl []
    | Lbl (l, plens) -> lbl l plens.to_fl []
    | Flg f -> flg f []
    | Def (pcons, default, _) -> def default pcons
    | Opt pcons -> opt pcons
    | Lst pconsl -> lst pconsl
    | Etr (cases, _) -> etr cases
    | Map (pcons, l) -> map pcons l.to_fl
    | Pa0 -> no_param
    | Pa2 (pc1, pc2) -> param2 pc1 pc2
    | Pa3 (pc1, pc2, pc3) -> param3 pc1 pc2 pc3
  in
  match aux cons params with
  | None, _ -> Misc.fatal_error "Missing parameter"
  | Some p, params -> p, params

let rec build_param : type p. of_fl_env -> p -> p param_cons -> param list =
  fun env p cons ->
  match cons with
  | Pos vl -> [Positional (vl.of_fl env p)]
  | Lbl (label, vl) -> [Labeled { label; value = vl.of_fl env p}]
  | Flg flag -> [Flag flag]
  | Opt pcons ->
      (match p with None -> [] | Some p -> build_param env p pcons)
  | Def (pcons, default, eq) ->
      if eq p default then [] else build_param env p pcons
  | Lst pconsl ->
      List.flatten @@
      List.map2 (build_param env) p pconsl
  | Etr ([], failure) -> failure p
  | Etr (Case (conv, param_cons)::cases, f) ->
      (match conv.of_fl env p with
      | None -> build_param env p (Etr (cases, f))
      | Some p -> build_param env p param_cons)
  | Map (pcons, f) -> build_param env (f.of_fl env p) pcons
  | Pa0 -> []
  | Pa2 (pc1, pc2) ->
      let p1, p2 = p in
      build_param env p1 pc1 @ build_param env p2 pc2
  | Pa3 (pc1, pc2, pc3) ->
      let p1, p2, p3 = p in
      build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3

let lens_of_cons (cons : 'p param_cons) : 'p params_lens =
  { of_fl = (fun env p -> build_param env p cons);
    to_fl = (fun env params ->
        let ps, rem = extract_param env params cons in
        match rem with
        | [] -> ps
        | _ ->
            Format.eprintf "Unexpected parameter %a\n"
              Print_fexpr.prim_params rem;
            ps)
  }

let prim_table = Hashtbl.create 32
let register_lens id l =
  Hashtbl.add prim_table id l.to_fl;
  l.of_fl

module L = struct

  let todo0 s =
    let f _ _ = Misc.fatal_errorf "TODO: %s" s in
    { of_fl = f; to_fl = f }

  let todops s =
    todo0 ("parameter "^s)

  let todop s = Pos (todops s)

  let todo s = register_lens s @@ todo0 s

  let target_ocaml_int : Target_ocaml_int.t value_lens =
    { of_fl = (fun _ t -> Target_ocaml_int.to_int t |> string_of_int);
      to_fl = (fun _ s ->
          (* CR mshinwell: Should get machine_width from fexpr context when
             available *)
          let mw = Target_system.Machine_width.Sixty_four in
          Target_ocaml_int.of_int mw @@ int_of_string s)
    }

  let int : int value_lens =
    { of_fl = (fun _ i -> wrap_loc @@ string_of_int i);
      to_fl = (fun _ s -> int_of_string (unwrap_loc s))
    }

  let string : string Fexpr.located value_lens =
    { of_fl = (fun _ s -> s);
      to_fl = (fun _ s -> s)
    }

  let present : (bool, unit option) map_lens =
    { of_fl = (fun _ b -> if b then Some () else None);
      to_fl = (fun _ -> Option.is_some)
    }

  let diy = string

  let constructor_value (constrs : (string * 'a) list): 'a value_lens =
    let rec findc c = function
      | [] -> Misc.fatal_error "Undefined constructor"
      | (s, c')::l ->
          if Stdlib.(=) c c' then s else findc c l
    in
    let rec finds s = function
      | [] -> Misc.fatal_error "Undefined constructor"
      | (s', c)::l ->
          if String.equal s s' then c else finds s l
    in
    { of_fl = (fun _ c -> wrap_loc @@ findc c constrs);
      to_fl = (fun _ s -> finds (unwrap_loc s) constrs)}

  let positional (plens : 'a value_lens) : 'a param_cons =
    Pos plens

 let labeled label (plens : 'a value_lens) : 'a param_cons =
    Lbl (label, plens)

 let flag flag : unit param_cons = Flg flag

 let default ~(def : 'p) ?(eq : 'p -> 'p -> bool = Stdlib.(=)) (pcons : 'p param_cons) : 'p param_cons =
   Def (pcons, def, eq)

 let option (pcons : 'p param_cons) : 'p option param_cons =
   Opt pcons

 let list (pconsl : 'p param_cons list) : 'p list param_cons =
   Lst pconsl

 let either ?(no_match_handler = (fun _ -> Misc.fatal_error "Missing parameter case"))
     (cases : 'p case_cons list) : 'p param_cons =
   Etr (cases, no_match_handler)

 let case ~(box : _ -> 'c -> 'p) ~(unbox : _ -> 'p -> 'c option)
     (param_cons : 'c param_cons) : 'p case_cons =
   Case (
     { of_fl = unbox;
       to_fl = (fun e -> function
           | Some p -> box e p
           | None -> assert false) },
     param_cons)

 let id_case (param_cons : 'p param_cons) : 'p case_cons =
   Case (
     { of_fl = (fun _ p -> Some p);
       to_fl = (fun _ -> function
           | Some p -> p
           | None -> assert false) },
     param_cons)

 let map (f : ('b, 'a) map_lens) (pcons : 'a param_cons) : 'b param_cons =
   Map (pcons, f)

 let opt_presence (pcons : unit option param_cons) : bool param_cons =
   map { of_fl = (fun _ b -> if b then Some () else None);
         to_fl = (fun _ -> Option.is_some)
       }
     pcons

 let bool_flag f : bool param_cons =
   opt_presence @@ option @@ flag f

 let constructor_flag ?no_match_handler (flags : (string * 'p) list) : 'p param_cons =
   either ?no_match_handler
     (List.map (fun (f, constr) ->
          case (flag f)
            ~box:(fun _ () -> constr)
            ~unbox:(fun _ p -> if Stdlib.(=) p constr then Some () else None))
         flags)

 let no_param = Pa0
 let param2 pc1 pc2 = Pa2 (pc1, pc2)
 let param3 pc1 pc2 pc3 = Pa3 (pc1, pc2, pc3)

 let nullary :
   type p. string -> params:p param_cons -> p cons0 -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     match args with
     | [] -> Misc.fatal_errorf "Primitive %s takes no arguments" id
     | _ ->
         let params = params.to_fl env t.params in
         Flambda_primitive.Nullary (cons env params)
   in
   register_lens id { of_fl; to_fl }

 let unary :
   type p. string -> params:p param_cons -> p cons1 -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     match args with
     | [ arg ] ->
         let params = params.to_fl env t.params in
         Flambda_primitive.Unary (cons env params, arg)
     | _ -> Misc.fatal_errorf "Primitive %s takes one argument" id
   in
   register_lens id { of_fl; to_fl }

 let binary :
   type p. string -> params:p param_cons -> p cons2 -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     match args with
     | [ arg1; arg2 ] ->
         let params = params.to_fl env t.params in
         Flambda_primitive.Binary (cons env params, arg1, arg2)
     | _ -> Misc.fatal_errorf "Primitive %s takes two argument" id
   in
   register_lens id { of_fl; to_fl }

 let ternary :
   type p. string -> params:p param_cons -> p cons3 -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     match args with
     | [ arg1; arg2; arg3 ] ->
         let params = params.to_fl env t.params in
         Flambda_primitive.Ternary (cons env params, arg1, arg2, arg3)
     | _ -> Misc.fatal_errorf "Primitive %s takes three argument" id
   in
   register_lens id { of_fl; to_fl }

 let quaternary :
   type p. string -> params:p param_cons -> p cons4 -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     match args with
     | [ arg1; arg2; arg3; arg4 ] ->
         let params = params.to_fl env t.params in
         Flambda_primitive.Quaternary (cons env params, arg1, arg2, arg3, arg4)
     | _ -> Misc.fatal_errorf "Primitive %s takes four argument" id
   in
   register_lens id { of_fl; to_fl }

 let variadic :
   type p. string -> params:p param_cons -> p consN -> p conv =
   fun id ~params cons ->
   let params = lens_of_cons params in
   let of_fl env p =
     let params = params.of_fl env p in
     { prim = id; params; }
   in
   let to_fl env t args =
     let params = params.to_fl env t.params in
     Flambda_primitive.Variadic (cons env params (List.length args), args)
   in
   register_lens id { of_fl; to_fl }

end

let todo = L.todo

(* Nullaries *)
let invalid = L.(nullary "%invalid" ~params:(todop "Flambda_kind")
                   (fun _env kind -> Flambda_primitive.Invalid kind))
let optimised_out = L.(nullary "%optimised_out" ~params:(todop "Flambda_kind")
                         (fun _env kind -> Flambda_primitive.Optimised_out kind))
let probe_is_enabled = L.(nullary "%probe_is_enable"
                            ~params:(param2
                                       (todop "probe_name")
                                       (todop "probe_init"))
                            (fun _env (name, enabled_at_init) ->
                                Flambda_primitive.Probe_is_enabled { name; enabled_at_init }))
let enter_inlined_apply = L.(nullary  "%inlined_apply" ~params:(todop "dbginfo")
                               (fun _env dbg -> Flambda_primitive.Enter_inlined_apply {dbg}))
let dls_get = L.(nullary "%dls_get" ~params:no_param
                   (fun _env () -> Flambda_primitive.Dls_get))
let tls_get = L.(nullary "%tls_get" ~params:no_param
                   (fun _env () -> Flambda_primitive.Tls_get))
let poll = L.(nullary "%poll" ~params:no_param
                   (fun _env () -> Flambda_primitive.Poll))
let cpu_relax = L.(nullary "%cpu_relax" ~params:no_param
                   (fun _env () -> Flambda_primitive.Cpu_relax))

(* Unaries *)
let block_load = L.(unary "%block_load"
                      ~params:(param3
                                (todop "Block_access_kind")
                                (default ~def:Mutability.Immutable @@
                                 either [
                                   val_flag Mutability.Immutable "immutable";
                                   val_flag Mutability.Immutable_unique "immutable_unique";
                                   val_flag Mutability.Mutable "mutable" ]
                                   (function
                                     | Mutability.Immutable -> 0
                                     | Mutability.Immutable_unique -> 1
                                     | Mutability.Mutable -> 2)
                                )
                                (positional target_ocaml_int))
                      (fun _env (kind, mut, field) ->
                         Flambda_primitive.Block_load { kind; mut; field }))
let array_length = todo "%array_length"
let box_number = todo "%box_number"
let tag_immediate = L.(unary "%tag_immediate" ~params:no_param
                         (fun _ () -> Flambda_primitive.Tag_immediate))
let get_tag = L.(unary "%get_tag" ~params:no_param
                         (fun _ () -> Flambda_primitive.Get_tag))
let end_region = todo "%end_region"
let end_try_region = todo "%end_try_region"
let int_arith_u = todo "%int_arith"
let is_flat_float_array = L.(unary "%is_flat_float_array" ~params:no_param
                               (fun _ () -> Flambda_primitive.Is_flat_float_array))
let is_int = todo "%is_int"
let is_null = todo "%is_null"
let num_conv = todo "%num_conv"
let opaque_identity = todo "%opaque_identity"
let unbox_number = todo "%unbox_number"
let untag_immediate = L.(unary "%untag_immediate" ~params:no_param
                               (fun _ () -> Flambda_primitive.Untag_immediate))
let project_value_slot = todo "%project_value_slot"
let project_function_slot = todo "%project_function_slot"
let string_length = todo "%string_length"
let boolean_not = L.(unary "%boolean_not" ~params:no_param
                       (fun _ () -> Flambda_primitive.Boolean_not))

(* Binaries *)
let block_set = todo "%block_set"
let array_load = todo "%array_load"
let phys_equal = todo "%phys_equal"
let int_arith_b = todo "%int_arith"
let int_comp = todo "%int_comp"
let int_shift = todo "%int_shift"
let float_arith = todo "%float_arith"
let float_comp = todo "%float_comp"
let string_or_bigstring_load = todo "%string_or_bigstring_load"
let bigarray_get_alignment = todo "%bigarray_get_alignment"

(* Ternaries *)
let array_set = todo "%array_set"
let bytes_or_bigstring_set = todo "%bytes_or_bigstring_set"

(* Quaternaries *)

(* Variadics *)
let begin_region = todo "%begin_region"
let begin_try_region = todo "%begin_try_region"
let make_block = todo "%make_block"

module OfFlambda = struct

let nullop env (op : Flambda_primitive.nullary_primitive) =
  match op with
  | Invalid kind -> invalid env kind
  | Optimised_out kind -> optimised_out env kind
  | Probe_is_enabled { name; enabled_at_init} ->
      probe_is_enabled env (name, enabled_at_init)
  | Enter_inlined_apply { dbg } -> enter_inlined_apply env dbg
  | Dls_get -> dls_get env ()
  | Tls_get -> tls_get env ()
  | Poll -> poll env ()
  | Cpu_relax -> cpu_relax env ()

let unop env (op : Flambda_primitive.unary_primitive) =
  match op with
  | Block_load { kind; mut; field } -> block_load env (kind, mut, field)
  | Array_length ak -> array_length env ak
  | Box_number (bk, alloc) ->
    box_number env (bk, (* alloc_mode_for_allocations env *) alloc)
  | Tag_immediate -> tag_immediate env ()
  | Get_tag -> get_tag env ()
  | End_region { ghost } -> end_region env ghost
  | End_try_region { ghost } -> end_try_region env ghost
  | Int_arith (i, o) -> int_arith_u env (i, o)
  | Is_flat_float_array -> is_flat_float_array env ()
  | Is_int { variant_only } -> is_int env variant_only (* CR vlaviron: discuss *)
  | Is_null -> is_null env ()
  | Num_conv { src; dst } -> num_conv env (src, dst)
  | Opaque_identity { middle_end_only; kind } -> opaque_identity env (middle_end_only, kind)
  | Unbox_number bk -> unbox_number env bk
  | Untag_immediate -> untag_immediate env ()
  | Project_value_slot { project_from; value_slot } ->
      project_value_slot env
        ((* Env.translate_function_slot env *) project_from,
         (* Env.translate_value_slot env *) value_slot)
  | Project_function_slot { move_from; move_to } ->
      project_function_slot env
    ((* Env.translate_function_slot env *) move_from,
     (* Env.translate_function_slot env *) move_to)
  | String_length string_or_bytes -> string_length env string_or_bytes
  | Boolean_not -> boolean_not env ()
  | Int_as_pointer _ | Duplicate_block _ | Duplicate_array _ | Bigarray_length _
  | Float_arith _ | Reinterpret_64_bit_word _ | Is_boxed_float | Obj_dup
  | Get_header | Peek _ | Make_lazy _ ->
      todo
        (Format.asprintf "%a"
           Flambda_primitive.Without_args.print
           (Flambda_primitive.Without_args.Unary op))
        env ()

let binop env (op : Flambda_primitive.binary_primitive) =
  match op with
  | Block_set { kind; init; field } ->
    (* let kind = block_access_kind kind in *)
    (* let init = init_or_assign env init in *)
    block_set env (kind, init, field)
  | Array_load (ak, width, mut) -> array_load env (ak, width, mut)
  | Phys_equal op -> phys_equal env op
  (* | Int_arith (Tagged_immediate, o) -> Infix (Int_arith o) *)
  | Int_arith (i, o) -> int_arith_b env (i, o)
  | Int_comp (i, c) -> int_comp env (i, c)
  (* | Int_shift (Tagged_immediate, s) -> Infix (Int_shift s) *)
  | Int_shift (i, s) -> int_shift env (i, s)
  (* | Float_arith (w, o) -> Infix (Float_arith (w, o)) *)
  | Float_arith (w, o) -> float_arith env (w, o)
  (* | Float_comp (w, c) -> Infix (Float_comp (w, c)) *)
  | Float_comp (w, c) -> float_comp env (w, c)
  | String_or_bigstring_load (slv, saw) -> string_or_bigstring_load env (slv, saw)
  | Bigarray_get_alignment align -> bigarray_get_alignment env align
  | Bigarray_load _ | Atomic_load_field _ | Poke _ | Read_offset _ ->
    Misc.fatal_errorf "TODO: Binary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Binary op)

let ternop env (op : Flambda_primitive.ternary_primitive) =
  match op with
  | Array_set (ak, ask) ->
    (* let ak = fexpr_of_array_kind ak in *)
    (* let ask = fexpr_of_array_set_kind env ask in *)
    array_set env (ak, ask)
  | Bytes_or_bigstring_set (blv, saw) -> bytes_or_bigstring_set env (blv, saw)
  | Bigarray_set _ | Atomic_field_int_arith _ | Atomic_set_field _
  | Atomic_exchange_field _ | Write_offset _ ->
    todo
      (Format.asprintf "%a"
         Flambda_primitive.Without_args.print
         (Flambda_primitive.Without_args.Ternary op))
      env ()

let quaternop env (op : Flambda_primitive.quaternary_primitive) =
  match op with
  | Atomic_compare_and_set_field _
  | Atomic_compare_exchange_field _ ->
      todo
      (Format.asprintf "%a"
         Flambda_primitive.Without_args.print
         (Flambda_primitive.Without_args.Quaternary op))
      env ()

let varop env (op : Flambda_primitive.variadic_primitive) =
  match op with
  | Begin_region { ghost } -> begin_region env ghost
  | Begin_try_region { ghost } -> begin_try_region env ghost
  | Make_block (Values (tag, _), mutability, alloc) ->
    (* let tag = tag |> Tag.Scannable.to_int in *)
    (* let alloc = alloc_mode_for_allocations env alloc in *)
    make_block env (tag, mutability, alloc)
  | Make_block ((Naked_floats | Mixed _), _, _) | Make_array _ ->
    todo
      (Format.asprintf "%a"
         Flambda_primitive.Without_args.print
         (Flambda_primitive.Without_args.Variadic op))
      env ()

let prim env (p : Flambda_primitive.t) : t * Simple.t list =
  match p with
  | Nullary op -> nullop env op, []
  | Unary (op, arg) -> unop env op, [arg]
  | Binary (op, arg1, arg2) -> binop env op, [arg1; arg2]
  | Ternary (op, arg1, arg2, arg3) -> ternop env op, [arg1; arg2; arg3]
  | Quaternary (op, arg1, arg2, arg3, arg4) ->
      quaternop env op, [arg1; arg2; arg3; arg4]
  | Variadic (op, args) -> varop env op, args
end

module ToFlambda = struct

let prim (env : Fexpr_to_flambda_commons.env) (p : t) (args : Simple.t list) : Flambda_primitive.t =
  match Hashtbl.find_opt prim_table p.prim with
  | None ->
      Misc.fatal_errorf "Unregistered primitive: %s" p.prim
  | Some conv -> conv env p args

end
