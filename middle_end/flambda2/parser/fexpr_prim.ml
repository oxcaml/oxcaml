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

(* Common cons *)
let or_unknown cons =
  L.(map
       { to_fl = (fun _ o ->
             match o with None -> Or_unknown.Unknown | Some v -> Or_unknown.known v);
         of_fl = (fun _ ouk ->
             match (ouk : _ Or_unknown.t) with
             | Unknown -> None
             | Known v -> Some v)
       }
    (option cons))

let target_ocaml_int : Target_ocaml_int.t value_lens =
  { of_fl = (fun _ t -> Target_ocaml_int.to_int t |> string_of_int |> wrap_loc);
    to_fl = (fun _ s ->
        (* CR mshinwell: Should get machine_width from fexpr context when
           available *)
        let mw = Target_system.Machine_width.Sixty_four in
        Target_ocaml_int.of_int mw @@ int_of_string (unwrap_loc s))
  }

let scannable_tag : Tag.Scannable.t value_lens =
  { of_fl = (fun _ t -> Tag.Scannable.to_int t |> string_of_int |> wrap_loc);
    to_fl = (fun _ s ->
        Tag.Scannable.create_exn @@ int_of_string (unwrap_loc s))
  }

let mutability =
  L.(default ~def:Mutability.Immutable @@
     constructor_flag Mutability.[
       "imm_uniq", Immutable_unique;
       "mut", Mutable ])

let standard_int =
  L.(default ~def:Flambda_kind.Standard_int.Tagged_immediate
     @@ constructor_flag
       ~no_match_handler:Flambda_kind.Standard_int.(function
           | Naked_int8 | Naked_int16 as i ->
               Misc.fatal_errorf "Unsupported %a" print i
           | Naked_immediate | Naked_int32
           | Naked_int64 | Naked_nativeint
           | Tagged_immediate -> assert false)
       Flambda_kind.Standard_int.[
         "imm", Naked_immediate;
         "int32", Naked_int32;
         "int64", Naked_int64;
         "nativeint", Naked_nativeint;
       ])

let standard_int_or_float =
  L.constructor_value
    Flambda_kind.Standard_int_or_float.[
      "tagged_imm", Tagged_immediate;
      "imm", Naked_immediate;
      "float", Naked_float;
      "int32", Naked_int32;
      "int64", Naked_int64;
      "nativeint", Naked_nativeint;
    ]

let int_shift_op =
  L.(constructor_flag
       ([
         "lsl", Lsl;
         "lsr", Lsr;
         "asr", Asr;
       ] : (string * Flambda_primitive.int_shift_op) list))

let unary_int_arith_op =
  L.(constructor_flag
       ([
         "bswp", Swap_byte_endianness;
       ] : (string * Flambda_primitive.unary_int_arith_op) list))

let binary_int_arith_op =
  L.(constructor_flag
       ([
         "add", Add;
         "sub", Sub;
         "mul", Mul;
         "div", Div;
         "mod", Mod;
         "and", And;
         "or", Or;
         "xor", Xor;
       ] : (string * Flambda_primitive.binary_int_arith_op) list))

let binary_float_arith_op =
  L.(constructor_flag
       ([
         "add", Add;
         "sub", Sub;
         "mul", Mul;
         "div", Div;
       ] : (string * Flambda_primitive.binary_float_arith_op) list))

let block_access_kind =
  let value =
    L.(param3
        (default ~def:Flambda_primitive.Block_access_field_kind.Any_value @@
         constructor_flag [ "imm", Flambda_primitive.Block_access_field_kind.Immediate ])
        (or_unknown @@ labeled "tag" scannable_tag)
        (or_unknown @@ labeled "size" target_ocaml_int))
  in
  let naked_float =
    L.(param2
         (flag "floaf")
         (or_unknown @@ labeled "size" target_ocaml_int))
  in
  L.(either
       ~no_match_handler:Flambda_primitive.Block_access_kind.(function
           | Mixed _ as bak ->
               Misc.fatal_errorf "Unsupported %a" print bak
           | Values _ | Naked_floats _ -> assert false)
       [ case
           ~box:(fun _ ((), size) ->
                 Flambda_primitive.Block_access_kind.Naked_floats { size })
           ~unbox:(fun _ bak ->
               match (bak : Flambda_primitive.Block_access_kind.t) with
               | Naked_floats { size } -> Some ((), size)
               | Values _ | Mixed _ -> None)
           naked_float;
       case
           ~box:(fun _ (field_kind, tag, size) ->
               Flambda_primitive.Block_access_kind.Values { field_kind; tag; size })
           ~unbox:(fun _ bak ->
               match (bak : Flambda_primitive.Block_access_kind.t) with
               | Values { field_kind; tag; size } ->
                   Some (field_kind, tag, size)
               | Naked_floats _ | Mixed _ -> None)
           value;
         ])

let string_accessor_width =
  { to_fl = (fun _ i : Flambda_primitive.string_accessor_width ->
      let i = unwrap_loc i in
      let m =
        match String.get i (String.length i-1) with
        | 'a' | 'u' as m -> Some m
        | _ -> None
      in
      match int_of_string i, m with
      | 8, _ -> Eight
      | 16, _ -> Sixteen
      | 32, _ -> Thirty_two
      | 64, _ -> Sixty_four
      | 128, Some 'a' -> One_twenty_eight {aligned = true}
      | 128, Some 'u' -> One_twenty_eight {aligned = false}
      | 256, Some 'a' -> Two_fifty_six {aligned = true}
      | 256, Some 'u' -> Two_fifty_six {aligned = false}
      | 512, Some 'a' -> Five_twelve {aligned = true}
      | 512, Some 'u' -> Five_twelve {aligned = false}
      | _ | exception _ ->
          Misc.fatal_error "invalid string accessor width");
    of_fl = (fun _ saw ->
        let s =
          match (saw : Flambda_primitive.string_accessor_width) with
          | Eight -> "8"
          | Sixteen -> "16"
          | Thirty_two -> "32"
          | Single -> "f32"
          | Sixty_four -> "64"
          | One_twenty_eight { aligned = false } -> "128u"
          | One_twenty_eight { aligned = true } -> "128a"
          | Two_fifty_six { aligned = false } -> "256u"
          | Two_fifty_six { aligned = true } -> "256a"
          | Five_twelve { aligned = false } -> "512u"
          | Five_twelve { aligned = true } -> "512a"
        in
        wrap_loc s)}

let init_or_assign =
  L.(default ~def:(Flambda_primitive.Init_or_assign.Assignment Alloc_mode.For_assignments.heap) @@
     constructor_flag Flambda_primitive.Init_or_assign.[
         "init", Initialization;
         "lassign", Assignment (Alloc_mode.For_assignments.local ())
       ])

let alloc_mode_for_allocation =
  L.(default ~def:Alloc_mode.For_allocations.heap @@
     either
       [case (labeled "local" string)
          ~box:(fun env r ->
              let region =
                if String.equal (unwrap_loc r) "toplevel"
                then env.toplevel_region
                else Fexpr_to_flambda_commons.find_var env r
              in
              Alloc_mode.For_allocations.local ~region)
          ~unbox:(fun env -> function
              | Alloc_mode.For_allocations.Local { region } ->
                  let r =
                    match Flambda_to_fexpr_commons.Env.find_region_exn env region with
                    | Fexpr.Toplevel -> wrap_loc "toplevel"
                    | Named s -> s
                  in
                  Some r
              | Alloc_mode.For_allocations.Heap -> None
            )])

let boxable_number =
  L.constructor_flag Flambda_kind.Boxable_number.[
      "float", Naked_float;
      "fnt32", Naked_float32;
      "int32", Naked_int32;
      "int64", Naked_int64;
      "nativeint", Naked_nativeint;
      "vec128", Naked_vec128;
      "vec256", Naked_vec256;
      "vec512", Naked_vec512;
    ]

let array_kind =
  L.(default ~def:Flambda_primitive.Array_kind.Values @@
     constructor_flag
     ~no_match_handler:(fun (k : Flambda_primitive.Array_kind.t) ->
           match k with
           | Immediates | Values | Naked_floats | Gc_ignorable_values -> assert false
           | Naked_float32s
           | Naked_int32s
           | Naked_int64s
           | Naked_nativeints
           | Naked_vec128s
           | Naked_vec256s
           | Naked_vec512s
           | Unboxed_product _ ->
               Misc.fatal_error
                 "fexpr support for arrays of unboxed elements not yet implemented"
         )
       Flambda_primitive.Array_kind.[
         "imm", Immediates;
         "float", Naked_floats;
         "gc_ign", Gc_ignorable_values
       ])

let array_kind_for_length =
  L.(either
    Flambda_primitive.Array_kind_for_length.[
      id_case @@ constructor_flag ["generic", Float_array_opt_dynamic];
      case array_kind
        ~box:(fun _ k -> Array_kind k)
        ~unbox:(fun _ -> function
            | Float_array_opt_dynamic -> None
            | Array_kind k -> Some k)
    ])

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
                                block_access_kind
                                mutability
                                (positional target_ocaml_int))
                      (fun _env (kind, mut, field) ->
                         Flambda_primitive.Block_load { kind; mut; field }))
let array_length =
  L.(unary "%array_length" ~params:array_kind_for_length
       (fun _ k -> Flambda_primitive.Array_length k))
let box_num = L.(unary "%box_num" ~params:
                      (param2
                       boxable_number
                       alloc_mode_for_allocation)
                      (fun _ (b, a) -> Flambda_primitive.Box_number (b, a)))
let tag_immediate = L.(unary "%tag_imm" ~params:no_param
                         (fun _ () -> Flambda_primitive.Tag_immediate))
let get_tag = L.(unary "%get_tag" ~params:no_param
                         (fun _ () -> Flambda_primitive.Get_tag))
let end_region = L.(unary "%end_region" ~params:no_param
                          (fun _ () -> Flambda_primitive.End_region {ghost = false}))
let end_try_region = L.(unary "%end_try_region" ~params:no_param
                          (fun _ () -> Flambda_primitive.End_try_region {ghost = false}))
let end_ghost_region = L.(unary "%end_ghost_region" ~params:no_param
                          (fun _ () -> Flambda_primitive.End_region {ghost = true}))
let end_try_ghost_region = L.(unary "%end_try_ghost_region" ~params:no_param
                          (fun _ () -> Flambda_primitive.End_try_region {ghost = true}))
let int_uarith =
  L.(unary "%int_uarith" ~params:
    (param2
       standard_int
       unary_int_arith_op)
    (fun _ (i, o) -> Flambda_primitive.Int_arith (i, o)))
let is_flat_float_array = L.(unary "%is_flat_float_array" ~params:no_param
                               (fun _ () -> Flambda_primitive.Is_flat_float_array))
let is_int = L.(unary "%is_int" ~params:no_param
                  (fun _ () -> Flambda_primitive.Is_int { variant_only = true } (* CR vlaviron: discuss *)))
let is_null = todo "%is_null"
let num_conv =
  L.(unary "%num_conv" ~params:
       (param2
          (positional standard_int_or_float)
          (positional standard_int_or_float))
       (fun _ (src, dst)-> Flambda_primitive.Num_conv { src; dst }))
let opaque_identity =
  L.(unary "%opaque" ~params:no_param
       (fun _ () -> Flambda_primitive.Opaque_identity
           { middle_end_only = false; kind = Flambda_kind.value }))
let unbox_num = L.(unary "%unbox_num" ~params:boxable_number
                     (fun _ b -> Flambda_primitive.Unbox_number b))
let untag_immediate = L.(unary "%untag_imm" ~params:no_param
                               (fun _ () -> Flambda_primitive.Untag_immediate))
let project_value_slot =
  (* CR mshinwell: support non-value kinds *)
  let kind = Flambda_kind.value in
  L.(unary "%project_value_slot" ~params:
       (param2
          (map
             { to_fl = (fun env pf ->
                   Fexpr_to_flambda_commons.fresh_or_existing_function_slot env pf);
               of_fl = (fun env pf ->
                   Flambda_to_fexpr_commons.Env.translate_function_slot env pf)
             }
             (positional string))
          (map
             { to_fl = (fun env vs ->
                   Fexpr_to_flambda_commons.fresh_or_existing_value_slot env vs kind);
               of_fl = (fun env vs ->
                   Flambda_to_fexpr_commons.Env.translate_value_slot env vs)
             }
             (positional string)))
       (fun _ (project_from, value_slot) ->
          Flambda_primitive.Project_value_slot { project_from; value_slot }))
let project_function_slot =
  L.(unary "%project_function_slot" ~params:
       (param2
          (map
             { to_fl = (fun env mf ->
                   Fexpr_to_flambda_commons.fresh_or_existing_function_slot env mf);
               of_fl = (fun env mf ->
                   Flambda_to_fexpr_commons.Env.translate_function_slot env mf)
             }
             (positional string))
          (map
             { to_fl = (fun env mt ->
                   Fexpr_to_flambda_commons.fresh_or_existing_function_slot env mt);
               of_fl = (fun env mt ->
                   Flambda_to_fexpr_commons.Env.translate_function_slot env mt)
             }
             (positional string)))
       (fun _ (move_from, move_to) ->
          Flambda_primitive.Project_function_slot { move_from; move_to }))
let string_length =
  L.(unary "%string_length" ~params:no_param
       (fun _ () -> Flambda_primitive.String_length String))
let bytes_length =
  L.(unary "%bytes_length" ~params:no_param
       (fun _ () -> Flambda_primitive.String_length String))
let boolean_not = L.(unary "%boolean_not" ~params:no_param
                       (fun _ () -> Flambda_primitive.Boolean_not))

(* Binaries *)
let block_set =
L.(binary "%block_set" ~params:
     (param3
       block_access_kind
       init_or_assign
       (positional target_ocaml_int))
     (fun _ (kind, init, field) -> Flambda_primitive.Block_set { kind; init; field } ))
let array_load =
  L.(binary "%array_load" ~params:
       (map
          { to_fl = (fun _ (k, m) ->
                let lk : Flambda_primitive.Array_load_kind.t =
                  match (k : Flambda_primitive.Array_kind.t) with
                  | Immediates -> Immediates
                  | Gc_ignorable_values -> Gc_ignorable_values
                  | Values -> Values
                  | Naked_floats -> Naked_floats
                  | Naked_float32s -> Naked_float32s
                  | Naked_int32s -> Naked_int32s
                  | Naked_int64s -> Naked_int64s
                  | Naked_nativeints -> Naked_nativeints
                  | Naked_vec128s -> Naked_vec128s
                  | Naked_vec256s -> Naked_vec256s
                  | Naked_vec512s -> Naked_vec512s
                  | Unboxed_product _ ->
                      Misc.fatal_error "Unboxed product array ops not supported"
                in
                (k, lk, m));
            of_fl = (fun _ (k, _, m) -> (k, m)) }
          (param2 array_kind mutability))
       (fun _ (k, lk, m) -> Flambda_primitive.Array_load (k, lk, m)))
let phys_eq = L.(binary "%phys_eq" ~params:no_param
                      (fun _ () -> Flambda_primitive.Phys_equal Eq))
let phys_ne = L.(binary "%phys_ne" ~params:no_param
                      (fun _ () -> Flambda_primitive.Phys_equal Neq))
let int_barith = L.(binary "%int_barith" ~params:
                      (param2 standard_int binary_int_arith_op)
                   (fun _ (i, o) -> Flambda_primitive.Int_arith (i, o)))
let int_comp =
  let open L in
  let open Flambda_primitive in
  let sign =
    default ~def:Signed @@
    constructor_flag ["unsigned", Unsigned]
  in
  let[@warning "-fragile-match"] comp =
    either
      [ case (param2 sign (flag "lt"))
          ~box:(fun _ (s,()) -> Yielding_bool (Lt s))
          ~unbox:(fun _ -> function
              | Yielding_bool (Lt s) -> Some (s,())
              | Yielding_bool (Le _ | Gt _ | Ge _ | Eq | Neq)
              | Yielding_int_like_compare_functions _ -> None);
        case (param2 sign (flag "le"))
          ~box:(fun _ (s,()) -> Yielding_bool (Le s))
          ~unbox:(fun _ -> function
              | Yielding_bool (Le s) -> Some (s,())
              | _ -> None);
        case (param2 sign (flag "gt"))
          ~box:(fun _ (s,()) -> Yielding_bool (Gt s))
          ~unbox:(fun _ -> function
              | Yielding_bool (Gt s) -> Some (s,())
              | _ -> None);
        case (param2 sign (flag "ge"))
          ~box:(fun _ (s,()) -> Yielding_bool (Ge s))
          ~unbox:(fun _ -> function
              | Yielding_bool (Ge s) -> Some (s,())
              | _ -> None);
        id_case (constructor_flag [
            "eq", Yielding_bool Eq;
            "ne", Yielding_bool Neq]);
        case (param2 sign (flag "qmark"))
          ~box:(fun _ (s,()) -> Yielding_int_like_compare_functions s)
          ~unbox:(fun _ -> function
              | Yielding_int_like_compare_functions s -> Some (s,())
              | _ -> None)
      ]
  in
  binary "%int_comp" ~params:
    (param2 standard_int comp)
    (fun _ (i, c) -> Flambda_primitive.Int_comp (i, c))
let int_shift =
  L.(binary "%int_shift" ~params:
       (param2 standard_int int_shift_op)
       (fun _ (i, o) -> Flambda_primitive.Int_shift (i, o)))
let float_arith =
  L.(binary "%float_arith" ~params:binary_float_arith_op
    (fun _ op -> Flambda_primitive.Float_arith (Float64, op)))
let float_comp = todo "%float_comp"
let string_load =
  L.(binary "%string_load" ~params:
       (positional string_accessor_width)
       (fun _ saw -> Flambda_primitive.String_or_bigstring_load (String, saw)))
let bytes_load =
  L.(binary "%bytes_load" ~params:
       (positional string_accessor_width)
       (fun _ saw -> Flambda_primitive.String_or_bigstring_load (Bytes, saw)))
let bigstring_load =
  L.(binary "%bigstring_load" ~params:
       (positional string_accessor_width)
       (fun _ saw -> Flambda_primitive.String_or_bigstring_load (Bigstring, saw)))
let bigarray_get_alignment = todo "%bigarray_get_alignment"

(* Ternaries *)
let array_set =
  L.(ternary "%array_set" ~params:
       (map
          { to_fl = (fun _ (k, ia) ->
                let sk : Flambda_primitive.Array_set_kind.t =
                  match (k : Flambda_primitive.Array_kind.t) with
                  | Values -> Values ia
                  | Immediates -> Immediates
                  | Gc_ignorable_values -> Gc_ignorable_values
                  | Naked_floats -> Naked_floats
                  | Naked_float32s -> Naked_float32s
                  | Naked_int32s -> Naked_int32s
                  | Naked_int64s -> Naked_int64s
                  | Naked_nativeints -> Naked_nativeints
                  | Naked_vec128s -> Naked_vec128s
                  | Naked_vec256s -> Naked_vec256s
                  | Naked_vec512s -> Naked_vec512s
                  | Unboxed_product _ ->
                      Misc.fatal_error "Unboxed product array ops not supported"
                in
                k, sk
              );
            of_fl = (fun _ (k, sk) : (Flambda_primitive.Array_kind.t * _) ->
                let no_ai = Flambda_primitive.Init_or_assign.Assignment Alloc_mode.For_assignments.heap in
                let ai =
                  match (sk : Flambda_primitive.Array_set_kind.t) with
                  | Values ai -> ai
                | Immediates
                | Gc_ignorable_values
                | Naked_floats
                | Naked_float32s
                | Naked_int32s
                | Naked_int64s
                | Naked_nativeints
                | Naked_vec128s
                | Naked_vec256s
                | Naked_vec512s -> no_ai
                in
                k, ai
              )}
          (param2 array_kind init_or_assign))
       (fun _ (k, sk) -> Flambda_primitive.Array_set (k, sk)))
let bytes_or_bigstring_set =
  L.(ternary "%bytes_or_bigstring_set" ~params:
       (param2
          (constructor_flag [
              "bytes", Flambda_primitive.Bytes;
              "bigstring", Flambda_primitive.Bigstring])
          (positional string_accessor_width))
       (fun _ (blv, saw) -> Flambda_primitive.Bytes_or_bigstring_set (blv, saw))
    )

(* Quaternaries *)

(* Variadics *)
let begin_region = L.(variadic "%begin_region" ~params:no_param
                          (fun _ () _ -> Flambda_primitive.Begin_region {ghost = false}))
let begin_try_region = L.(variadic "%begin_try_region" ~params:no_param
                          (fun _ () _ -> Flambda_primitive.Begin_try_region {ghost = false}))
let begin_ghost_region = L.(variadic "%begin_ghost_region" ~params:no_param
                          (fun _ () _ -> Flambda_primitive.Begin_region {ghost = true}))
let begin_try_ghost_region = L.(variadic "%begin_try_ghost_region" ~params:no_param
                          (fun _ () _ -> Flambda_primitive.Begin_try_region {ghost = true}))
let make_block =
  L.(variadic "%block" ~params:
       (param3
          mutability
          (positional scannable_tag)
          alloc_mode_for_allocation)
       (fun _ (m, t, a) n ->
          let kind =
            Flambda_primitive.Block_kind.Values
              (t, List.init n (fun _ -> Flambda_kind.With_subkind.any_value))
          in
          Flambda_primitive.Make_block (kind, m, a)))

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
  | Box_number (bk, alloc) -> box_num env (bk, alloc)
  | Tag_immediate -> tag_immediate env ()
  | Get_tag -> get_tag env ()
  | End_region { ghost = false } -> end_region env ()
  | End_try_region { ghost = false } -> end_try_region env ()
  | End_region { ghost = true } -> end_ghost_region env ()
  | End_try_region { ghost = true } -> end_try_ghost_region env ()
  | Int_arith (i, o) -> int_uarith env (i, o)
  | Is_flat_float_array -> is_flat_float_array env ()
  | Is_int _ -> is_int env () (* CR vlaviron: discuss *)
  | Is_null -> is_null env ()
  | Num_conv { src; dst } -> num_conv env (src, dst)
  | Opaque_identity _ -> opaque_identity env ()
  | Unbox_number bk -> unbox_num env bk
  | Untag_immediate -> untag_immediate env ()
  | Project_value_slot { project_from; value_slot } ->
      project_value_slot env (project_from, value_slot)
  | Project_function_slot { move_from; move_to } ->
      project_function_slot env (move_from, move_to)
  | String_length String -> string_length env ()
  | String_length Bytes -> bytes_length env ()
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
  | Phys_equal Eq -> phys_eq env ()
  | Phys_equal Neq -> phys_ne env ()
  (* | Int_arith (Tagged_immediate, o) -> Infix (Int_arith o) *)
  | Int_arith (i, o) -> int_barith env (i, o)
  | Int_comp (i, c) -> int_comp env (i, c)
  (* | Int_shift (Tagged_immediate, s) -> Infix (Int_shift s) *)
  | Int_shift (i, s) -> int_shift env (i, s)
  (* | Float_arith (w, o) -> Infix (Float_arith (w, o)) *)
  | Float_arith (Float64, o) -> float_arith env o
  (* | Float_comp (w, c) -> Infix (Float_comp (w, c)) *)
  | Float_comp (w, c) -> float_comp env (w, c)
  | String_or_bigstring_load (String, saw) -> string_load env saw
  | String_or_bigstring_load (Bytes, saw) -> bytes_load env saw
  | String_or_bigstring_load (Bigstring, saw) -> bigstring_load env saw
  | Bigarray_get_alignment align -> bigarray_get_alignment env align
  | Float_arith (Float32, _) | Bigarray_load _ | Atomic_load_field _ | Poke _ | Read_offset _ ->
    Misc.fatal_errorf "TODO: Binary primitive: %a"
      Flambda_primitive.Without_args.print
      (Flambda_primitive.Without_args.Binary op)

let ternop env (op : Flambda_primitive.ternary_primitive) =
  match op with
  | Array_set (k, sk) -> array_set env (k, sk)
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
  | Begin_region { ghost = false } -> begin_region env ()
  | Begin_try_region { ghost = false } -> begin_try_region env ()
  | Begin_region { ghost = true } -> begin_ghost_region env ()
  | Begin_try_region { ghost = true } -> begin_try_ghost_region env ()
  | Make_block (Values (tag, _), mutability, alloc) ->
    make_block env (mutability, tag, alloc)
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
