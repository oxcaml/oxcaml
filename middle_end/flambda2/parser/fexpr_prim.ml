open Fexpr_prim_descr
module D = Describe
module P = Flambda_primitive

let todo = D.todo

(* Common cons *)
let or_unknown cons =
  D.(
    maps (option cons)
      ~from:(fun _ o ->
        match o with None -> Or_unknown.Unknown | Some v -> Or_unknown.known v)
      ~to_:(fun _ ouk ->
        match (ouk : _ Or_unknown.t) with Unknown -> None | Known v -> Some v))

let target_ocaml_int : Target_ocaml_int.t value_lens =
  { encode = (fun _ t -> Target_ocaml_int.to_int t |> string_of_int |> wrap_loc);
    decode =
      (fun _ s ->
        (* CR mshinwell: Should get machine_width from fexpr context when
           available *)
        let mw = Target_system.Machine_width.Sixty_four in
        Target_ocaml_int.of_int mw @@ int_of_string (unwrap_loc s))
  }

let scannable_tag : Tag.Scannable.t value_lens =
  { encode = (fun _ t -> Tag.Scannable.to_int t |> string_of_int |> wrap_loc);
    decode =
      (fun _ s -> Tag.Scannable.create_exn @@ int_of_string (unwrap_loc s))
  }

let mutability =
  D.(
    default ~def:Mutability.Immutable
    @@ constructor_flag Mutability.["imm_uniq", Immutable_unique; "mut", Mutable])

let mutability_as_value =
  { decode =
      (fun _ m : Mutability.t ->
        match unwrap_loc m with
        | "immut" -> Immutable
        | "immut_uniq" -> Immutable_unique
        | "mut" -> Mutable
        | _ -> Misc.fatal_error "invalid mutability");
    encode =
      (fun _ m ->
        let s =
          match (m : Mutability.t) with
          | Immutable -> "immut"
          | Immutable_unique -> "immut_uniq"
          | Mutable -> "mut"
        in
        wrap_loc s)
  }

let standard_int =
  D.(
    default ~def:Flambda_kind.Standard_int.Tagged_immediate
    @@ constructor_flag
         Flambda_kind.Standard_int.
           [ "imm", Naked_immediate;
             "int8", Naked_int8;
             "int16", Naked_int16;
             "int32", Naked_int32;
             "int64", Naked_int64;
             "nativeint", Naked_nativeint ])

let standard_int_or_float =
  D.constructor_value
    Flambda_kind.Standard_int_or_float.
      [ "tagged_imm", Tagged_immediate;
        "imm", Naked_immediate;
        "float", Naked_float;
        "float32", Naked_float32;
        "int8", Naked_int8;
        "int16", Naked_int16;
        "int32", Naked_int32;
        "int64", Naked_int64;
        "nativeint", Naked_nativeint ]

let int_shift_op =
  D.(
    constructor_flag
      (["lsl", Lsl; "lsr", Lsr; "asr", Asr] : (string * P.int_shift_op) list))

let unary_int_arith_op =
  D.(
    constructor_flag
      (["bswp", Swap_byte_endianness] : (string * P.unary_int_arith_op) list))

let binary_int_arith_op =
  D.(
    constructor_flag
      ([ "add", Add;
         "sub", Sub;
         "mul", Mul;
         "div", Div;
         "mod", Mod;
         "and", And;
         "or", Or;
         "xor", Xor ]
        : (string * P.binary_int_arith_op) list))

let float_bitwidth =
  D.(
    default ~def:P.Float64
    @@ constructor_flag ["float32", (Float32 : P.float_bitwidth)])

let unary_float_arith_op = D.constructor_flag P.["abs", Abs; "neg", Neg]

let binary_float_arith_op =
  D.(
    constructor_flag
      (["add", Add; "sub", Sub; "mul", Mul; "div", Div]
        : (string * P.binary_float_arith_op) list))

let block_access_field_kind =
  D.(
    default ~def:P.Block_access_field_kind.Any_value
    @@ constructor_flag ["imm", P.Block_access_field_kind.Immediate])

let block_access_kind =
  let value =
    D.(
      param3 block_access_field_kind
        (or_unknown @@ labeled "tag" scannable_tag)
        (or_unknown @@ labeled "size" target_ocaml_int))
  in
  let naked_float =
    D.(param2 (flag "float") (or_unknown @@ labeled "size" target_ocaml_int))
  in
  D.(
    either
      ~no_match_handler:
        P.Block_access_kind.(
          function
          | Mixed _ as bak -> Misc.fatal_errorf "Unsupported %a" print bak
          | Values _ | Naked_floats _ -> assert false)
      [ case
          ~box:(fun _ ((), size) -> P.Block_access_kind.Naked_floats { size })
          ~unbox:(fun _ bak ->
            match (bak : P.Block_access_kind.t) with
            | Naked_floats { size } -> Some ((), size)
            | Values _ | Mixed _ -> None)
          naked_float;
        case
          ~box:(fun _ (field_kind, tag, size) ->
            P.Block_access_kind.Values { field_kind; tag; size })
          ~unbox:(fun _ bak ->
            match (bak : P.Block_access_kind.t) with
            | Values { field_kind; tag; size } -> Some (field_kind, tag, size)
            | Naked_floats _ | Mixed _ -> None)
          value ])

let string_accessor_width =
  { decode =
      (fun _ i : P.string_accessor_width ->
        let i = unwrap_loc i in
        match i with
        | "f32" -> Single
        | "8" -> Eight
        | "16" -> Sixteen
        | "32" -> Thirty_two
        | "64" -> Sixty_four
        | "128a" -> One_twenty_eight { aligned = true }
        | "128u" -> One_twenty_eight { aligned = false }
        | "256a" -> Two_fifty_six { aligned = true }
        | "256u" -> Two_fifty_six { aligned = false }
        | "512a" -> Five_twelve { aligned = true }
        | "512u" -> Five_twelve { aligned = false }
        | _ -> Misc.fatal_errorf "invalid string accessor width '%s'" i);
    encode =
      (fun _ saw ->
        let s =
          match (saw : P.string_accessor_width) with
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
        wrap_loc s)
  }

let init_or_assign =
  D.(
    default ~def:(P.Init_or_assign.Assignment Alloc_mode.For_assignments.heap)
    @@ constructor_flag
         P.Init_or_assign.
           [ "init", Initialization;
             "lassign", Assignment (Alloc_mode.For_assignments.local ()) ])

let alloc_mode_for_allocation =
  D.(
    default ~def:Alloc_mode.For_allocations.heap
    @@ either
         [ case (labeled "local" string)
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
                   match
                     Flambda_to_fexpr_commons.Env.find_region_exn env region
                   with
                   | Fexpr.Toplevel -> wrap_loc "toplevel"
                   | Named s -> s
                 in
                 Some r
               | Alloc_mode.For_allocations.Heap -> None) ])

let boxable_number =
  D.constructor_flag
    Flambda_kind.Boxable_number.
      [ "float", Naked_float;
        "fnt32", Naked_float32;
        "int32", Naked_int32;
        "int64", Naked_int64;
        "nativeint", Naked_nativeint;
        "vec128", Naked_vec128;
        "vec256", Naked_vec256;
        "vec512", Naked_vec512 ]

let array_kind =
  D.(
    default ~def:P.Array_kind.Values
    @@ constructor_flag
         ~no_match_handler:(fun (k : P.Array_kind.t) ->
           match k with
           | Immediates | Values | Naked_floats | Gc_ignorable_values ->
             assert false
           | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
           | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
           | Naked_vec256s | Naked_vec512s | Unboxed_product _ ->
             Misc.fatal_error
               "fexpr support for arrays of unboxed elements not yet \
                implemented")
         P.Array_kind.
           [ "imm", Immediates;
             "float", Naked_floats;
             "gc_ign", Gc_ignorable_values ])

let array_kind_for_length =
  D.(
    either
      P.Array_kind_for_length.
        [ id_case @@ constructor_flag ["generic", Float_array_opt_dynamic];
          case array_kind
            ~box:(fun _ k -> Array_kind k)
            ~unbox:(fun _ -> function
              | Float_array_opt_dynamic -> None | Array_kind k -> Some k) ])

let lazy_tag =
  D.(
    default ~def:Lambda.Lazy_tag
    @@ constructor_flag ["forward", Lambda.Forward_tag])

let duplicate_array_kind =
  D.(
    default ~def:P.Duplicate_array_kind.Values
    @@ constructor_flag
         ~no_match_handler:(fun k ->
           match (k : P.Duplicate_array_kind.t) with
           | Values | Immediates -> assert false
           | Naked_floats _ | Naked_float32s _ | Naked_ints _ | Naked_int8s _
           | Naked_int16s _ | Naked_int32s _ | Naked_int64s _
           | Naked_nativeints _ | Naked_vec128s _ | Naked_vec256s _
           | Naked_vec512s _ ->
             Misc.fatal_error
               "fexpr support for duplication of array of unboxed element not \
                yet implemented")
         P.Duplicate_array_kind.["imm", Immediates])

let bigarray_kind =
  D.(
    constructor_flag
      P.Bigarray_kind.
        [ "float16", Float16;
          "float32", Float32;
          "float32_t", Float32_t;
          "float64", Float64;
          "sint8", Sint8;
          "uint8", Uint8;
          "sint16", Sint16;
          "uint16", Uint16;
          "int32", Int32;
          "int64", Int64;
          "int_width_int", Int_width_int;
          "targetint_width_int", Targetint_width_int;
          "complex32", Complex32;
          "complex64", Complex64 ])

let bigarray_layout =
  D.(
    default ~def:P.Bigarray_layout.C
    @@ constructor_flag ["fortran", P.Bigarray_layout.Fortran])

let reinterp_64bit_word =
  D.constructor_flag
    P.Reinterpret_64_bit_word.
      [ "int63_as_int64", Tagged_int63_as_unboxed_int64;
        "int64_as_int63", Unboxed_int64_as_tagged_int63;
        "int64_as_float64", Unboxed_int64_as_unboxed_float64;
        "float64_as_int64", Unboxed_float64_as_unboxed_int64 ]

let int_atomic_op =
  D.constructor_flag
    P.
      [ "fetch_add", Fetch_add;
        "add", Add;
        "sub", Sub;
        "and", And;
        "or", Or;
        "xor", Xor ]

(* Nullaries *)
let invalid =
  D.(
    nullary "%invalid" ~params:(todop "Flambda_kind") (fun _env kind ->
        P.Invalid kind))

let optimised_out =
  D.(
    nullary "%optimised_out" ~params:(todop "Flambda_kind") (fun _env kind ->
        P.Optimised_out kind))

let probe_is_enabled =
  D.(
    nullary "%probe_is_enable"
      ~params:(param2 (todop "probe_name") (todop "probe_init"))
      (fun _env (name, enabled_at_init) ->
        P.Probe_is_enabled { name; enabled_at_init }))

let enter_inlined_apply =
  D.(
    nullary "%inlined_apply" ~params:(todop "dbginfo") (fun _env dbg ->
        P.Enter_inlined_apply { dbg }))

let domain_index =
  D.(nullary "%domain_index" ~params:param0 (fun _env () -> P.Domain_index))

let dls_get = D.(nullary "%dls_get" ~params:param0 (fun _env () -> P.Dls_get))

let tls_get = D.(nullary "%tls_get" ~params:param0 (fun _env () -> P.Tls_get))

let poll = D.(nullary "%poll" ~params:param0 (fun _env () -> P.Poll))

let cpu_relax =
  D.(nullary "%cpu_relax" ~params:param0 (fun _env () -> P.Cpu_relax))

(* Unaries *)
let block_load =
  D.(
    unary "%block_load"
      ~params:
        (param3 block_access_kind mutability (positional target_ocaml_int))
      (fun _env (kind, mut, field) -> P.Block_load { kind; mut; field }))

let bigarray_length =
  D.(
    unary "%bigarray_lenght" ~params:(positional int) (fun _ dimension ->
        P.Bigarray_length { dimension }))

let array_length =
  D.(
    unary "%array_length" ~params:array_kind_for_length (fun _ k ->
        P.Array_length k))

let box_num =
  D.(
    unary "%box_num" ~params:(param2 boxable_number alloc_mode_for_allocation)
      (fun _ (b, a) -> P.Box_number (b, a)))

let tag_immediate =
  D.(unary "%tag_imm" ~params:param0 (fun _ () -> P.Tag_immediate))

let get_tag = D.(unary "%get_tag" ~params:param0 (fun _ () -> P.Get_tag))

let end_region =
  D.(
    unary "%end_region" ~params:param0 (fun _ () ->
        P.End_region { ghost = false }))

let end_try_region =
  D.(
    unary "%end_try_region" ~params:param0 (fun _ () ->
        P.End_try_region { ghost = false }))

let end_ghost_region =
  D.(
    unary "%end_ghost_region" ~params:param0 (fun _ () ->
        P.End_region { ghost = true }))

let end_try_ghost_region =
  D.(
    unary "%end_try_ghost_region" ~params:param0 (fun _ () ->
        P.End_try_region { ghost = true }))

let duplicate_array =
  D.(
    unary "%duplicate_array"
      ~params:
        (param3 duplicate_array_kind
           (positional mutability_as_value)
           (positional mutability_as_value))
      (fun _ (kind, source_mutability, destination_mutability) ->
        P.Duplicate_array { kind; source_mutability; destination_mutability }))

let int_as_pointer =
  D.(
    unary "%int_as_pointer" ~params:alloc_mode_for_allocation (fun _ a ->
        P.Int_as_pointer a))

let int_uarith =
  D.(
    unary "%int_uarith" ~params:(param2 standard_int unary_int_arith_op)
      (fun _ (i, o) -> P.Int_arith (i, o)))

let is_boxed_float =
  D.(unary "%is_boxed_float" ~params:param0 (fun _ () -> P.Is_boxed_float))

let is_flat_float_array =
  D.(
    unary "%is_flat_float_array" ~params:param0 (fun _ () ->
        P.Is_flat_float_array))

let is_int =
  D.(
    unary "%is_int" ~params:param0 (fun _ () ->
        P.Is_int { variant_only = true } (* CR vlaviron: discuss *)))

let is_null = D.(unary "%is_null" ~params:param0 (fun _ () -> P.Is_null))

let num_conv =
  D.(
    unary "%num_conv"
      ~params:
        (param2
           (positional standard_int_or_float)
           (positional standard_int_or_float))
      (fun _ (src, dst) -> P.Num_conv { src; dst }))

let make_lazy = D.(unary "%lazy" ~params:lazy_tag (fun _ lt -> P.Make_lazy lt))

let opaque_identity =
  D.(
    unary "%opaque" ~params:param0 (fun _ () ->
        P.Opaque_identity { middle_end_only = false; kind = Flambda_kind.value }))

let reinterpret_64_bit_word =
  D.(
    unary "%reinterpret_64_bit_word" ~params:reinterp_64bit_word (fun _ r ->
        P.Reinterpret_64_bit_word r))

let ufloat_arith =
  D.(
    unary "%ufloat_arith" ~params:(param2 float_bitwidth unary_float_arith_op)
      (fun _ (w, o) -> Float_arith (w, o)))

let unbox_num =
  D.(unary "%unbox_num" ~params:boxable_number (fun _ b -> P.Unbox_number b))

let untag_immediate =
  D.(unary "%untag_imm" ~params:param0 (fun _ () -> P.Untag_immediate))

let project_value_slot =
  (* CR mshinwell: support non-value kinds *)
  let kind = Flambda_kind.value in
  D.(
    unary "%project_value_slot"
      ~params:
        (param2
           (maps (positional string)
              ~from:(fun env pf ->
                Fexpr_to_flambda_commons.fresh_or_existing_function_slot env pf)
              ~to_:(fun env pf ->
                Flambda_to_fexpr_commons.Env.translate_function_slot env pf))
           (maps (positional string)
              ~from:(fun env vs ->
                Fexpr_to_flambda_commons.fresh_or_existing_value_slot env vs
                  kind)
              ~to_:(fun env vs ->
                Flambda_to_fexpr_commons.Env.translate_value_slot env vs)))
      (fun _ (project_from, value_slot) ->
        P.Project_value_slot { project_from; value_slot }))

let project_function_slot =
  D.(
    unary "%project_function_slot"
      ~params:
        (param2
           (maps (positional string)
              ~from:(fun env mf ->
                Fexpr_to_flambda_commons.fresh_or_existing_function_slot env mf)
              ~to_:(fun env mf ->
                Flambda_to_fexpr_commons.Env.translate_function_slot env mf))
           (maps (positional string)
              ~from:(fun env mt ->
                Fexpr_to_flambda_commons.fresh_or_existing_function_slot env mt)
              ~to_:(fun env mt ->
                Flambda_to_fexpr_commons.Env.translate_function_slot env mt)))
      (fun _ (move_from, move_to) ->
        P.Project_function_slot { move_from; move_to }))

let string_length =
  D.(unary "%string_length" ~params:param0 (fun _ () -> P.String_length String))

let bytes_length =
  D.(unary "%bytes_length" ~params:param0 (fun _ () -> P.String_length String))

let boolean_not =
  D.(unary "%boolean_not" ~params:param0 (fun _ () -> P.Boolean_not))

(* Binaries *)
let atomic_load_field =
  D.(
    binary "%atomic_load_field" ~params:block_access_field_kind (fun _ kind ->
        P.Atomic_load_field kind))

let block_set =
  D.(
    binary "%block_set"
      ~params:
        (param3 block_access_kind init_or_assign (positional target_ocaml_int))
      (fun _ (kind, init, field) -> P.Block_set { kind; init; field }))

let array_load =
  D.(
    binary "%array_load"
      ~params:
        (maps
           (param2 array_kind mutability)
           ~from:(fun _ (k, m) ->
             let lk : P.Array_load_kind.t =
               match (k : P.Array_kind.t) with
               | Immediates -> Immediates
               | Gc_ignorable_values -> Gc_ignorable_values
               | Values -> Values
               | Naked_floats -> Naked_floats
               | Naked_float32s -> Naked_float32s
               | Naked_ints -> Naked_ints
               | Naked_int8s -> Naked_int8s
               | Naked_int16s -> Naked_int16s
               | Naked_int32s -> Naked_int32s
               | Naked_int64s -> Naked_int64s
               | Naked_nativeints -> Naked_nativeints
               | Naked_vec128s -> Naked_vec128s
               | Naked_vec256s -> Naked_vec256s
               | Naked_vec512s -> Naked_vec512s
               | Unboxed_product _ ->
                 Misc.fatal_error "Unboxed product array ops not supported"
             in
             k, lk, m)
           ~to_:(fun _ (k, _, m) -> k, m))
      (fun _ (k, lk, m) -> P.Array_load (k, lk, m)))

let bigarray_load =
  D.(
    binary "%bigarray_load"
      ~params:(param3 (positional int) bigarray_kind bigarray_layout)
      (fun _ (d, k, l) -> P.Bigarray_load (d, k, l)))

let phys_eq = D.(binary "%phys_eq" ~params:param0 (fun _ () -> P.Phys_equal Eq))

let phys_ne =
  D.(binary "%phys_ne" ~params:param0 (fun _ () -> P.Phys_equal Neq))

let int_barith =
  D.(
    binary "%int_barith" ~params:(param2 standard_int binary_int_arith_op)
      (fun _ (i, o) -> P.Int_arith (i, o)))

let int_comp =
  let open D in
  let open Flambda_primitive in
  let sign = default ~def:Signed @@ constructor_flag ["unsigned", Unsigned] in
  let[@warning "-fragile-match"] comp =
    either
      [ case
          (param2 sign (flag "lt"))
          ~box:(fun _ (s, ()) -> Yielding_bool (Lt s))
          ~unbox:(fun _ -> function
            | Yielding_bool (Lt s) -> Some (s, ())
            | Yielding_bool (Le _ | Gt _ | Ge _ | Eq | Neq)
            | Yielding_int_like_compare_functions _ ->
              None);
        case
          (param2 sign (flag "le"))
          ~box:(fun _ (s, ()) -> Yielding_bool (Le s))
          ~unbox:(fun _ -> function
            | Yielding_bool (Le s) -> Some (s, ()) | _ -> None);
        case
          (param2 sign (flag "gt"))
          ~box:(fun _ (s, ()) -> Yielding_bool (Gt s))
          ~unbox:(fun _ -> function
            | Yielding_bool (Gt s) -> Some (s, ()) | _ -> None);
        case
          (param2 sign (flag "ge"))
          ~box:(fun _ (s, ()) -> Yielding_bool (Ge s))
          ~unbox:(fun _ -> function
            | Yielding_bool (Ge s) -> Some (s, ()) | _ -> None);
        case
          (param2 sign (flag "qmark"))
          ~box:(fun _ (s, ()) -> Yielding_int_like_compare_functions s)
          ~unbox:(fun _ -> function
            | Yielding_int_like_compare_functions s -> Some (s, ()) | _ -> None);
        case (flag "eq")
          ~box:(fun _ () -> Yielding_bool Eq)
          ~unbox:(fun _ -> function Yielding_bool Eq -> Some () | _ -> None);
        case (flag "ne")
          ~box:(fun _ () -> Yielding_bool Neq)
          ~unbox:(fun _ -> function Yielding_bool Neq -> Some () | _ -> None)
        (* id_case *)
        (*   (constructor_flag ["eq", Yielding_bool Eq; "ne", Yielding_bool Neq]); *)
      ]
  in
  binary "%int_comp" ~params:(param2 standard_int comp) (fun _ (i, c) ->
      P.Int_comp (i, c))

let int_shift =
  D.(
    binary "%int_shift" ~params:(param2 standard_int int_shift_op)
      (fun _ (i, o) -> P.Int_shift (i, o)))

let bfloat_arith =
  D.(
    binary "%bfloat_arith" ~params:(param2 float_bitwidth binary_float_arith_op)
      (fun _ (w, op) -> P.Float_arith (w, op)))

let float_comp =
  let open D in
  let open Flambda_primitive in
  let comp =
    constructor_flag
      [ "lt", Yielding_bool (Lt ());
        "le", Yielding_bool (Le ());
        "gt", Yielding_bool (Gt ());
        "ge", Yielding_bool (Ge ());
        "eq", Yielding_bool Eq;
        "ne", Yielding_bool Neq;
        "lt", Yielding_int_like_compare_functions () ]
  in
  binary "%float_comp" ~params:(param2 float_bitwidth comp) (fun _ (w, c) ->
      P.Float_comp (w, c))

let string_load =
  D.(
    binary "%string_load" ~params:(positional string_accessor_width)
      (fun _ saw -> P.String_or_bigstring_load (String, saw)))

let bytes_load =
  D.(
    binary "%bytes_load" ~params:(positional string_accessor_width)
      (fun _ saw -> P.String_or_bigstring_load (Bytes, saw)))

let bigstring_load =
  D.(
    binary "%bigstring_load" ~params:(positional string_accessor_width)
      (fun _ saw -> P.String_or_bigstring_load (Bigstring, saw)))

let bigarray_get_alignment = todo "%bigarray_get_alignment"

(* Ternaries *)
let array_set =
  D.(
    ternary "%array_set"
      ~params:
        (maps
           (param2 array_kind init_or_assign)
           ~from:(fun _ (k, ia) ->
             let sk : P.Array_set_kind.t =
               match (k : P.Array_kind.t) with
               | Values -> Values ia
               | Immediates -> Immediates
               | Gc_ignorable_values -> Gc_ignorable_values
               | Naked_floats -> Naked_floats
               | Naked_float32s -> Naked_float32s
               | Naked_ints -> Naked_ints
               | Naked_int8s -> Naked_int8s
               | Naked_int16s -> Naked_int16s
               | Naked_int32s -> Naked_int32s
               | Naked_int64s -> Naked_int64s
               | Naked_nativeints -> Naked_nativeints
               | Naked_vec128s -> Naked_vec128s
               | Naked_vec256s -> Naked_vec256s
               | Naked_vec512s -> Naked_vec512s
               | Unboxed_product _ ->
                 Misc.fatal_error "Unboxed product array ops not supported"
             in
             k, sk)
           ~to_:(fun _ (k, sk) : (P.Array_kind.t * _) ->
             let no_ai =
               P.Init_or_assign.Assignment Alloc_mode.For_assignments.heap
             in
             let ai =
               match (sk : P.Array_set_kind.t) with
               | Values ai -> ai
               | Immediates | Gc_ignorable_values | Naked_floats
               | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
               | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
               | Naked_vec256s | Naked_vec512s ->
                 no_ai
             in
             k, ai))
      (fun _ (k, sk) -> P.Array_set (k, sk)))

let atomic_exchange_field =
  D.(
    ternary "%atomic_exchange_field" ~params:block_access_field_kind (fun _ a ->
        P.Atomic_exchange_field a))

let atomic_field_int_arith =
  D.(
    ternary "%atomic_field_int_arith" ~params:int_atomic_op (fun _ o ->
        P.Atomic_field_int_arith o))

let atomic_set_field =
  D.(
    ternary "%atomic_set_field" ~params:block_access_field_kind (fun _ a ->
        P.Atomic_set_field a))

let bigarray_set =
  D.(
    ternary "%bigarray_set"
      ~params:(param3 (positional int) bigarray_kind bigarray_layout)
      (fun _ (d, k, l) -> P.Bigarray_set (d, k, l)))

let bytes_or_bigstring_set =
  D.(
    ternary "%bytes_or_bigstring_set"
      ~params:
        (param2
           (constructor_flag ["bytes", P.Bytes; "bigstring", P.Bigstring])
           (positional string_accessor_width))
      (fun _ (blv, saw) -> P.Bytes_or_bigstring_set (blv, saw)))

(* Quaternaries *)
let atomic_compare_and_set_field =
  D.(
    quaternary "%atomic_compare_and_set_field" ~params:block_access_field_kind
      (fun _ a -> P.Atomic_compare_and_set_field a))

(* Variadics *)
let begin_region =
  D.(
    variadic "%begin_region" ~params:param0 (fun _ () _ ->
        P.Begin_region { ghost = false }))

let begin_try_region =
  D.(
    variadic "%begin_try_region" ~params:param0 (fun _ () _ ->
        P.Begin_try_region { ghost = false }))

let begin_ghost_region =
  D.(
    variadic "%begin_ghost_region" ~params:param0 (fun _ () _ ->
        P.Begin_region { ghost = true }))

let begin_try_ghost_region =
  D.(
    variadic "%begin_try_ghost_region" ~params:param0 (fun _ () _ ->
        P.Begin_try_region { ghost = true }))

let make_block =
  D.(
    variadic "%block"
      ~params:
        (param3 mutability (positional scannable_tag) alloc_mode_for_allocation)
      (fun _ (m, t, a) n ->
        let kind =
          P.Block_kind.Values
            (t, List.init n (fun _ -> Flambda_kind.With_subkind.any_value))
        in
        P.Make_block (kind, m, a)))

let make_array =
  D.(
    variadic "%array"
      ~params:(param3 array_kind mutability alloc_mode_for_allocation)
      (fun _ (k, m, a) _ -> P.Make_array (k, m, a)))

module OfFlambda = struct
  let nullop env (op : P.nullary_primitive) =
    match op with
    | Invalid kind -> invalid env kind
    | Optimised_out kind -> optimised_out env kind
    | Probe_is_enabled { name; enabled_at_init } ->
      probe_is_enabled env (name, enabled_at_init)
    | Enter_inlined_apply { dbg } -> enter_inlined_apply env dbg
    | Domain_index -> domain_index env ()
    | Dls_get -> dls_get env ()
    | Tls_get -> tls_get env ()
    | Poll -> poll env ()
    | Cpu_relax -> cpu_relax env ()

  let unop env (op : P.unary_primitive) =
    match op with
    | Array_length ak -> array_length env ak
    | Block_load { kind; mut; field } -> block_load env (kind, mut, field)
    | Bigarray_length { dimension } -> bigarray_length env dimension
    | Box_number (bk, alloc) -> box_num env (bk, alloc)
    | Boolean_not -> boolean_not env ()
    | End_region { ghost = false } -> end_region env ()
    | End_try_region { ghost = false } -> end_try_region env ()
    | End_region { ghost = true } -> end_ghost_region env ()
    | End_try_region { ghost = true } -> end_try_ghost_region env ()
    | Duplicate_array { kind; source_mutability; destination_mutability } ->
      duplicate_array env (kind, source_mutability, destination_mutability)
    | Float_arith (w, o) -> ufloat_arith env (w, o)
    | Get_tag -> get_tag env ()
    | Is_boxed_float -> is_boxed_float env ()
    | Int_arith (i, o) -> int_uarith env (i, o)
    | Int_as_pointer a -> int_as_pointer env a
    | Is_flat_float_array -> is_flat_float_array env ()
    | Is_int _ -> is_int env () (* CR vlaviron: discuss *)
    | Is_null -> is_null env ()
    | Make_lazy lt -> make_lazy env lt
    | Num_conv { src; dst } -> num_conv env (src, dst)
    | Opaque_identity _ -> opaque_identity env ()
    | Unbox_number bk -> unbox_num env bk
    | Untag_immediate -> untag_immediate env ()
    | Project_value_slot { project_from; value_slot } ->
      project_value_slot env (project_from, value_slot)
    | Project_function_slot { move_from; move_to } ->
      project_function_slot env (move_from, move_to)
    | Reinterpret_64_bit_word r -> reinterpret_64_bit_word env r
    | String_length String -> string_length env ()
    | String_length Bytes -> bytes_length env ()
    | Tag_immediate -> tag_immediate env ()
    | Duplicate_block _ | Obj_dup | Get_header | Peek _ ->
      todo
        (Format.asprintf "%a" P.Without_args.print (P.Without_args.Unary op))
        env ()

  let binop env (op : P.binary_primitive) =
    match op with
    | Atomic_load_field ak -> atomic_load_field env ak
    | Block_set { kind; init; field } -> block_set env (kind, init, field)
    | Array_load (ak, width, mut) -> array_load env (ak, width, mut)
    | Bigarray_load (d, k, l) -> bigarray_load env (d, k, l)
    | Phys_equal Eq -> phys_eq env ()
    | Phys_equal Neq -> phys_ne env ()
    | Int_arith (i, o) -> int_barith env (i, o)
    | Int_comp (i, c) -> int_comp env (i, c)
    | Int_shift (i, s) -> int_shift env (i, s)
    | Float_arith (w, o) -> bfloat_arith env (w, o)
    | Float_comp (w, c) -> float_comp env (w, c)
    | String_or_bigstring_load (String, saw) -> string_load env saw
    | String_or_bigstring_load (Bytes, saw) -> bytes_load env saw
    | String_or_bigstring_load (Bigstring, saw) -> bigstring_load env saw
    | Bigarray_get_alignment align -> bigarray_get_alignment env align
    | Poke _ | Read_offset _ ->
      Misc.fatal_errorf "TODO: Binary primitive: %a" P.Without_args.print
        (P.Without_args.Binary op)

  let ternop env (op : P.ternary_primitive) =
    match op with
    | Array_set (k, sk) -> array_set env (k, sk)
    | Atomic_exchange_field a -> atomic_exchange_field env a
    | Atomic_field_int_arith o -> atomic_field_int_arith env o
    | Atomic_set_field a -> atomic_set_field env a
    | Bytes_or_bigstring_set (blv, saw) -> bytes_or_bigstring_set env (blv, saw)
    | Bigarray_set (d, k, l) -> bigarray_set env (d, k, l)
    | Write_offset _ ->
      todo
        (Format.asprintf "%a" P.Without_args.print (P.Without_args.Ternary op))
        env ()

  let quaternop env (op : P.quaternary_primitive) =
    match op with
    | Atomic_compare_and_set_field a -> atomic_compare_and_set_field env a
    | Atomic_compare_exchange_field _ ->
      todo
        (Format.asprintf "%a" P.Without_args.print
           (P.Without_args.Quaternary op))
        env ()

  let varop env (op : P.variadic_primitive) =
    match op with
    | Begin_region { ghost = false } -> begin_region env ()
    | Begin_try_region { ghost = false } -> begin_try_region env ()
    | Begin_region { ghost = true } -> begin_ghost_region env ()
    | Begin_try_region { ghost = true } -> begin_try_ghost_region env ()
    | Make_block (Values (tag, _), mutability, alloc) ->
      make_block env (mutability, tag, alloc)
    | Make_array (kind, mutability, alloc) ->
      make_array env (kind, mutability, alloc)
    | Make_block ((Naked_floats | Mixed (_, _)), _, _) ->
      todo
        (Format.asprintf "%a" P.Without_args.print (P.Without_args.Variadic op))
        env ()

  let prim env (p : P.t) : t * Simple.t list =
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
  let prim (env : Fexpr_to_flambda_commons.env) (p : t) (args : Simple.t list) :
      P.t =
    match lookup_prim p with
    | None -> Misc.fatal_errorf "Unregistered primitive: %s" p.prim
    | Some conv -> conv env p args
end
