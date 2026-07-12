open Fexpr_prim_descr
module D = Describe
module P = Flambda_primitive
module K = Flambda_kind

(* Common cons *)
let or_unknown cons =
  D.(
    maps (option cons)
      ~from:(fun _ o ->
        match o with None -> Or_unknown.Unknown | Some v -> Or_unknown.known v)
      ~to_:(fun _ ouk ->
        match (ouk : _ Or_unknown.t) with Unknown -> None | Known v -> Some v))

let target_ocaml_int : Target_ocaml_int.t param_cons =
  D.value
    { encode =
        (fun _ t -> Target_ocaml_int.to_int t |> string_of_int |> wrap_loc);
      decode =
        (fun _ s ->
          (* CR mshinwell: Should get machine_width from fexpr context when
             available *)
          let mw = Target_system.Machine_width.Sixty_four in
          Target_ocaml_int.of_int mw @@ int_of_string (unwrap_loc s))
    }

let scannable_tag : Tag.Scannable.t param_cons =
  D.value
    { encode = (fun _ t -> Tag.Scannable.to_int t |> string_of_int |> wrap_loc);
      decode =
        (fun _ s -> Tag.Scannable.create_exn @@ int_of_string (unwrap_loc s))
    }

let mutability =
  D.constructor_flag
    Mutability.
      ["immut", Immutable; "immut_uniq", Immutable_unique; "mut", Mutable]

let opt_mutability = D.default ~def:Mutability.Immutable mutability

let mutable_flag =
  D.(
    default ~def:Asttypes.Immutable @@ constructor_flag Asttypes.["mut", Mutable])

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
  D.constructor_flag
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

let flat_suffix_element =
  D.constructor_flag
    K.
      [ "float", Naked_float;
        "float32", Naked_float32;
        "int8", Naked_int8;
        "int16", Naked_int16;
        "int32", Naked_int32;
        "int64", Naked_int64;
        "nativeint", Naked_nativeint;
        "imm", Naked_immediate;
        "vec128", Naked_vec128;
        "vec256", Naked_vec256;
        "vec512", Naked_vec512 ]

let mixed_block_shape =
  let open D in
  maps
    ~from:(fun _ (size, suffix) ->
      K.Mixed_block_shape.from_prefix_size_and_suffix_elements size suffix)
    ~to_:(fun _ shape ->
      K.Mixed_block_shape.(
        value_prefix_size shape, Array.to_list (flat_suffix shape)))
    (param2 int (list flat_suffix_element))

let block_shape =
  let open D in
  patterns
  @@
  let| mixed =
    ( mixed_block_shape,
      fun _ m ->
        K.(Block_shape.Scannable (Scannable_block_shape.Mixed_record m)) )
  in
  let| float_record = flag_case "float_record" K.Block_shape.Float_record in
  let| values =
    flag_case "values"
      K.(Block_shape.Scannable Scannable_block_shape.Value_only)
  in
  return_either (function
    | K.Block_shape.Float_record -> float_record ()
    | K.Block_shape.Scannable K.Scannable_block_shape.Value_only -> values ()
    | K.Block_shape.Scannable (K.Scannable_block_shape.Mixed_record m) ->
      mixed m)

let block_access_kind =
  let open D in
  let tag = or_unknown @@ labeled "tag" scannable_tag in
  let size = or_unknown @@ labeled "size" target_ocaml_int in
  let|= mixed_field_kind =
    let open P.Mixed_block_access_field_kind in
    let| value_k = block_access_field_kind, fun _ bak -> Value_prefix bak in
    let| suffix_k = flat_suffix_element, fun _ fse -> Flat_suffix fse in
    return_either (function
      | Value_prefix bak -> value_k bak
      | Flat_suffix fse -> suffix_k fse)
  in
  patterns
  @@
  let| naked_float =
    ( param2 (flag "float") size,
      fun _ ((), size) -> P.Block_access_kind.Naked_floats { size } )
  in
  let| mixed =
    param5_case
      ~decode:(fun _ () tag size field_kind shape ->
        P.Block_access_kind.Mixed { tag; size; field_kind; shape })
      (flag "mixed") tag size mixed_field_kind mixed_block_shape
  in
  let| value_k =
    param3_case block_access_field_kind tag size
      ~decode:(fun _ field_kind tag size ->
        P.Block_access_kind.Values { field_kind; tag; size })
  in
  P.Block_access_kind.(
    return_either (function
      | Values { field_kind; tag; size } -> value_k (field_kind, tag, size)
      | Naked_floats { size } -> naked_float ((), size)
      | Mixed m -> mixed ((), m.tag, m.size, m.field_kind, m.shape)))

type block_kind =
  | FNaked_floats
  | FValues of Tag.Scannable.t
  | FMixed of Tag.Scannable.t * K.Mixed_block_shape.t

let block_kind : block_kind param_cons =
  let open D in
  patterns
  @@
  let| floats = flag "floats", fun _ () -> FNaked_floats in
  let| mixed =
    param3_case (flag "mixed") scannable_tag mixed_block_shape
      ~decode:(fun _ () tag shape -> FMixed (tag, shape))
  in
  let| values = positional scannable_tag, fun _ tag -> FValues tag in
  return_either (function
    | FNaked_floats -> floats ()
    | FValues tag -> values tag
    | FMixed (tag, shape) -> mixed ((), tag, shape))

let string_accessor_width =
  D.value
    { decode =
        (fun _ i : P.string_accessor_width ->
          let i = unwrap_loc i in
          match i with
          | "f32" -> Single
          | "8" -> Eight
          | "8s" -> Eight_signed
          | "16" -> Sixteen
          | "16s" -> Sixteen_signed
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
            | Eight_signed -> "8s"
            | Sixteen -> "16"
            | Sixteen_signed -> "16s"
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
  let open D in
  patterns
  @@
  let| local =
    ( labeled "local" string,
      fun env r ->
        let region =
          if String.equal (unwrap_loc r) "toplevel"
          then env.toplevel_region
          else Fexpr_to_flambda_commons.find_var env r
        in
        Alloc_mode.For_allocations.local ~region )
  in
  let| heap = param0, fun _ () -> Alloc_mode.For_allocations.heap in
  return_either (fun alloc env ->
      match alloc with
      | Alloc_mode.For_allocations.Local { region } ->
        let r =
          match Flambda_to_fexpr_commons.Env.find_region_exn env region with
          | Fexpr.Toplevel -> wrap_loc "toplevel"
          | Named s -> s
        in
        local r env
      | Alloc_mode.For_allocations.Heap -> heap () env)

let alloc_mode_for_assignments =
  let open D in
  patterns
  @@
  let| local =
    flag "local", fun _env () -> Alloc_mode.For_assignments.local ()
  in
  let| heap = param0, fun _ () -> Alloc_mode.For_assignments.heap in
  return_either (function
    | Alloc_mode.For_assignments.Local -> local ()
    | Alloc_mode.For_assignments.Heap -> heap ())

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
  let open D in
  let open P.Array_kind in
  let ak =
    recursive_patterns (fun ak ->
        let| imm = flag_case "imm" Immediates in
        let| values = flag_case "values" Values in
        let| float = flag_case "float" Naked_floats in
        let| float32 = flag_case "float32" Naked_float32s in
        let| int = flag_case "int" Naked_ints in
        let| int8 = flag_case "int8" Naked_int8s in
        let| int16 = flag_case "int16" Naked_int16s in
        let| int32 = flag_case "int32" Naked_int32s in
        let| int64 = flag_case "int64" Naked_int64s in
        let| nativeint = flag_case "nativeint" Naked_nativeints in
        let| vec128 = flag_case "vec128" Naked_vec128s in
        let| vec256 = flag_case "vec256" Naked_vec256s in
        let| vec512 = flag_case "vec512" Naked_vec512s in
        let| gc_ign = flag_case "gc_ign" Gc_ignorable_values in
        let| product = list ak, fun _ aks -> Unboxed_product aks in
        return_either (function
          | Immediates -> imm ()
          | Values -> values ()
          | Naked_floats -> float ()
          | Naked_float32s -> float32 ()
          | Naked_ints -> int ()
          | Naked_int8s -> int8 ()
          | Naked_int16s -> int16 ()
          | Naked_int32s -> int32 ()
          | Naked_int64s -> int64 ()
          | Naked_nativeints -> nativeint ()
          | Naked_vec128s -> vec128 ()
          | Naked_vec256s -> vec256 ()
          | Naked_vec512s -> vec512 ()
          | Gc_ignorable_values -> gc_ign ()
          | Unboxed_product aks -> product aks))
  in
  default ~def:Values ak

let array_kind_for_length =
  let open D in
  let open P.Array_kind_for_length in
  patterns
  @@
  let| generic = flag "generic", fun _ () -> Float_array_opt_dynamic in
  let| arrayk = array_kind, fun _ k -> Array_kind k in
  return_either (function
    | Float_array_opt_dynamic -> generic ()
    | Array_kind k -> arrayk k)

let lazy_tag =
  D.(
    default ~def:Lambda.Lazy_tag
    @@ constructor_flag ["forward", Lambda.Forward_tag])

let duplicate_array_kind =
  let open D in
  let open P.Duplicate_array_kind in
  default ~def:Values @@ patterns
  @@
  let| imm = flag_case "imm" Immediates in
  let| values = flag_case "values" Values in
  let| float =
    ( labeled "float" (option target_ocaml_int),
      fun _ length -> Naked_floats { length } )
  in
  let| float32 =
    ( labeled "float32" (option target_ocaml_int),
      fun _ length -> Naked_float32s { length } )
  in
  let| int =
    ( labeled "int" (option target_ocaml_int),
      fun _ length -> Naked_ints { length } )
  in
  let| int8 =
    ( labeled "int8" (option target_ocaml_int),
      fun _ length -> Naked_int8s { length } )
  in
  let| int16 =
    ( labeled "int16" (option target_ocaml_int),
      fun _ length -> Naked_int16s { length } )
  in
  let| int32 =
    ( labeled "int32" (option target_ocaml_int),
      fun _ length -> Naked_int32s { length } )
  in
  let| int64 =
    ( labeled "int64" (option target_ocaml_int),
      fun _ length -> Naked_int64s { length } )
  in
  let| nativeint =
    ( labeled "nativeint" (option target_ocaml_int),
      fun _ length -> Naked_nativeints { length } )
  in
  let| vec128 =
    ( labeled "vec128" (option target_ocaml_int),
      fun _ length -> Naked_vec128s { length } )
  in
  let| vec256 =
    ( labeled "vec256" (option target_ocaml_int),
      fun _ length -> Naked_vec256s { length } )
  in
  let| vec512 =
    ( labeled "vec512" (option target_ocaml_int),
      fun _ length -> Naked_vec512s { length } )
  in
  return_either (function
    | Immediates -> imm ()
    | Values -> values ()
    | Naked_floats { length } -> float length
    | Naked_float32s { length } -> float32 length
    | Naked_ints { length } -> int length
    | Naked_int8s { length } -> int8 length
    | Naked_int16s { length } -> int16 length
    | Naked_int32s { length } -> int32 length
    | Naked_int64s { length } -> int64 length
    | Naked_nativeints { length } -> nativeint length
    | Naked_vec128s { length } -> vec128 length
    | Naked_vec256s { length } -> vec256 length
    | Naked_vec512s { length } -> vec512 length)

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

let kind =
  D.(
    default ~def:K.value
    @@ constructor_flag
         [ "value", K.value;
           "imm", K.naked_immediate;
           "float32", K.naked_float32;
           "float", K.naked_float;
           "int8", K.naked_int8;
           "int16", K.naked_int16;
           "int32", K.naked_int8;
           "int64", K.naked_int16;
           "nativeint", K.naked_nativeint;
           "vec128", K.naked_vec128;
           "vec256", K.naked_vec256;
           "vec512", K.naked_vec512;
           "region", K.region;
           "rec_info", K.rec_info ])

let kind_with_subkind =
  let open D in
  let nullable =
    let open K.With_subkind.Nullable in
    maps (bool_flag "or_null")
      ~from:(fun _ b -> if b then Nullable else Non_nullable)
      ~to_:(fun _ -> function Nullable -> true | Non_nullable -> false)
  in
  recursive_patterns (fun full_kind ->
      let|= non_null_value_subkind =
        let open K.With_subkind.Non_null_value_subkind in
        let| anything = flag_case "value" Anything in
        let| tagged_imm = flag_case "tagged_imm" Tagged_immediate in
        let| boxed_float = flag_case "boxed_float" Boxed_float in
        let| boxed_float32 = flag_case "boxed_float32" Boxed_float32 in
        let| boxed_int32 = flag_case "boxed_int32" Boxed_int32 in
        let| boxed_int64 = flag_case "boxed_int64" Boxed_int64 in
        let| boxed_nativeint = flag_case "boxed_nativeint" Boxed_nativeint in
        let| boxed_vec128 = flag_case "boxed_vec128" Boxed_vec128 in
        let| boxed_vec256 = flag_case "boxed_vec256" Boxed_vec256 in
        let| boxed_vec512 = flag_case "boxed_vec512" Boxed_vec512 in
        let| value_array = flag_case "value_array" Value_array in
        let| imm_array = flag_case "imm_array" Immediate_array in
        let| float_array = flag_case "float_array" Float_array in
        let| generic_array = flag_case "generic_array" Generic_array in
        let| untagged_int_array =
          flag_case "untagged_int_array" Untagged_int_array
        in
        let| untagged_int8_array =
          flag_case "untagged_int8_array" Untagged_int8_array
        in
        let| untagged_int16_array =
          flag_case "untagged_int16_array" Untagged_int16_array
        in
        let| unboxed_int32_array =
          flag_case "unboxed_int32_array" Unboxed_int32_array
        in
        let| unboxed_int64_array =
          flag_case "unboxed_int64_array" Unboxed_int64_array
        in
        let| unboxed_nativeint_array =
          flag_case "unboxed_nativeint_array" Unboxed_nativeint_array
        in
        let| unboxed_float32_array =
          flag_case "unboxed_float32_array" Unboxed_float32_array
        in
        let| unboxed_vec128_array =
          flag_case "unboxed_vec128_array" Unboxed_vec128_array
        in
        let| unboxed_vec256_array =
          flag_case "unboxed_vec256_array" Unboxed_vec256_array
        in
        let| unboxed_vec512_array =
          flag_case "unboxed_vec512_array" Unboxed_vec512_array
        in
        let| unboxed_product_array =
          flag_case "unboxed_product_array" Unboxed_product_array
        in
        let| float_block =
          ( labeled "float_block" int,
            fun _ num_fields -> Float_block { num_fields } )
        in
        let| variant =
          let item = param2 block_shape (list full_kind) in
          let map_bind = positional (param2 scannable_tag item) in
          let tag_map =
            maps (list map_bind)
              ~from:(fun _ l -> Tag.Scannable.Map.of_list l)
              ~to_:(fun _ m -> Tag.Scannable.Map.bindings m)
          in
          let target_int_set =
            maps (list target_ocaml_int)
              ~from:(fun _ l -> Target_ocaml_int.Set.of_list l)
              ~to_:(fun _ m -> Target_ocaml_int.Set.elements m)
          in
          param2_case target_int_set tag_map ~decode:(fun _ consts non_consts ->
              Variant { consts; non_consts })
        in
        return_either (function
          | Anything -> anything ()
          | Boxed_float32 -> boxed_float32 ()
          | Boxed_float -> boxed_float ()
          | Boxed_int32 -> boxed_int32 ()
          | Boxed_int64 -> boxed_int64 ()
          | Boxed_nativeint -> boxed_nativeint ()
          | Boxed_vec128 -> boxed_vec128 ()
          | Boxed_vec256 -> boxed_vec256 ()
          | Boxed_vec512 -> boxed_vec512 ()
          | Tagged_immediate -> tagged_imm ()
          | Float_array -> float_array ()
          | Immediate_array -> imm_array ()
          | Value_array -> value_array ()
          | Generic_array -> generic_array ()
          | Unboxed_float32_array -> unboxed_float32_array ()
          | Untagged_int_array -> untagged_int_array ()
          | Untagged_int8_array -> untagged_int8_array ()
          | Untagged_int16_array -> untagged_int16_array ()
          | Unboxed_int32_array -> unboxed_int32_array ()
          | Unboxed_int64_array -> unboxed_int64_array ()
          | Unboxed_nativeint_array -> unboxed_nativeint_array ()
          | Unboxed_vec128_array -> unboxed_vec128_array ()
          | Unboxed_vec256_array -> unboxed_vec256_array ()
          | Unboxed_vec512_array -> unboxed_vec512_array ()
          | Unboxed_product_array -> unboxed_product_array ()
          | Float_block { num_fields } -> float_block num_fields
          | Variant { consts; non_consts } -> variant (consts, non_consts))
      in
      let| region = flag_case "region" K.With_subkind.region in
      let| rec_info = flag_case "rec_info" K.With_subkind.rec_info in
      let| naked_immediate = flag_case "imm" K.With_subkind.naked_immediate in
      let| naked_float = flag_case "float" K.With_subkind.naked_float in
      let| naked_float32 = flag_case "float32" K.With_subkind.naked_float32 in
      let| naked_int8 = flag_case "int8" K.With_subkind.naked_int8 in
      let| naked_int16 = flag_case "int16" K.With_subkind.naked_int16 in
      let| naked_int32 = flag_case "int32" K.With_subkind.naked_int32 in
      let| naked_int64 = flag_case "int64" K.With_subkind.naked_int64 in
      let| naked_nativeint =
        flag_case "nativeint" K.With_subkind.naked_nativeint
      in
      let| naked_vec128 = flag_case "vec128" K.With_subkind.naked_vec128 in
      let| naked_vec256 = flag_case "vec256" K.With_subkind.naked_vec256 in
      let| naked_vec512 = flag_case "vec512" K.With_subkind.naked_vec512 in
      let| value =
        param2_case non_null_value_subkind nullable ~decode:(fun _ sk n ->
            K.With_subkind.create K.value sk n)
      in
      return_either (fun full_kind ->
          match K.With_subkind.kind full_kind with
          | Region -> region ()
          | Rec_info -> rec_info ()
          | Naked_number K.Naked_number_kind.Naked_immediate ->
            naked_immediate ()
          | Naked_number K.Naked_number_kind.Naked_float32 -> naked_float32 ()
          | Naked_number K.Naked_number_kind.Naked_float -> naked_float ()
          | Naked_number K.Naked_number_kind.Naked_int8 -> naked_int8 ()
          | Naked_number K.Naked_number_kind.Naked_int16 -> naked_int16 ()
          | Naked_number K.Naked_number_kind.Naked_int32 -> naked_int32 ()
          | Naked_number K.Naked_number_kind.Naked_int64 -> naked_int64 ()
          | Naked_number K.Naked_number_kind.Naked_nativeint ->
            naked_nativeint ()
          | Naked_number K.Naked_number_kind.Naked_vec128 -> naked_vec128 ()
          | Naked_number K.Naked_number_kind.Naked_vec256 -> naked_vec256 ()
          | Naked_number K.Naked_number_kind.Naked_vec512 -> naked_vec512 ()
          | Value ->
            value
              ( K.With_subkind.non_null_value_subkind full_kind,
                K.With_subkind.nullable full_kind )))

(* Nullaries *)
let invalid =
  D.(nullary "%invalid" ~params:kind (fun _env kind -> P.Invalid kind))

let optimised_out =
  D.(
    nullary "%optimised_out" ~params:kind (fun _env kind ->
        P.Optimised_out kind))

let probe_is_enabled =
  D.(
    nullary "%probe_is_enabled"
      ~params:
        (param2 (labeled "name" string)
           (option (labeled "enabled_at_init" bool)))
      (fun _env (name, enabled_at_init) ->
        let name = unwrap_loc name in
        P.Probe_is_enabled { name; enabled_at_init }))

let enter_inlined_apply =
  D.(
    nullary "%inlined_apply" ~params:param0 (fun _env () ->
        P.Enter_inlined_apply { dbg = Inlined_debuginfo.none }))

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
        (param3 block_access_kind opt_mutability (positional target_ocaml_int))
      (fun _env (kind, mut, field) -> P.Block_load { kind; mut; field }))

let bigarray_length =
  D.(
    unary "%bigarray_length" ~params:(positional int) (fun _ dimension ->
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

let obj_dup = D.(unary "%obj_dup" ~params:param0 (fun _ () -> P.Obj_dup))

let get_header =
  D.(unary "%get_header" ~params:param0 (fun _ () -> P.Get_header))

let reinterpret_boxed_vector =
  D.(
    unary "%reinterpret_boxed_vector" ~params:param0 (fun _ () ->
        P.Reinterpret_boxed_vector))

let peek =
  D.(
    unary "%peek" ~params:(positional standard_int_or_float)
      (fun _ standard_int_or_float -> P.Peek standard_int_or_float))

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
        (param3 duplicate_array_kind (positional mutability)
           (positional mutability))
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
  (* CR mshinwell: support non-value kinds in the projection syntax. Note that
     if the value slot's definition (in a "with" clause, where kinds are
     supported) has already been parsed, the slot registered under this name
     will have the correct kind and the kind here is ignored. *)
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

let duplicate_block =
  let open D in
  let|= kind =
    let open P.Duplicate_block_kind in
    let| floats =
      labeled "floats" target_ocaml_int, fun _ length -> Naked_floats { length }
    in
    let| mixed = flag_case "mixed" Mixed in
    let| values =
      param2_case scannable_tag target_ocaml_int ~decode:(fun _ tag length ->
          Values { tag; length })
    in
    return_either (function
      | Values { tag; length } -> values (tag, length)
      | Naked_floats { length } -> floats length
      | Mixed -> mixed ())
  in
  unary "%duplicate_block" ~params:kind (fun _ kind ->
      P.Duplicate_block { kind })

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
  let open D in
  let load_kind =
    let open P.Array_load_kind in
    constructor_flag
      [ "imm", Immediates;
        "gc_ign", Gc_ignorable_values;
        "value", Values;
        "float", Naked_floats;
        "float32", Naked_float32s;
        "int", Naked_ints;
        "int8", Naked_int8s;
        "int16", Naked_int16s;
        "int32", Naked_int32s;
        "int64", Naked_int64s;
        "nativeint", Naked_nativeints;
        "vec128", Naked_vec128s;
        "vec256", Naked_vec256s;
        "vec512", Naked_vec512s ]
  in
  binary "%array_load"
    ~params:
      (maps
         (param3 array_kind (option load_kind) opt_mutability)
         ~from:(fun _ (k, lk, m) ->
           let lk : P.Array_load_kind.t =
             match lk with
             | Some lk -> lk
             | None -> (
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
                 Misc.fatal_error "missing product array load kind")
           in
           k, lk, m)
         ~to_:(fun _ (k, lk, m) ->
           let lk =
             match (k : P.Array_kind.t) with
             | Unboxed_product _ -> Some lk
             | Immediates | Gc_ignorable_values | Values | Naked_floats
             | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
             | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
             | Naked_vec256s | Naked_vec512s ->
               None
           in
           k, lk, m))
    (fun _ (k, lk, m) -> P.Array_load (k, lk, m))

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
  let|= comp =
    let| lt =
      param2_case sign (flag "lt") ~decode:(fun _ s () -> Yielding_bool (Lt s))
    in
    let| le =
      param2_case sign (flag "le") ~decode:(fun _ s () -> Yielding_bool (Le s))
    in
    let| gt =
      param2_case sign (flag "gt") ~decode:(fun _ s () -> Yielding_bool (Gt s))
    in
    let| ge =
      param2_case sign (flag "ge") ~decode:(fun _ s () -> Yielding_bool (Ge s))
    in
    let| qmark =
      param2_case sign (flag "qmark") ~decode:(fun _ s () ->
          Yielding_int_like_compare_functions s)
    in
    let| eq = flag "eq", fun _ () -> Yielding_bool Eq in
    let| neq = flag "ne", fun _ () -> Yielding_bool Neq in
    return_either (function
      | Yielding_bool (Lt s) -> lt (s, ())
      | Yielding_bool (Le s) -> le (s, ())
      | Yielding_bool (Gt s) -> gt (s, ())
      | Yielding_bool (Ge s) -> ge (s, ())
      | Yielding_bool Eq -> eq ()
      | Yielding_bool Neq -> neq ()
      | Yielding_int_like_compare_functions s -> qmark (s, ()))
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

let bigarray_get_alignment =
  D.(
    binary "%bigarray_get_alignment" ~params:(positional int) (fun _ align ->
        P.Bigarray_get_alignment align))

let read_offset =
  D.(
    binary "%read_offset"
      ~params:(param2 (labeled "kind" kind_with_subkind) mutable_flag)
      (fun _ (kind, mutable_flag) -> P.Read_offset (kind, mutable_flag)))

let poke =
  D.(
    binary "%poke" ~params:(positional standard_int_or_float)
      (fun _ standard_int_or_float -> P.Poke standard_int_or_float))

(* Ternaries *)
let array_set =
  let open D in
  let|= set_kind =
    let open P.Array_set_kind in
    let| imm = flag_case "imm" Immediates in
    let| values =
      param2_case (flag "value") init_or_assign ~decode:(fun _ () ioa ->
          Values ioa)
    in
    let| float = flag_case "float" Naked_floats in
    let| float32 = flag_case "float32" Naked_float32s in
    let| int = flag_case "int" Naked_ints in
    let| int8 = flag_case "int8" Naked_int8s in
    let| int16 = flag_case "int16" Naked_int16s in
    let| int32 = flag_case "int32" Naked_int32s in
    let| int64 = flag_case "int64" Naked_int64s in
    let| nativeint = flag_case "nativeint" Naked_nativeints in
    let| vec128 = flag_case "vec128" Naked_vec128s in
    let| vec256 = flag_case "vec256" Naked_vec256s in
    let| vec512 = flag_case "vec512" Naked_vec512s in
    let| gc_ign = flag_case "gc_ign" Gc_ignorable_values in
    return_either (function
      | Immediates -> imm ()
      | Values ioa -> values ((), ioa)
      | Naked_floats -> float ()
      | Naked_float32s -> float32 ()
      | Naked_ints -> int ()
      | Naked_int8s -> int8 ()
      | Naked_int16s -> int16 ()
      | Naked_int32s -> int32 ()
      | Naked_int64s -> int64 ()
      | Naked_nativeints -> nativeint ()
      | Naked_vec128s -> vec128 ()
      | Naked_vec256s -> vec256 ()
      | Naked_vec512s -> vec512 ()
      | Gc_ignorable_values -> gc_ign ())
  in
  ternary "%array_set"
    ~params:
      (maps
         (param2 array_kind (option set_kind))
         ~from:(fun _ (k, sk) ->
           let sk : P.Array_set_kind.t =
             match sk with
             | Some sk -> sk
             | None -> (
               match (k : P.Array_kind.t) with
               | Values ->
                 Values
                   (P.Init_or_assign.Assignment Alloc_mode.For_assignments.heap)
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
                 Misc.fatal_error "Missing product array set kind")
           in
           k, sk)
         ~to_:(fun _ (k, sk) ->
           let sk =
             match (k : P.Array_kind.t) with
             | Unboxed_product _ -> Some sk
             | Immediates | Gc_ignorable_values | Values | Naked_floats
             | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
             | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
             | Naked_vec256s | Naked_vec512s ->
               None
           in
           k, sk))
    (fun _ (k, sk) -> P.Array_set (k, sk))

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

let write_offset =
  D.(
    ternary "%write_offset"
      ~params:
        (param3
           (constructor_flag
              [ "into_block", P.Write_offset_kind.Into_block;
                ( "into_block_or_off_heap",
                  P.Write_offset_kind.Into_block_or_off_heap ) ])
           (labeled "kind" kind_with_subkind)
           alloc_mode_for_assignments)
      (fun _ (wok, kind, alloc_mode) -> P.Write_offset (wok, kind, alloc_mode)))

(* Quaternaries *)
let atomic_compare_and_set_field =
  D.(
    quaternary "%atomic_compare_and_set_field" ~params:block_access_field_kind
      (fun _ a -> P.Atomic_compare_and_set_field a))

let atomic_compare_exchange_field =
  D.(
    quaternary "%atomic_compare_exchange_field"
      ~params:(param2 block_access_field_kind block_access_field_kind)
      (fun _ (atomic_kind, args_kind) ->
        P.Atomic_compare_exchange_field { atomic_kind; args_kind }))

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
      ~params:(param3 opt_mutability block_kind alloc_mode_for_allocation)
      (fun _ (m, k, a) n ->
        let kind =
          match k with
          | FValues t ->
            P.Block_kind.Values
              (t, List.init n (fun _ -> Flambda_kind.With_subkind.any_value))
          | FNaked_floats -> P.Block_kind.Naked_floats
          | FMixed (tag, shape) -> P.Block_kind.Mixed (tag, shape)
        in
        P.Make_block (kind, m, a)))

let make_array =
  D.(
    variadic "%array"
      ~params:(param3 array_kind opt_mutability alloc_mode_for_allocation)
      (fun _ (k, m, a) _ -> P.Make_array (k, m, a)))

module OfFlambda = struct
  let nullop env (op : P.nullary_primitive) =
    match op with
    | Invalid kind -> invalid env kind
    | Optimised_out kind -> optimised_out env kind
    | Probe_is_enabled { name; enabled_at_init } ->
      probe_is_enabled env (wrap_loc name, enabled_at_init)
    | Enter_inlined_apply { dbg = _ } -> enter_inlined_apply env ()
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
    | Obj_dup -> obj_dup env ()
    | Get_header -> get_header env ()
    | Reinterpret_boxed_vector -> reinterpret_boxed_vector env ()
    | Peek standard_int_or_float -> peek env standard_int_or_float
    | Duplicate_block { kind } -> duplicate_block env kind

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
    | Read_offset (kind, mutable_flag) -> read_offset env (kind, mutable_flag)
    | Poke standard_int_or_float -> poke env standard_int_or_float

  let ternop env (op : P.ternary_primitive) =
    match op with
    | Array_set (k, sk) -> array_set env (k, sk)
    | Atomic_exchange_field a -> atomic_exchange_field env a
    | Atomic_field_int_arith o -> atomic_field_int_arith env o
    | Atomic_set_field a -> atomic_set_field env a
    | Bytes_or_bigstring_set (blv, saw) -> bytes_or_bigstring_set env (blv, saw)
    | Bigarray_set (d, k, l) -> bigarray_set env (d, k, l)
    | Write_offset (wok, kind, alloc_mode) ->
      write_offset env (wok, kind, alloc_mode)

  let quaternop env (op : P.quaternary_primitive) =
    match op with
    | Atomic_compare_and_set_field a -> atomic_compare_and_set_field env a
    | Atomic_compare_exchange_field { atomic_kind; args_kind } ->
      atomic_compare_exchange_field env (atomic_kind, args_kind)

  let varop env (op : P.variadic_primitive) =
    match op with
    | Begin_region { ghost = false } -> begin_region env ()
    | Begin_try_region { ghost = false } -> begin_try_region env ()
    | Begin_region { ghost = true } -> begin_ghost_region env ()
    | Begin_try_region { ghost = true } -> begin_try_ghost_region env ()
    | Make_block (Values (tag, _), mutability, alloc) ->
      make_block env (mutability, FValues tag, alloc)
    | Make_block (Naked_floats, mutability, alloc) ->
      make_block env (mutability, FNaked_floats, alloc)
    | Make_block (Mixed (tag, shape), mutability, alloc) ->
      make_block env (mutability, FMixed (tag, shape), alloc)
    | Make_array (kind, mutability, alloc) ->
      make_array env (kind, mutability, alloc)

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
