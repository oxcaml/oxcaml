open Fexpr_prim_descr
module L = Describe

let todo = L.todo

(* Common cons *)
let or_unknown cons =
  L.(
    map
      { to_fl =
          (fun _ o ->
            match o with
            | None -> Or_unknown.Unknown
            | Some v -> Or_unknown.known v);
        of_fl =
          (fun _ ouk ->
            match (ouk : _ Or_unknown.t) with
            | Unknown -> None
            | Known v -> Some v)
      }
      (option cons))

let target_ocaml_int : Target_ocaml_int.t value_lens =
  { of_fl = (fun _ t -> Target_ocaml_int.to_int t |> string_of_int |> wrap_loc);
    to_fl =
      (fun _ s ->
        (* CR mshinwell: Should get machine_width from fexpr context when
           available *)
        let mw = Target_system.Machine_width.Sixty_four in
        Target_ocaml_int.of_int mw @@ int_of_string (unwrap_loc s))
  }

let scannable_tag : Tag.Scannable.t value_lens =
  { of_fl = (fun _ t -> Tag.Scannable.to_int t |> string_of_int |> wrap_loc);
    to_fl =
      (fun _ s -> Tag.Scannable.create_exn @@ int_of_string (unwrap_loc s))
  }

let mutability =
  L.(
    default ~def:Mutability.Immutable
    @@ constructor_flag Mutability.["imm_uniq", Immutable_unique; "mut", Mutable])

let standard_int =
  L.(
    default ~def:Flambda_kind.Standard_int.Tagged_immediate
    @@ constructor_flag
         ~no_match_handler:
           Flambda_kind.Standard_int.(
             function
             | (Naked_int8 | Naked_int16) as i ->
               Misc.fatal_errorf "Unsupported %a" print i
             | Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint
             | Tagged_immediate ->
               assert false)
         Flambda_kind.Standard_int.
           [ "imm", Naked_immediate;
             "int32", Naked_int32;
             "int64", Naked_int64;
             "nativeint", Naked_nativeint ])

let standard_int_or_float =
  L.constructor_value
    Flambda_kind.Standard_int_or_float.
      [ "tagged_imm", Tagged_immediate;
        "imm", Naked_immediate;
        "float", Naked_float;
        "int32", Naked_int32;
        "int64", Naked_int64;
        "nativeint", Naked_nativeint ]

let int_shift_op =
  L.(
    constructor_flag
      (["lsl", Lsl; "lsr", Lsr; "asr", Asr]
        : (string * Flambda_primitive.int_shift_op) list))

let unary_int_arith_op =
  L.(
    constructor_flag
      (["bswp", Swap_byte_endianness]
        : (string * Flambda_primitive.unary_int_arith_op) list))

let binary_int_arith_op =
  L.(
    constructor_flag
      ([ "add", Add;
         "sub", Sub;
         "mul", Mul;
         "div", Div;
         "mod", Mod;
         "and", And;
         "or", Or;
         "xor", Xor ]
        : (string * Flambda_primitive.binary_int_arith_op) list))

let binary_float_arith_op =
  L.(
    constructor_flag
      (["add", Add; "sub", Sub; "mul", Mul; "div", Div]
        : (string * Flambda_primitive.binary_float_arith_op) list))

let block_access_kind =
  let value =
    L.(
      param3
        (default ~def:Flambda_primitive.Block_access_field_kind.Any_value
        @@ constructor_flag
             ["imm", Flambda_primitive.Block_access_field_kind.Immediate])
        (or_unknown @@ labeled "tag" scannable_tag)
        (or_unknown @@ labeled "size" target_ocaml_int))
  in
  let naked_float =
    L.(param2 (flag "floaf") (or_unknown @@ labeled "size" target_ocaml_int))
  in
  L.(
    either
      ~no_match_handler:
        Flambda_primitive.Block_access_kind.(
          function
          | Mixed _ as bak -> Misc.fatal_errorf "Unsupported %a" print bak
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
            | Values { field_kind; tag; size } -> Some (field_kind, tag, size)
            | Naked_floats _ | Mixed _ -> None)
          value ])

let string_accessor_width =
  { to_fl =
      (fun _ i : Flambda_primitive.string_accessor_width ->
        let i = unwrap_loc i in
        let m =
          match String.get i (String.length i - 1) with
          | ('a' | 'u') as m -> Some m
          | _ -> None
        in
        match int_of_string i, m with
        | 8, _ -> Eight
        | 16, _ -> Sixteen
        | 32, _ -> Thirty_two
        | 64, _ -> Sixty_four
        | 128, Some 'a' -> One_twenty_eight { aligned = true }
        | 128, Some 'u' -> One_twenty_eight { aligned = false }
        | 256, Some 'a' -> Two_fifty_six { aligned = true }
        | 256, Some 'u' -> Two_fifty_six { aligned = false }
        | 512, Some 'a' -> Five_twelve { aligned = true }
        | 512, Some 'u' -> Five_twelve { aligned = false }
        | _ | (exception _) -> Misc.fatal_error "invalid string accessor width");
    of_fl =
      (fun _ saw ->
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
        wrap_loc s)
  }

let init_or_assign =
  L.(
    default
      ~def:
        (Flambda_primitive.Init_or_assign.Assignment
           Alloc_mode.For_assignments.heap)
    @@ constructor_flag
         Flambda_primitive.Init_or_assign.
           [ "init", Initialization;
             "lassign", Assignment (Alloc_mode.For_assignments.local ()) ])

let alloc_mode_for_allocation =
  L.(
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
  L.constructor_flag
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
  L.(
    default ~def:Flambda_primitive.Array_kind.Values
    @@ constructor_flag
         ~no_match_handler:(fun (k : Flambda_primitive.Array_kind.t) ->
           match k with
           | Immediates | Values | Naked_floats | Gc_ignorable_values ->
             assert false
           | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
           | Naked_int32s | Naked_int64s | Naked_nativeints | Naked_vec128s
           | Naked_vec256s | Naked_vec512s | Unboxed_product _ ->
             Misc.fatal_error
               "fexpr support for arrays of unboxed elements not yet \
                implemented")
         Flambda_primitive.Array_kind.
           [ "imm", Immediates;
             "float", Naked_floats;
             "gc_ign", Gc_ignorable_values ])

let array_kind_for_length =
  L.(
    either
      Flambda_primitive.Array_kind_for_length.
        [ id_case @@ constructor_flag ["generic", Float_array_opt_dynamic];
          case array_kind
            ~box:(fun _ k -> Array_kind k)
            ~unbox:(fun _ -> function
              | Float_array_opt_dynamic -> None | Array_kind k -> Some k) ])

(* Nullaries *)
let invalid =
  L.(
    nullary "%invalid" ~params:(todop "Flambda_kind") (fun _env kind ->
        Flambda_primitive.Invalid kind))

let optimised_out =
  L.(
    nullary "%optimised_out" ~params:(todop "Flambda_kind") (fun _env kind ->
        Flambda_primitive.Optimised_out kind))

let probe_is_enabled =
  L.(
    nullary "%probe_is_enable"
      ~params:(param2 (todop "probe_name") (todop "probe_init"))
      (fun _env (name, enabled_at_init) ->
        Flambda_primitive.Probe_is_enabled { name; enabled_at_init }))

let enter_inlined_apply =
  L.(
    nullary "%inlined_apply" ~params:(todop "dbginfo") (fun _env dbg ->
        Flambda_primitive.Enter_inlined_apply { dbg }))

let dls_get =
  L.(
    nullary "%dls_get" ~params:no_param (fun _env () ->
        Flambda_primitive.Dls_get))

let tls_get =
  L.(
    nullary "%tls_get" ~params:no_param (fun _env () ->
        Flambda_primitive.Tls_get))

let poll =
  L.(nullary "%poll" ~params:no_param (fun _env () -> Flambda_primitive.Poll))

let cpu_relax =
  L.(
    nullary "%cpu_relax" ~params:no_param (fun _env () ->
        Flambda_primitive.Cpu_relax))

(* Unaries *)
let block_load =
  L.(
    unary "%block_load"
      ~params:
        (param3 block_access_kind mutability (positional target_ocaml_int))
      (fun _env (kind, mut, field) ->
        Flambda_primitive.Block_load { kind; mut; field }))

let array_length =
  L.(
    unary "%array_length" ~params:array_kind_for_length (fun _ k ->
        Flambda_primitive.Array_length k))

let box_num =
  L.(
    unary "%box_num" ~params:(param2 boxable_number alloc_mode_for_allocation)
      (fun _ (b, a) -> Flambda_primitive.Box_number (b, a)))

let tag_immediate =
  L.(
    unary "%tag_imm" ~params:no_param (fun _ () ->
        Flambda_primitive.Tag_immediate))

let get_tag =
  L.(unary "%get_tag" ~params:no_param (fun _ () -> Flambda_primitive.Get_tag))

let end_region =
  L.(
    unary "%end_region" ~params:no_param (fun _ () ->
        Flambda_primitive.End_region { ghost = false }))

let end_try_region =
  L.(
    unary "%end_try_region" ~params:no_param (fun _ () ->
        Flambda_primitive.End_try_region { ghost = false }))

let end_ghost_region =
  L.(
    unary "%end_ghost_region" ~params:no_param (fun _ () ->
        Flambda_primitive.End_region { ghost = true }))

let end_try_ghost_region =
  L.(
    unary "%end_try_ghost_region" ~params:no_param (fun _ () ->
        Flambda_primitive.End_try_region { ghost = true }))

let int_uarith =
  L.(
    unary "%int_uarith" ~params:(param2 standard_int unary_int_arith_op)
      (fun _ (i, o) -> Flambda_primitive.Int_arith (i, o)))

let is_flat_float_array =
  L.(
    unary "%is_flat_float_array" ~params:no_param (fun _ () ->
        Flambda_primitive.Is_flat_float_array))

let is_int =
  L.(
    unary "%is_int" ~params:no_param (fun _ () ->
        Flambda_primitive.Is_int { variant_only = true }
        (* CR vlaviron: discuss *)))

let is_null = todo "%is_null"

let num_conv =
  L.(
    unary "%num_conv"
      ~params:
        (param2
           (positional standard_int_or_float)
           (positional standard_int_or_float))
      (fun _ (src, dst) -> Flambda_primitive.Num_conv { src; dst }))

let opaque_identity =
  L.(
    unary "%opaque" ~params:no_param (fun _ () ->
        Flambda_primitive.Opaque_identity
          { middle_end_only = false; kind = Flambda_kind.value }))

let unbox_num =
  L.(
    unary "%unbox_num" ~params:boxable_number (fun _ b ->
        Flambda_primitive.Unbox_number b))

let untag_immediate =
  L.(
    unary "%untag_imm" ~params:no_param (fun _ () ->
        Flambda_primitive.Untag_immediate))

let project_value_slot =
  (* CR mshinwell: support non-value kinds *)
  let kind = Flambda_kind.value in
  L.(
    unary "%project_value_slot"
      ~params:
        (param2
           (map
              { to_fl =
                  (fun env pf ->
                    Fexpr_to_flambda_commons.fresh_or_existing_function_slot env
                      pf);
                of_fl =
                  (fun env pf ->
                    Flambda_to_fexpr_commons.Env.translate_function_slot env pf)
              }
              (positional string))
           (map
              { to_fl =
                  (fun env vs ->
                    Fexpr_to_flambda_commons.fresh_or_existing_value_slot env vs
                      kind);
                of_fl =
                  (fun env vs ->
                    Flambda_to_fexpr_commons.Env.translate_value_slot env vs)
              }
              (positional string)))
      (fun _ (project_from, value_slot) ->
        Flambda_primitive.Project_value_slot { project_from; value_slot }))

let project_function_slot =
  L.(
    unary "%project_function_slot"
      ~params:
        (param2
           (map
              { to_fl =
                  (fun env mf ->
                    Fexpr_to_flambda_commons.fresh_or_existing_function_slot env
                      mf);
                of_fl =
                  (fun env mf ->
                    Flambda_to_fexpr_commons.Env.translate_function_slot env mf)
              }
              (positional string))
           (map
              { to_fl =
                  (fun env mt ->
                    Fexpr_to_flambda_commons.fresh_or_existing_function_slot env
                      mt);
                of_fl =
                  (fun env mt ->
                    Flambda_to_fexpr_commons.Env.translate_function_slot env mt)
              }
              (positional string)))
      (fun _ (move_from, move_to) ->
        Flambda_primitive.Project_function_slot { move_from; move_to }))

let string_length =
  L.(
    unary "%string_length" ~params:no_param (fun _ () ->
        Flambda_primitive.String_length String))

let bytes_length =
  L.(
    unary "%bytes_length" ~params:no_param (fun _ () ->
        Flambda_primitive.String_length String))

let boolean_not =
  L.(
    unary "%boolean_not" ~params:no_param (fun _ () ->
        Flambda_primitive.Boolean_not))

(* Binaries *)
let block_set =
  L.(
    binary "%block_set"
      ~params:
        (param3 block_access_kind init_or_assign (positional target_ocaml_int))
      (fun _ (kind, init, field) ->
        Flambda_primitive.Block_set { kind; init; field }))

let array_load =
  L.(
    binary "%array_load"
      ~params:
        (map
           { to_fl =
               (fun _ (k, m) ->
                 let lk : Flambda_primitive.Array_load_kind.t =
                   match (k : Flambda_primitive.Array_kind.t) with
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
                 k, lk, m);
             of_fl = (fun _ (k, _, m) -> k, m)
           }
           (param2 array_kind mutability))
      (fun _ (k, lk, m) -> Flambda_primitive.Array_load (k, lk, m)))

let phys_eq =
  L.(
    binary "%phys_eq" ~params:no_param (fun _ () ->
        Flambda_primitive.Phys_equal Eq))

let phys_ne =
  L.(
    binary "%phys_ne" ~params:no_param (fun _ () ->
        Flambda_primitive.Phys_equal Neq))

let int_barith =
  L.(
    binary "%int_barith" ~params:(param2 standard_int binary_int_arith_op)
      (fun _ (i, o) -> Flambda_primitive.Int_arith (i, o)))

let int_comp =
  let open L in
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
        id_case
          (constructor_flag ["eq", Yielding_bool Eq; "ne", Yielding_bool Neq]);
        case
          (param2 sign (flag "qmark"))
          ~box:(fun _ (s, ()) -> Yielding_int_like_compare_functions s)
          ~unbox:(fun _ -> function
            | Yielding_int_like_compare_functions s -> Some (s, ()) | _ -> None)
      ]
  in
  binary "%int_comp" ~params:(param2 standard_int comp) (fun _ (i, c) ->
      Flambda_primitive.Int_comp (i, c))

let int_shift =
  L.(
    binary "%int_shift" ~params:(param2 standard_int int_shift_op)
      (fun _ (i, o) -> Flambda_primitive.Int_shift (i, o)))

let float_arith =
  L.(
    binary "%float_arith" ~params:binary_float_arith_op (fun _ op ->
        Flambda_primitive.Float_arith (Float64, op)))

let float_comp = todo "%float_comp"

let string_load =
  L.(
    binary "%string_load" ~params:(positional string_accessor_width)
      (fun _ saw -> Flambda_primitive.String_or_bigstring_load (String, saw)))

let bytes_load =
  L.(
    binary "%bytes_load" ~params:(positional string_accessor_width)
      (fun _ saw -> Flambda_primitive.String_or_bigstring_load (Bytes, saw)))

let bigstring_load =
  L.(
    binary "%bigstring_load" ~params:(positional string_accessor_width)
      (fun _ saw -> Flambda_primitive.String_or_bigstring_load (Bigstring, saw)))

let bigarray_get_alignment = todo "%bigarray_get_alignment"

(* Ternaries *)
let array_set =
  L.(
    ternary "%array_set"
      ~params:
        (map
           { to_fl =
               (fun _ (k, ia) ->
                 let sk : Flambda_primitive.Array_set_kind.t =
                   match (k : Flambda_primitive.Array_kind.t) with
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
                 k, sk);
             of_fl =
               (fun _ (k, sk) : (Flambda_primitive.Array_kind.t * _) ->
                 let no_ai =
                   Flambda_primitive.Init_or_assign.Assignment
                     Alloc_mode.For_assignments.heap
                 in
                 let ai =
                   match (sk : Flambda_primitive.Array_set_kind.t) with
                   | Values ai -> ai
                   | Immediates | Gc_ignorable_values | Naked_floats
                   | Naked_float32s | Naked_ints | Naked_int8s | Naked_int16s
                   | Naked_int32s | Naked_int64s | Naked_nativeints
                   | Naked_vec128s | Naked_vec256s | Naked_vec512s ->
                     no_ai
                 in
                 k, ai)
           }
           (param2 array_kind init_or_assign))
      (fun _ (k, sk) -> Flambda_primitive.Array_set (k, sk)))

let bytes_or_bigstring_set =
  L.(
    ternary "%bytes_or_bigstring_set"
      ~params:
        (param2
           (constructor_flag
              [ "bytes", Flambda_primitive.Bytes;
                "bigstring", Flambda_primitive.Bigstring ])
           (positional string_accessor_width))
      (fun _ (blv, saw) -> Flambda_primitive.Bytes_or_bigstring_set (blv, saw)))

(* Quaternaries *)

(* Variadics *)
let begin_region =
  L.(
    variadic "%begin_region" ~params:no_param (fun _ () _ ->
        Flambda_primitive.Begin_region { ghost = false }))

let begin_try_region =
  L.(
    variadic "%begin_try_region" ~params:no_param (fun _ () _ ->
        Flambda_primitive.Begin_try_region { ghost = false }))

let begin_ghost_region =
  L.(
    variadic "%begin_ghost_region" ~params:no_param (fun _ () _ ->
        Flambda_primitive.Begin_region { ghost = true }))

let begin_try_ghost_region =
  L.(
    variadic "%begin_try_ghost_region" ~params:no_param (fun _ () _ ->
        Flambda_primitive.Begin_try_region { ghost = true }))

let make_block =
  L.(
    variadic "%block"
      ~params:
        (param3 mutability (positional scannable_tag) alloc_mode_for_allocation)
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
    | Probe_is_enabled { name; enabled_at_init } ->
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
    | Int_as_pointer _ | Duplicate_block _ | Duplicate_array _
    | Bigarray_length _ | Float_arith _ | Reinterpret_64_bit_word _
    | Is_boxed_float | Obj_dup | Get_header | Peek _ | Make_lazy _ ->
      todo
        (Format.asprintf "%a" Flambda_primitive.Without_args.print
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
    | Float_arith (Float32, _)
    | Bigarray_load _ | Atomic_load_field _ | Poke _ | Read_offset _ ->
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
        (Format.asprintf "%a" Flambda_primitive.Without_args.print
           (Flambda_primitive.Without_args.Ternary op))
        env ()

  let quaternop env (op : Flambda_primitive.quaternary_primitive) =
    match op with
    | Atomic_compare_and_set_field _ | Atomic_compare_exchange_field _ ->
      todo
        (Format.asprintf "%a" Flambda_primitive.Without_args.print
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
        (Format.asprintf "%a" Flambda_primitive.Without_args.print
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
  let prim (env : Fexpr_to_flambda_commons.env) (p : t) (args : Simple.t list) :
      Flambda_primitive.t =
    match lookup_prim p with
    | None -> Misc.fatal_errorf "Unregistered primitive: %s" p.prim
    | Some conv -> conv env p args
end
