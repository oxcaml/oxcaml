(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module L = Lambda
module H = Fdo_prehash

module Body_hash = struct
  (* Two bodies should hash alike unless their differences are likely to change
     code layout decisions.

     As such, the following are covered:

     - control-flow constructs

     - constants

     - primitives (with selected payloads)

     - call sites with their callees and inlining attributes

     - function attributes

     And the following are not:

     - what varies between compilations of the same code (source positions,
     identifier stamps, etc.)

     - layouts and modes of terms.

     Every node is hashed from a tag of its own, its fields and its children, so
     that distinct trees get distinct hashes.

     CR yusumez: The translation to Lambda and the names of the primitives
     change between compiler versions, so a body hash from one compiler version
     does not in general match the one from another. *)

  (* Identifiers by name. Stamps are unstable since they depend on everything
     compiled before, and we need the name to tell callees apart. This means
     renaming variables changes the hash. *)
  let ident id = H.of_string (Ident.name id)

  let constant (c : L.constant) =
    match c with
    | Const_int n -> H.node ~tag:1 [H.of_int n]
    | Const_char c -> H.node ~tag:2 [H.of_int (Char.code c)]
    | Const_untagged_char n -> H.node ~tag:3 [H.of_int n]
    | Const_string (s, _loc, _delim) -> H.node ~tag:4 [H.of_string s]
    | Const_float s -> H.node ~tag:5 [H.of_string s]
    | Const_float32 s -> H.node ~tag:6 [H.of_string s]
    | Const_unboxed_float s -> H.node ~tag:7 [H.of_string s]
    | Const_unboxed_float32 s -> H.node ~tag:8 [H.of_string s]
    | Const_int8 n -> H.node ~tag:9 [H.of_int n]
    | Const_int16 n -> H.node ~tag:10 [H.of_int n]
    | Const_int32 n -> H.node ~tag:11 [H.of_int (Int32.to_int n)]
    | Const_int64 n -> H.node ~tag:12 [H.of_int64 n]
    | Const_nativeint n -> H.node ~tag:13 [H.of_int64 (Int64.of_nativeint n)]
    | Const_untagged_int n -> H.node ~tag:14 [H.of_int n]
    | Const_untagged_int8 n -> H.node ~tag:15 [H.of_int n]
    | Const_untagged_int16 n -> H.node ~tag:16 [H.of_int n]
    | Const_unboxed_int32 n -> H.node ~tag:17 [H.of_int (Int32.to_int n)]
    | Const_unboxed_int64 n -> H.node ~tag:18 [H.of_int64 n]
    | Const_unboxed_nativeint n ->
      H.node ~tag:19 [H.of_int64 (Int64.of_nativeint n)]

  let rec structured_constant (c : L.structured_constant) =
    match c with
    | Const_base c -> H.node ~tag:1 [constant c]
    | Const_block (tag, fields) ->
      H.node ~tag:2
        [H.of_int tag; H.of_list (List.map structured_constant fields)]
    | Const_mixed_block (tag, _shape, fields) ->
      H.node ~tag:3
        [H.of_int tag; H.of_list (List.map structured_constant fields)]
    | Const_float_array fields ->
      H.node ~tag:4 [H.of_list (List.map H.of_string fields)]
    | Const_immstring s -> H.node ~tag:5 [H.of_string s]
    | Const_float_block fields ->
      H.node ~tag:6 [H.of_list (List.map H.of_string fields)]
    | Const_null -> H.node ~tag:7 []

  let bool b = H.of_int (Bool.to_int b)

  let peek_or_poke (access : L.peek_or_poke) =
    H.of_int
      (match access with
      | Ppp_tagged_immediate -> 0
      | Ppp_untagged_immediate -> 1
      | Ppp_unboxed_float32 -> 2
      | Ppp_unboxed_float -> 3
      | Ppp_untagged_int8 -> 4
      | Ppp_untagged_int16 -> 5
      | Ppp_unboxed_int32 -> 6
      | Ppp_unboxed_int64 -> 7
      | Ppp_unboxed_nativeint -> 8)

  (* A primitive is hashed from its constructor and the payloads that select
     what it computes: field indices and paths, block tags, globals, C
     functions, scalar operations, bounds checks and sizes of raw accesses.
     Modes, layouts, kinds, shapes, mutability and the representation of indices
     and results are left out. *)
  let primitive (prim : L.primitive) =
    let int = H.of_int in
    let path p = H.of_list (List.map H.of_int p) in
    let vector (size : L.boxed_vector) =
      int
        (match size with
        | Boxed_vec128 -> 128
        | Boxed_vec256 -> 256
        | Boxed_vec512 -> 512)
    in
    let checks checks =
      H.of_option
        (Option.map
           (fun (~len, ~align) -> H.combine (int len) (int align))
           checks)
    in
    let payload =
      match prim with
      | Pbytes_to_string -> []
      | Pbytes_of_string -> []
      | Pignore -> []
      | Pgetglobal (cu, _staticity) ->
        [H.of_string (Compilation_unit.full_path_as_string cu)]
      | Pgetpredef id -> [H.of_string (Ident.name id)]
      | Pmakeblock (tag, _mutability, _shape, _mode) -> [int tag]
      | Pmakefloatblock (_mutability, _mode) -> []
      | Pmakeufloatblock (_mutability, _mode) -> []
      | Pmakelazyblock tag ->
        [int (match tag with Lazy_tag -> 0 | Forward_tag -> 1)]
      | Pfield (n, _ptr, _semantics) -> [int n]
      | Pfield_computed _semantics -> []
      | Psetfield (n, _ptr, _initialization) -> [int n]
      | Psetfield_computed (_ptr, _initialization) -> []
      | Pfloatfield (n, _semantics, _mode) -> [int n]
      | Pufloatfield (n, _semantics) -> [int n]
      | Pmixedfield (p, _shape, _semantics) -> [path p]
      | Psetfloatfield (n, _initialization) -> [int n]
      | Psetufloatfield (n, _initialization) -> [int n]
      | Psetmixedfield (p, _shape, _initialization) -> [path p]
      | Pduprecord (_representation, _size) -> []
      | Pmake_unboxed_product _layouts -> []
      | Punboxed_product_field (n, _layouts) -> [int n]
      | Parray_element_size_in_bytes _kind -> []
      | Pmake_idx_field n -> [int n]
      | Pmake_idx_mixed_field (_shape, n, p) -> [int n; path p]
      | Pmake_idx_array (_kind, _index_kind, _element, p) -> [path p]
      | Pidx_deepen (_element, p) -> [path p]
      | Pwith_stack -> []
      | Pwith_stack_preemptible -> []
      | Pperform -> []
      | Pcontinue -> []
      | Pdiscontinue -> []
      | Pdiscontinue_with_backtrace -> []
      | Preperform -> []
      | Pccall
          { prim_name;
            prim_arity = _;
            prim_alloc = _;
            prim_c_builtin = _;
            prim_effects = _;
            prim_coeffects = _;
            prim_native_name = _;
            prim_native_repr_args = _;
            prim_native_repr_res = _;
            prim_is_layout_poly = _
          } ->
        [H.of_string prim_name]
      | Praise kind ->
        [ int
            (match kind with
            | Raise_regular -> 0
            | Raise_reraise -> 1
            | Raise_notrace -> 2) ]
      | Psequand -> []
      | Psequor -> []
      | Pnot -> []
      | Pphys_equal eq -> [int (match eq with Eq -> 0 | Noteq -> 1)]
      | Pscalar scalar ->
        [ H.of_string
            (Scalar.Operation.to_string
               (Scalar.Operation.map scalar ~f:(fun (_ : L.locality_mode) ->
                    Scalar.Any_locality_mode))) ]
      | Poffsetref n -> [int n]
      | Pstringlength -> []
      | Pstringrefu -> []
      | Pstringrefs -> []
      | Pbyteslength -> []
      | Pbytesrefu -> []
      | Pbytessetu -> []
      | Pbytesrefs -> []
      | Pbytessets -> []
      | Pmakearray (_kind, _mutability, _mode) -> []
      | Pmakearray_dynamic (_kind, _mode, initializer_) ->
        [ int
            (match initializer_ with
            | With_initializer -> 1
            | Uninitialized -> 0) ]
      | Pduparray (_kind, _mutability) -> []
      | Parrayblit { src_mutability = _; dst_array_set_kind = _ } -> []
      | Parraylength _kind -> []
      | Parrayrefu (_ref_kind, _index_kind, _mutability) -> []
      | Parraysetu (_set_kind, _index_kind) -> []
      | Parrayrefs (_ref_kind, _index_kind, _mutability) -> []
      | Parraysets (_set_kind, _index_kind) -> []
      | Pisint { variant_only = _ } -> []
      | Pisnull -> []
      | Pisout -> []
      | Pbigarrayref (unsafe, dimensions, _kind, _layout) ->
        [bool unsafe; int dimensions]
      | Pbigarrayset (unsafe, dimensions, _kind, _layout) ->
        [bool unsafe; int dimensions]
      | Pbigarraydim n -> [int n]
      | Pstring_load_i8 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pstring_load_i16 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pstring_load_16 { unsafe; index_kind = _ } -> [bool unsafe]
      | Pstring_load_32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pstring_load_f32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pstring_load_64 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pstring_load_vec { size; unsafe; index_kind = _; mode = _; boxed = _ }
        ->
        [vector size; bool unsafe]
      | Pstring_load_mask { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbytes_load_i8 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbytes_load_i16 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbytes_load_16 { unsafe; index_kind = _ } -> [bool unsafe]
      | Pbytes_load_32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbytes_load_f32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbytes_load_64 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbytes_load_vec { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pbytes_load_mask { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbytes_set_8 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbytes_set_16 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbytes_set_32 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbytes_set_f32 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbytes_set_64 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbytes_set_vec { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pbytes_set_mask { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbigstring_load_i8 { unsafe; index_kind = _; tagged = _ } ->
        [bool unsafe]
      | Pbigstring_load_i16 { unsafe; index_kind = _; tagged = _ } ->
        [bool unsafe]
      | Pbigstring_load_16 { unsafe; index_kind = _ } -> [bool unsafe]
      | Pbigstring_load_32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbigstring_load_f32 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbigstring_load_64 { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbigstring_load_vec
          { size; checks = c; index_kind = _; mode = _; aligned; boxed = _ } ->
        [vector size; checks c; bool aligned]
      | Pbigstring_load_mask { unsafe; index_kind = _; mode = _; boxed = _ } ->
        [bool unsafe]
      | Pbigstring_set_8 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbigstring_set_16 { unsafe; index_kind = _; tagged = _ } -> [bool unsafe]
      | Pbigstring_set_32 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbigstring_set_f32 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbigstring_set_64 { unsafe; index_kind = _; boxed = _ } -> [bool unsafe]
      | Pbigstring_set_vec
          { size; checks = c; index_kind = _; aligned; boxed = _ } ->
        [vector size; checks c; bool aligned]
      | Pbigstring_set_mask { unsafe; index_kind = _; boxed = _ } ->
        [bool unsafe]
      | Pfloatarray_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pint_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_float_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_float32_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Puntagged_int8_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Puntagged_int16_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_int32_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_int64_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_nativeint_array_load_vec
          { size; unsafe; index_kind = _; mode = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pfloatarray_set_vec { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pint_array_set_vec { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_float_array_set_vec { size; unsafe; index_kind = _; boxed = _ }
        ->
        [vector size; bool unsafe]
      | Punboxed_float32_array_set_vec
          { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Puntagged_int8_array_set_vec { size; unsafe; index_kind = _; boxed = _ }
        ->
        [vector size; bool unsafe]
      | Puntagged_int16_array_set_vec
          { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Punboxed_int32_array_set_vec { size; unsafe; index_kind = _; boxed = _ }
        ->
        [vector size; bool unsafe]
      | Punboxed_int64_array_set_vec { size; unsafe; index_kind = _; boxed = _ }
        ->
        [vector size; bool unsafe]
      | Punboxed_nativeint_array_set_vec
          { size; unsafe; index_kind = _; boxed = _ } ->
        [vector size; bool unsafe]
      | Pctconst const ->
        [ int
            (match const with
            | Big_endian -> 0
            | Word_size -> 1
            | Int_size -> 2
            | Max_wosize -> 3
            | Ostype_unix -> 4
            | Ostype_win32 -> 5
            | Ostype_cygwin -> 6
            | Backend_type -> 7
            | Arch_amd64 -> 8
            | Arch_arm64 -> 9) ]
      | Pint_as_pointer _mode -> []
      | Patomic_load_field { immediate_or_pointer = _ } -> []
      | Patomic_load_mixed_field { index; shape = _ } -> [int index]
      | Patomic_set_field { immediate_or_pointer = _; mode = _ } -> []
      | Patomic_set_mixed_field { index; shape = _; mode = _ } -> [int index]
      | Patomic_exchange_field { immediate_or_pointer = _; mode = _ } -> []
      | Patomic_compare_exchange_field { immediate_or_pointer = _; mode = _ } ->
        []
      | Patomic_compare_set_field { immediate_or_pointer = _; mode = _ } -> []
      | Patomic_fetch_add_field -> []
      | Patomic_add_field -> []
      | Patomic_sub_field -> []
      | Patomic_land_field -> []
      | Patomic_lor_field -> []
      | Patomic_lxor_field -> []
      | Patomic_load_idx { layout = _ } -> []
      | Patomic_set_idx { layout = _; mode = _ } -> []
      | Patomic_exchange_idx { layout = _; mode = _ } -> []
      | Patomic_compare_exchange_idx { layout = _; mode = _ } -> []
      | Patomic_compare_set_idx { layout = _; mode = _ } -> []
      | Patomic_fetch_add_idx -> []
      | Patomic_add_idx -> []
      | Patomic_sub_idx -> []
      | Patomic_land_idx -> []
      | Patomic_lor_idx -> []
      | Patomic_lxor_idx -> []
      | Popaque _layout -> []
      | Pprobe_is_enabled { name; enabled_at_init = _ } -> [H.of_string name]
      | Pobj_dup -> []
      | Pobj_magic _layout -> []
      | Punbox_unit -> []
      | Punbox_vector _size -> []
      | Pbox_vector (_size, _mode) -> []
      | Punbox_mask -> []
      | Pbox_mask _mode -> []
      | Pjoin_vec256 -> []
      | Psplit_vec256 -> []
      | Preinterpret_boxed_vector_as_tuple _size -> []
      | Preinterpret_tuple_as_boxed_vector _size -> []
      | Preinterpret_unboxed_int64_as_tagged_int63 -> []
      | Preinterpret_tagged_int63_as_unboxed_int64 -> []
      | Parray_to_iarray -> []
      | Parray_of_iarray -> []
      | Pget_header _mode -> []
      | Ppeek access -> [peek_or_poke access]
      | Ppoke access -> [peek_or_poke access]
      | Pdls_get -> []
      | Ptls_get -> []
      | Pdomain_index -> []
      | Ppoll -> []
      | Pcpu_relax -> []
      | Pget_idx (_layout, _mutability) -> []
      | Pset_idx (_layout, _mode) -> []
      | Pget_ptr (_layout, _mutability) -> []
      | Pset_ptr (_layout, _mode) -> []
      | Pget_ext_ptr (_layout, _mutability) -> []
      | Pset_ext_ptr (_layout, _mode) -> []
      | Pbox (_layout, _mode) -> []
      | Punbox _layout -> []
    in
    H.of_list (H.of_string (Printlambda.name_of_primitive prim) :: payload)

  let inline_attribute (attr : L.inline_attribute) =
    match attr with
    | Always_inline -> H.of_int 1
    | Never_inline -> H.of_int 2
    | Available_inline -> H.of_int 3
    | Unroll n -> H.combine (H.of_int 4) (H.of_int n)
    | Default_inline -> H.of_int 5

  let inlined_attribute (attr : L.inlined_attribute) =
    match attr with
    | Always_inlined -> H.of_int 1
    | Never_inlined -> H.of_int 2
    | Hint_inlined -> H.of_int 3
    | Forward_inlined -> H.of_int 4
    | Unroll n -> H.combine (H.of_int 5) (H.of_int n)
    | Default_inlined -> H.of_int 6

  let specialise_attribute (attr : L.specialise_attribute) =
    H.of_int
      (match attr with
      | Always_specialise -> 1
      | Never_specialise -> 2
      | Default_specialise -> 3)

  let local_attribute (attr : L.local_attribute) =
    H.of_int
      (match attr with
      | Always_local -> 1
      | Never_local -> 2
      | Default_local -> 3)

  let loop_attribute (attr : L.loop_attribute) =
    H.of_int
      (match attr with Always_loop -> 1 | Never_loop -> 2 | Default_loop -> 3)

  (* The attributes that bear on inlining and on the shape of the function.
     [zero_alloc], [poll], [regalloc] and [unbox_return] do not. *)
  let function_attribute
      ({ inline;
         specialise;
         local;
         loop;
         cold;
         is_a_functor;
         is_opaque;
         stub;
         tmc_candidate;
         may_fuse_arity;
         zero_alloc = _;
         poll = _;
         regalloc = _;
         regalloc_param = _;
         unbox_return = _
       } :
        L.function_attribute) =
    H.node ~tag:1
      [ inline_attribute inline;
        specialise_attribute specialise;
        local_attribute local;
        loop_attribute loop;
        bool cold;
        bool is_a_functor;
        bool is_opaque;
        bool stub;
        bool tmc_candidate;
        bool may_fuse_arity ]

  let lparam
      ({ name; debug_uid = _; layout = _; attributes = _; mode = _ } : L.lparam)
      =
    ident name

  let rec lambda (lam : L.lambda) =
    match lam with
    | Lvar id -> H.node ~tag:1 [ident id]
    | Lmutvar id -> H.node ~tag:2 [ident id]
    | Lconst c -> H.node ~tag:3 [structured_constant c]
    | Lapply
        { ap_func;
          ap_args;
          ap_inlined;
          ap_specialised;
          ap_probe;
          ap_result_layout = _;
          ap_region_close = _;
          ap_mode = _;
          ap_yielding = _;
          ap_loc = _;
          ap_tailcall = _
        } ->
      H.node ~tag:4
        [ lambda ap_func;
          H.of_list (List.map lambda ap_args);
          inlined_attribute ap_inlined;
          specialise_attribute ap_specialised;
          bool (Option.is_some ap_probe) ]
    | Lfunction fn -> H.node ~tag:5 [lfunction fn]
    | Llet (_let_kind, _layout, id, _debug_uid, arg, body) ->
      H.node ~tag:6 [ident id; lambda arg; lambda body]
    | Lmutlet (_layout, id, _debug_uid, arg, body) ->
      H.node ~tag:7 [ident id; lambda arg; lambda body]
    | Lletrec (bindings, body) ->
      H.node ~tag:8
        [ H.of_list
            (List.map
               (fun ({ id; debug_uid = _; def } : L.rec_binding) ->
                 H.combine (ident id) (lfunction def))
               bindings);
          lambda body ]
    | Lprim (prim, args, _loc) ->
      H.node ~tag:9 [primitive prim; H.of_list (List.map lambda args)]
    | Lswitch
        ( arg,
          { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
          _loc,
          _layout ) ->
      (* The keys order the arms, and so their counters. *)
      let cases cases =
        H.of_list
          (List.map
             (fun (key, case) -> H.combine (H.of_int key) (lambda case))
             cases)
      in
      H.node ~tag:10
        [ lambda arg;
          H.of_int sw_numconsts;
          cases sw_consts;
          H.of_int sw_numblocks;
          cases sw_blocks;
          H.of_option (Option.map lambda sw_failaction) ]
    | Lstringswitch (arg, cases, default, _loc, _layout) ->
      H.node ~tag:11
        [ lambda arg;
          H.of_list
            (List.map
               (fun (key, case) -> H.combine (H.of_string key) (lambda case))
               cases);
          H.of_option (Option.map lambda default) ]
    (* Static handler numbers are fresh for every compilation and left out, so
       which handler a [Lstaticraise] jumps to is not hashed. *)
    | Lstaticraise (_label, args) ->
      H.node ~tag:12 [H.of_list (List.map lambda args)]
    | Lstaticcatch (body, (_label, params), handler, _pop_region, _layout) ->
      H.node ~tag:13
        [ lambda body;
          H.of_list
            (List.map (fun (id, _debug_uid, _layout) -> ident id) params);
          lambda handler ]
    | Ltrywith (body, id, _debug_uid, handler, _layout) ->
      H.node ~tag:14 [lambda body; ident id; lambda handler]
    | Lifthenelse (cond, ifso, ifnot, _layout) ->
      H.node ~tag:15 [lambda cond; lambda ifso; lambda ifnot]
    | Lsequence (first, second) -> H.node ~tag:16 [lambda first; lambda second]
    | Lwhile { wh_cond; wh_body } ->
      H.node ~tag:17 [lambda wh_cond; lambda wh_body]
    | Lfor
        { for_id;
          for_from;
          for_to;
          for_dir;
          for_body;
          for_debug_uid = _;
          for_loc = _
        } ->
      H.node ~tag:18
        [ ident for_id;
          lambda for_from;
          lambda for_to;
          H.of_int (match for_dir with Upto -> 0 | Downto -> 1);
          lambda for_body ]
    | Lassign (id, e) -> H.node ~tag:19 [ident id; lambda e]
    | Lsend
        ( kind,
          met,
          obj,
          args,
          _region_close,
          _return_mode,
          _loc,
          _layout,
          _yielding ) ->
      H.node ~tag:20
        [ H.of_int (match kind with Self -> 0 | Public -> 1 | Cached -> 2);
          lambda met;
          lambda obj;
          H.of_list (List.map lambda args) ]
    | Lifused (id, e) -> H.node ~tag:21 [ident id; lambda e]
    (* Debugger events, regions and exclaves are transparent *)
    | Levent (e, _event) -> lambda e
    | Lregion (e, _layout) -> lambda e
    | Lexclave e -> lambda e
    (* Removed by [Slambda.eval] before the translation to Flambda. *)
    | Lsplice (_loc, _slambda) -> L.fatal_error_invalid_constructor lam
    | Lkindtemplate _template -> L.fatal_error_invalid_constructor lam
    | Lkindinstantiate _instantiation -> L.fatal_error_invalid_constructor lam
    | Ltemplate _template -> L.fatal_error_invalid_constructor lam
    | Linstantiate _apply -> L.fatal_error_invalid_constructor lam

  and lfunction
      ({ params;
         body;
         attr;
         kind = _;
         return = _;
         loc = _;
         mode = _;
         ret_mode = _;
         yielding = _
       } :
        L.lfunction) =
    H.node ~tag:1
      [H.of_list (List.map lparam params); function_attribute attr; lambda body]

  let hash hasher hashee =
    Fdo_counter.Function_body_hash.of_int32 (H.to_int32 (hasher hashee))
end

(* CR yusumez: We need to check whether this is a good way to identify
   functions *)
module Leading_tokens = struct
  (* CR yusumez: As above. These are chosen mostly for stability across
     edits. *)
  let primitive_token (prim : L.primitive) =
    match[@ocaml.warning "-4"] prim with
    | Pccall p -> p.prim_name
    | Pgetglobal (cu, _) -> Compilation_unit.name_as_string cu
    | Pgetpredef id -> Ident.name id
    | _ -> Printlambda.name_of_primitive prim

  let leading_tokens ?(limit = 5) ~params body =
    let tokens = ref [] in
    let exception Enough in
    let mention token =
      if not (List.mem token !tokens)
      then (
        tokens := token :: !tokens;
        if List.length !tokens >= limit then raise Enough)
    in
    let ident id = mention (Ident.name id) in
    let rec term (lam : L.lambda) =
      (match lam with
      | Lvar id -> ident id
      | Lmutvar id -> ident id
      | Llet (_let_kind, _layout, id, _debug_uid, _arg, _body) -> ident id
      | Lmutlet (_layout, id, _debug_uid, _arg, _body) -> ident id
      | Ltrywith (_body, id, _debug_uid, _handler, _layout) -> ident id
      | Lassign (id, _arg) -> ident id
      | Lifused (id, _arg) -> ident id
      | Lfor { for_id; _ } -> ident for_id
      | Lfunction { params; _ } ->
        List.iter (fun (p : L.lparam) -> ident p.name) params
      | Lletrec (bindings, _body) ->
        List.iter (fun (b : L.rec_binding) -> ident b.id) bindings
      | Lstaticcatch (_body, (_label, params), _handler, _pop_region, _layout)
        ->
        List.iter (fun (id, _debug_uid, _layout) -> ident id) params
      | Lprim (prim, _args, _loc) -> mention (primitive_token prim)
      | Lconst (Const_base (Const_int n)) -> mention (string_of_int n)
      (* CR yusumez: Other types of constants? *)
      | Lconst
          (Const_base
             ( Const_char _ | Const_untagged_char _ | Const_string _
             | Const_float _ | Const_float32 _ | Const_unboxed_float _
             | Const_unboxed_float32 _ | Const_int8 _ | Const_int16 _
             | Const_int32 _ | Const_int64 _ | Const_nativeint _
             | Const_untagged_int _ | Const_untagged_int8 _
             | Const_untagged_int16 _ | Const_unboxed_int32 _
             | Const_unboxed_int64 _ | Const_unboxed_nativeint _ ))
      | Lconst
          ( Const_block _ | Const_mixed_block _ | Const_float_array _
          | Const_immstring _ | Const_float_block _ | Const_null )
      | Lapply _ | Lswitch _ | Lstringswitch _ | Lstaticraise _ | Lifthenelse _
      | Lsequence _ | Lwhile _ | Lsend _ | Levent _ | Lregion _ | Lexclave _
      | Lsplice _ | Lkindtemplate _ | Lkindinstantiate _ | Ltemplate _
      | Linstantiate _ ->
        ());
      L.iter_head_constructor term lam
    in
    (try
       List.iter ident params;
       term body
     with Enough -> ());
    List.rev !tokens
end

module Naming_context = struct
  type t =
    { enclosing_path : string;
      occurrences : int Misc.Stdlib.String.Tbl.t
          (** The number of functions already given each path in the unit,
              shared by all of its contexts. *)
    }

  let toplevel compilation_unit =
    { enclosing_path = Compilation_unit.full_path_as_string compilation_unit;
      occurrences = Misc.Stdlib.String.Tbl.create 64
    }

  let fresh_discriminator t ~path =
    let index =
      Option.value
        (Misc.Stdlib.String.Tbl.find_opt t.occurrences path)
        ~default:0
    in
    Misc.Stdlib.String.Tbl.replace t.occurrences path (index + 1);
    index
end

type t =
  { definition_id : Fdo_counter.Definition_id.t;
    body_hash : Fdo_counter.Function_body_hash.t
  }

let of_function (context : Naming_context.t) (fn : L.lfunction) =
  let { L.loc; params; body; _ } = fn in
  let params = List.map (fun (p : L.lparam) -> p.name) params in
  let path, anonymous =
    match loc with
    | Loc_known { scopes; loc = _ } ->
      let anonymous =
        match scopes with
        | Cons
            { item = Sc_anonymous_function | Sc_partial_or_eta_wrapper | Sc_lazy;
              _
            } ->
          true
        | Cons
            { item =
                ( Sc_value_definition | Sc_module_definition
                | Sc_class_definition | Sc_method_definition );
              _
            }
        | Empty ->
          false
      in
      ( Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
          scopes,
        anonymous )
    | Loc_unknown -> context.enclosing_path ^ ".(fun)", true
  in
  let path =
    if anonymous
    then
      path ^ "@"
      ^ String.concat "," (Leading_tokens.leading_tokens ~params body)
    else path
  in
  let discriminator = Naming_context.fresh_discriminator context ~path in
  ( { definition_id =
        Fdo_counter.Definition_id.create_original ~demangled_name:path
          ~discriminator;
      body_hash = Body_hash.(hash lfunction fn)
    },
    { context with enclosing_path = path } )

let of_toplevel compilation_unit module_initializer =
  let name = Compilation_unit.full_path_as_string compilation_unit in
  let body_hash = Body_hash.(hash lambda module_initializer) in
  { definition_id =
      Fdo_counter.Definition_id.create_original ~demangled_name:name
        ~discriminator:0;
    body_hash
  }

let definition_id t = t.definition_id

let body_hash t = t.body_hash

let leading_tokens = Leading_tokens.leading_tokens

let entry_annotation t =
  let annotation =
    Fdo_annotation.of_list
      [ { Fdo_counter.position =
            Fdo_counter.Position.create_function_entry t.definition_id;
          inlining_stack = []
        } ]
  in
  Fdo_annotation.With_body_hash.create ~annotation ~body_hash:(Some t.body_hash)
