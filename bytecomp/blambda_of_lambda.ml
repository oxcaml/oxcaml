(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Blambda

type tagged_integer = Scalar.Integral.Taggable.Width.t

(* We represent small integers as sign-extended immediates in bytecode.
   Additionally, all boxable integers are boxed, there are no naked integers,
   and there are no local allocations. *)

let is_nontail : Lambda.region_close -> bool = function
  | Rc_nontail -> true
  | Rc_normal | Rc_close_at_apply -> false

module Storer = Switch.Store (struct
  type t = Lambda.lambda

  type key = t

  let compare_key = Stdlib.compare

  let make_key = Lambda.make_key
end)

let constant_int size n : constant =
  match (size : Scalar.any_locality_mode Scalar.Integral.Width.t) with
  | Taggable Int -> Const_int n
  | Taggable Int8 -> Const_int (Numbers.Int8.to_int (Numbers.Int8.of_int_exn n))
  | Taggable Int16 ->
    Const_int (Numbers.Int16.to_int (Numbers.Int16.of_int_exn n))
  | Boxable (Int32 Any_locality_mode) -> Const_int32 (Int32.of_int n)
  | Boxable (Int64 Any_locality_mode) -> Const_int64 (Int64.of_int n)
  | Boxable (Nativeint Any_locality_mode) ->
    Const_nativeint (Nativeint.of_int n)

let const_int size n = Const_base (constant_int size n)

let tagged_immediate n = Const (const_int (Taggable Int) n)

let unit = Const Lambda.const_unit

let boolnot x = Prim (Boolnot, [x])

let kccallf f fmt = Printf.ksprintf (fun name -> f (Ccall name)) fmt

let ccallf fmt = kccallf Fun.id fmt

let is_immed n = Instruct.immed_min <= n && n <= Instruct.immed_max

let is_representable_const_int = function
  | Const (Const_base (Const_int i)) -> is_immed i
  | _ -> false

let comp_integer_comparison : Scalar.Integer_comparison.t -> comparison =
  function
  | Ceq -> Eq
  | Cne -> Neq
  | Clt -> Ltint
  | Cgt -> Gtint
  | Cle -> Leint
  | Cge -> Geint

let caml_sys_const name =
  let const_name =
    (* clearly [Lambda.compile_time_constant] is a bad name as in bytecode mode
       it's a runtime constant *)
    match (name : Lambda.compile_time_constant) with
    | Big_endian -> "big_endian"
    | Word_size -> "word_size"
    | Int_size -> "int_size"
    | Max_wosize -> "max_wosize"
    | Ostype_unix -> "ostype_unix"
    | Ostype_win32 -> "ostype_win32"
    | Ostype_cygwin -> "ostype_cygwin"
    | Backend_type -> "backend_type"
    | Runtime5 -> "runtime5"
  in
  ccallf "caml_sys_const_%s" const_name

let sign_extend width exp =
  let go ~bits =
    let unused_bits = Sys.int_size - bits in
    match exp with
    | Const (Const_base (Const_int n))
      when is_immed ((n lsl unused_bits) asr unused_bits) ->
      Const (Const_base (Const_int ((n lsl unused_bits) asr unused_bits)))
    | exp ->
      let width = tagged_immediate bits in
      let int_size = Prim (caml_sys_const Int_size, [unit]) in
      let unused_bits = Prim (Subint, [int_size; width]) in
      Prim (Asrint, [Prim (Lslint, [exp; unused_bits]); unused_bits])
  in
  match (width : tagged_integer) with
  | Int -> exp
  | Int8 -> go ~bits:8
  | Int16 -> go ~bits:16

let zero_extend width exp =
  let go ~bits =
    let mask = (1 lsl bits) - 1 in
    match exp with
    | Const (Const_base (Const_int n)) when is_immed (n land mask) ->
      Const (Const_base (Const_int (n land mask)))
    | exp -> Prim (Andint, [exp; tagged_immediate mask])
  in
  match (width : tagged_integer) with
  | Int -> exp
  | Int8 -> go ~bits:8
  | Int16 -> go ~bits:16

let static_cast ~src ~dst x =
  let open struct
    type boxed =
      | Int32
      | Nativeint
      | Int64
      | Float
      | Float32

    type builtin =
      | Boxed of boxed
      | Int

    type value =
      | Boxed of boxed
      | Tagged of tagged_integer

    let value : Scalar.any_locality_mode Scalar.Width.t -> value = function
      | Integral (Taggable tagged) -> Tagged tagged
      | Integral (Boxable (Int32 Any_locality_mode)) -> Boxed Int32
      | Integral (Boxable (Nativeint Any_locality_mode)) -> Boxed Nativeint
      | Integral (Boxable (Int64 Any_locality_mode)) -> Boxed Int64
      | Floating (Float64 Any_locality_mode) -> Boxed Float
      | Floating (Float32 Any_locality_mode) -> Boxed Float32

    let name : builtin -> string = function
      | Int -> "int"
      | Boxed Int32 -> "int32"
      | Boxed Nativeint -> "nativeint"
      | Boxed Int64 -> "int64"
      | Boxed Float -> "float"
      | Boxed Float32 -> "float32"
  end in
  let rec builtin x ~src ~dst =
    match (src : builtin), (dst : builtin) with
    | Boxed Int32, Boxed Int32
    | Boxed Int64, Boxed Int64
    | Boxed Nativeint, Boxed Nativeint
    | Boxed Float, Boxed Float
    | Boxed Float32, Boxed Float32
    | Int, Int ->
      (* the identity function *)
      x
    | Boxed Float32, Boxed (Int64 | Nativeint | Int32)
    | Boxed (Int64 | Nativeint | Int32), Boxed Float32 ->
      (* there are no builtins to convert directly, so we go indirectly via
         float *)
      x
      |> builtin ~src ~dst:(Boxed Float : builtin)
      |> builtin ~src:(Boxed Float : builtin) ~dst
    | Boxed Int64, (Int | Boxed (Int32 | Nativeint | Float))
    | Boxed Nativeint, (Int | Boxed (Int32 | Float))
    | Boxed Int32, (Int | Boxed Float) ->
      (* these happen to break from the more favored naming rule of
         caml_dst_of_src *)
      Prim (ccallf "caml_%s_to_%s" (name src) (name dst), [x])
    | Boxed (Float | Float32), Int
    | (Int | Boxed (Float | Int32 | Nativeint)), Boxed Int64
    | (Int | Boxed (Float | Int32)), Boxed Nativeint
    | (Int | Boxed Float), Boxed Int32
    | (Int | Boxed Float), Boxed Float32
    | (Int | Boxed Float32), Boxed Float ->
      Prim (ccallf "caml_%s_of_%s" (name dst) (name src), [x])
  in
  match value src, value dst with
  | Boxed src, Boxed dst -> builtin x ~src:(Boxed src) ~dst:(Boxed dst)
  | Tagged (Int | Int16 | Int8), Boxed dst ->
    (* we don't need to sign-extend in this case because tagged small integers
       are always represented sign-extended in bytecode, and none of these
       are narrowing conversions *)
    builtin x ~src:Int ~dst:(Boxed dst)
  | Boxed src, Tagged dst ->
    (* this can be a narrowing conversion, so must be sign extended, in order
       that the value is taken mod 2^31 or 2^63 as appropriate.  In addition
       this avoids any ambiguity about whether a random boxed int32 is already
       sign extended (on a 64-bit target). *)
    builtin x ~src:(Boxed src) ~dst:Int |> sign_extend dst
  | Tagged Int8, Tagged (Int8 | Int16 | Int)
  | Tagged Int16, Tagged (Int16 | Int)
  | Tagged Int, Tagged Int ->
    (* we don't need to sign-extend in this case because tagged small integers
       are always represented sign-extended in bytecode, and none of these are
       narrowing conversions *)
    x
  | Tagged Int, Tagged ((Int16 | Int8) as dst)
  | Tagged Int16, Tagged (Int8 as dst) ->
    (* we need to sign-extend for these narrowing conversions because the input
       values are stored in full-width immediates, and we need to take them
       modulo 2^8 or 2^16. *)
    sign_extend dst x

let rec comp_expr (exp : Lambda.lambda) : Blambda.blambda =
  let comp_fun ({ params; body; loc = _ } as lfunction : Lambda.lfunction) :
      Blambda.bfunction =
    (* assume kind = Curried *)
    { params = List.map (fun (p : Lambda.lparam) -> p.name) params;
      body = comp_expr body;
      free_variables = Lambda.free_variables (Lfunction lfunction)
    }
  in
  let comp_rec_binding ({ id; def } : Lambda.rec_binding) : Blambda.rec_binding
      =
    { id; def = comp_fun def }
  in
  match (exp : Lambda.lambda) with
  | Lvar id | Lmutvar id -> Var id
  | Lconst cst -> Const cst
  | Lapply { ap_func; ap_args; ap_region_close } ->
    Apply
      { func = comp_expr ap_func;
        args = List.map comp_expr ap_args;
        nontail = is_nontail ap_region_close
      }
  | Lsend (kind, met, obj, args, rc, _, _, _) ->
    Send
      { method_kind =
          (match (kind : Lambda.meth_kind) with
          | Self -> Self
          | Public -> Public
          | Cached -> assert false);
        met = comp_expr met;
        obj = comp_expr obj;
        args = List.map comp_expr args;
        nontail = is_nontail rc
      }
  | Lfunction f -> Pseudo_event (Function (comp_fun f), f.loc)
  | Llet (_, _k, id, _duid, arg, body) | Lmutlet (_k, id, _duid, arg, body) ->
    (* We are intentionally dropping the [debug_uid] identifiers here. *)
    Let { id; arg = comp_expr arg; body = comp_expr body }
  | Lletrec (decl, body) ->
    Letrec
      { decls = List.map comp_rec_binding decl;
        body = comp_expr body;
        free_variables_of_decls =
          Lambda.free_variables (Lletrec (decl, Lambda.lambda_unit))
      }
  | Lstaticcatch (body, (static_label, args), handler, _, _) ->
    Staticcatch
      { body = comp_expr body;
        id = static_label;
        args = List.map (fun (id, _, _) -> id) args;
        handler = comp_expr handler
      }
  | Lstaticraise (static_label, args) ->
    Staticraise (static_label, List.map comp_expr args)
  | Ltrywith (body, param, _param_duid, handler, _kind) ->
    (* We are intentionally dropping the [debug_uid] identifiers here. *)
    Trywith { body = comp_expr body; param; handler = comp_expr handler }
  | Lifthenelse (cond, ifso, ifnot, _kind) ->
    Ifthenelse
      { cond = comp_expr cond; ifso = comp_expr ifso; ifnot = comp_expr ifnot }
  | Lsequence (exp1, exp2) -> Sequence (comp_expr exp1, comp_expr exp2)
  | Lwhile { wh_cond; wh_body } ->
    While { cond = comp_expr wh_cond; body = comp_expr wh_body }
  | Lfor { for_id; for_from; for_to; for_dir; for_body } ->
    For
      { id = for_id;
        from = comp_expr for_from;
        to_ = comp_expr for_to;
        dir = for_dir;
        body = comp_expr for_body
      }
  | Lswitch
      ( arg,
        { sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction },
        _loc,
        _kind ) ->
    (* Build indirection vectors *)
    let store = Storer.mk_store () in
    let fail =
      match sw_failaction with
      | Some fail -> store.act_store () fail
      | None ->
        (* if there is no failaction (i.e., default action) either
           1. all of the potential cases have been covered, or
           2. Some cases have been refuted.

           In both cases, we arbitrarily pick the first case as the value to put
        *)
        0
    in
    let compile_cases src ~size =
      let dst = Array.make size fail in
      ListLabels.iter src ~f:(fun (n, case) ->
          dst.(n) <- store.act_store () case);
      dst
    in
    (* Compile and label actions *)
    let arg = comp_expr arg in
    let const_cases = compile_cases sw_consts ~size:sw_numconsts in
    let block_cases = compile_cases sw_blocks ~size:sw_numblocks in
    let cases = Array.map comp_expr (store.act_get ()) in
    Switch { arg; const_cases; block_cases; cases }
  | Lstringswitch (arg, sw, d, loc, kind) ->
    comp_expr (Matching.expand_stringswitch loc kind arg sw d)
  | Lassign (id, expr) -> Assign (id, comp_expr expr)
  | Levent (lam, lev) -> Event (comp_expr lam, lev)
  | Lifused (_, exp) | Lregion (exp, _) | Lexclave exp -> comp_expr exp
  | Lprim (primitive, args, loc) -> (
    let simd_is_not_supported () =
      let args = List.map comp_expr args in
      Blambda.Prim (Ccall "caml_simd_bytecode_not_supported", args)
    in
    let wrong_arity ~expected =
      Misc.fatal_errorf "Blambda_of_lambda: %a takes exactly %d %s"
        Printlambda.primitive primitive expected
        (if expected = 1 then "argument" else "arguments")
    in
    let check_arity ~arity =
      match List.compare_length_with args arity with
      | 0 -> List.map comp_expr args
      | _ -> wrong_arity ~expected:arity
    in
    let context_switch c ~arity =
      Blambda.Context_switch (c, check_arity ~arity)
    in
    let pseudo_event t = Blambda.Pseudo_event (t, loc) in
    let variadic primitive =
      Blambda.Prim (primitive, List.map comp_expr args)
    in
    let n_ary primitive ~arity = Blambda.Prim (primitive, check_arity ~arity) in
    let nullary = n_ary ~arity:0 in
    let unary = n_ary ~arity:1 in
    let binary = n_ary ~arity:2 in
    let ternary = n_ary ~arity:3 in
    let indexing_primitive (index_kind : Lambda.array_index_kind) prefix :
        Blambda.primitive =
      let suffix =
        match index_kind with
        | Ptagged_int_index
        | Punboxed_or_untagged_integer_index
            (Untagged_int16 | Untagged_int8 | Untagged_int) ->
          ""
        | Punboxed_or_untagged_integer_index Unboxed_int64 ->
          "_indexed_by_int64"
        | Punboxed_or_untagged_integer_index Unboxed_int32 ->
          "_indexed_by_int32"
        | Punboxed_or_untagged_integer_index Unboxed_nativeint ->
          "_indexed_by_nativeint"
      in
      Ccall (prefix ^ suffix)
    in
    match (primitive : Lambda.primitive) with
    | Pphys_equal cmp -> (
      match check_arity ~arity:2 with
      | [] | [_] | _ :: _ :: _ :: _ -> assert false
      | [x; y] ->
        let prim = match cmp with Eq -> Intcomp Eq | Noteq -> Intcomp Neq in
        (* Optimization for comparisons when the first argument is suitable:
           see [Emitcode]. *)
        if is_representable_const_int y && not (is_representable_const_int x)
        then Prim (prim, [y; x])
        else Prim (prim, [x; y]))
    | Popaque _ | Pobj_magic _ -> (
      match args with
      | [arg] ->
        (* in bytecode we only deal with boxed+tagged floats/ints/units *)
        comp_expr arg
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Punbox_unit -> (
      match args with
      | [x] -> comp_expr x
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pignore -> (
      match args with
      | [arg] -> Sequence (comp_expr arg, unit)
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pnot -> unary Boolnot
    | Psequand -> (
      match args with
      | [x; y] -> Sequand (comp_expr x, comp_expr y)
      | _ -> wrong_arity ~expected:2)
    | Psequor -> (
      match args with
      | [x; y] -> Sequor (comp_expr x, comp_expr y)
      | _ -> wrong_arity ~expected:2)
    | Praise k -> unary (Raise k)
    | Pmakefloatblock _ | Pmakeufloatblock _ ->
      (* In bytecode, float# is boxed, so we can treat these two primitives the
         same. *)
      pseudo_event (variadic Makefloatblock)
    | Pmakemixedblock (tag, _, shape, _) ->
      (* There is no notion of a mixed block at runtime in bytecode. Further,
         source-level unboxed types are represented as boxed in bytecode, so
         no ceremony is needed to box values before inserting them into
         the (normal, unmixed) block.
      *)
      let total_len = Array.length shape in
      pseudo_event (variadic (Make_faux_mixedblock { total_len; tag }))
    | Pmakearray (kind, _, _) ->
      pseudo_event
        (match kind with
        (* arrays of unboxed types have the same representation
           as the boxed ones on bytecode *)
        | Pintarray | Paddrarray | Punboxedoruntaggedintarray _
        | Punboxedfloatarray Unboxed_float32
        | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
          variadic (Makeblock { tag = 0 })
        | Pfloatarray | Punboxedfloatarray Unboxed_float64 ->
          variadic Makefloatblock
        | Punboxedvectorarray _ -> simd_is_not_supported ()
        | Pgenarray -> (
          let block = variadic (Makeblock { tag = 0 }) in
          match args with
          | [] -> block
          | _ :: _ ->
            (* for the floatarray hack *)
            Prim (Ccall "caml_make_array", [block])))
    | Presume -> context_switch Resume ~arity:4
    | Prunstack -> context_switch Runstack ~arity:3
    | Preperform -> context_switch Reperform ~arity:3
    | Pmakearray_dynamic (kind, locality, Uninitialized) -> (
      (* Use a dummy initializer to implement the "uninitialized" primitive *)
      try
        let init : Lambda.lambda =
          match kind with
          | Pgenarray | Paddrarray | Pintarray | Pfloatarray
          | Pgcscannableproductarray _ ->
            Misc.fatal_errorf
              "Array kind %s should have been ruled out by the frontend for \
               %%makearray_dynamic_uninit"
              (Printlambda.array_kind kind)
          | Punboxedoruntaggedintarray
              (Untagged_int8 | Untagged_int16 | Untagged_int) ->
            Misc.unboxed_small_int_arrays_are_not_implemented ()
          | Punboxedfloatarray Unboxed_float32 ->
            Lconst (Const_base (Const_float32 "0.0"))
          | Punboxedfloatarray Unboxed_float64 ->
            Lconst (Const_base (Const_float "0.0"))
          | Punboxedoruntaggedintarray Unboxed_int32 ->
            Lconst (Const_base (Const_int32 0l))
          | Punboxedoruntaggedintarray Unboxed_int64 ->
            Lconst (Const_base (Const_int64 0L))
          | Punboxedoruntaggedintarray Unboxed_nativeint ->
            Lconst (Const_base (Const_nativeint 0n))
          | Punboxedvectorarray _ -> raise Not_found
          | Pgcignorableproductarray ignorables ->
            let rec convert_ignorable
                (ign : Lambda.ignorable_product_element_kind) : Lambda.lambda =
              match ign with
              | Pint_ignorable -> Lconst (Const_base (Const_int 0))
              | Punboxedoruntaggedint_ignorable
                  (Untagged_int8 | Untagged_int16 | Untagged_int) ->
                Misc.unboxed_small_int_arrays_are_not_implemented ()
              | Punboxedfloat_ignorable Unboxed_float32 ->
                Lconst (Const_base (Const_float32 "0.0"))
              | Punboxedfloat_ignorable Unboxed_float64 ->
                Lconst (Const_base (Const_float "0.0"))
              | Punboxedoruntaggedint_ignorable Unboxed_int32 ->
                Lconst (Const_base (Const_int32 0l))
              | Punboxedoruntaggedint_ignorable Unboxed_int64 ->
                Lconst (Const_base (Const_int64 0L))
              | Punboxedoruntaggedint_ignorable Unboxed_nativeint ->
                Lconst (Const_base (Const_nativeint 0n))
              | Pproduct_ignorable ignorables ->
                let fields = List.map convert_ignorable ignorables in
                Lprim
                  ( Pmakeblock (0, Immutable, None, Lambda.alloc_heap),
                    fields,
                    loc )
            in
            convert_ignorable (Pproduct_ignorable ignorables)
        in
        match args with
        | [len] ->
          comp_expr
            (Lprim
               ( Pmakearray_dynamic (kind, locality, With_initializer),
                 [len; init],
                 loc )
              : Lambda.lambda)
        | _ -> wrong_arity ~expected:1
      with Not_found -> simd_is_not_supported ())
    | Pduparray (kind, mutability) -> (
      match args with
      | [Lprim (Pmakearray (kind', _, m), args, _)] ->
        assert (kind = kind');
        comp_expr (Lambda.Lprim (Pmakearray (kind, mutability, m), args, loc))
      | _ -> unary (Ccall "caml_obj_dup"))
    | Pmakeblock (tag, _mut, _, _) ->
      pseudo_event (variadic (Makeblock { tag }))
    | Pmake_unboxed_product _ -> pseudo_event (variadic (Makeblock { tag = 0 }))
    | Pgetglobal cu -> nullary (Getglobal cu)
    | Psetglobal cu -> unary (Setglobal cu)
    | Pgetpredef id -> nullary (Getpredef id)
    | Pfield (n, _, _) | Punboxed_product_field (n, _) -> unary (Getfield n)
    | Parray_element_size_in_bytes _array_kind -> (
      match args with
      | [arg] ->
        let word_size =
          comp_expr
            (Lambda.Lprim (Pctconst Word_size, [Lambda.lambda_unit], loc))
        in
        let element_size = Prim (Lsrint, [word_size; tagged_immediate 3]) in
        Sequence (comp_expr arg, element_size)
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pget_idx _ -> binary (Ccall "caml_unsafe_get_idx_bytecode")
    | Pset_idx _ -> ternary (Ccall "caml_unsafe_set_idx_bytecode")
    | Pmake_idx_field pos ->
      Const (Const_block (0, [Const_base (Const_int pos)]))
    | Pmake_idx_mixed_field (_, pos, path) ->
      let path_consts =
        List.map (fun x -> Const_base (Const_int x)) (pos :: path)
      in
      Const (Const_block (0, path_consts))
    | Pmake_idx_array (_, ik, _, path) -> (
      (* Make a block containing [ to_int index ] ++ path.
         See [jane/doc/extensions/_03-unboxed-types/03-block-indices.md]. *)
      match args with
      | [index] ->
        let index =
          match ik with
          | Ptagged_int_index -> comp_expr index
          | Punboxed_or_untagged_integer_index
              (Untagged_int8 | Untagged_int16 | Untagged_int) ->
            (* [int8#]/[int16#] are already tagged ints on bytecode, so this
               case is likely implemented by [comp_expr index]. But this should
               be unreachable as the frontend doesn't support these indices. *)
            Misc.fatal_error
              "Array block indices with small int indices not expected"
            (* CR mshinwell: this is probably ok for now, but it seems like
               these conversions could silently overflow *)
          | Punboxed_or_untagged_integer_index Unboxed_int64 ->
            unary (Ccall "caml_int64_to_int")
          | Punboxed_or_untagged_integer_index Unboxed_int32 ->
            unary (Ccall "caml_int32_to_int")
          | Punboxed_or_untagged_integer_index Unboxed_nativeint ->
            unary (Ccall "caml_nativeint_to_int")
        in
        let path =
          List.map (fun pos -> Const (Const_base (Const_int pos))) path
        in
        Blambda.Prim (Makeblock { tag = 0 }, index :: path)
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pidx_deepen (_, path) -> (
      (* In bytecode, an index is a block storing a series of positions; deepening
         an index "appends" to the end (by making a new block) *)
      match args with
      | [path_prefix] ->
        let path_prefix = comp_expr path_prefix in
        let path_suffix_consts =
          List.map (fun x -> Const_base (Const_int x)) path
        in
        let path_suffix = Const (Const_block (0, path_suffix_consts)) in
        Blambda.Prim
          (Ccall "caml_deepen_idx_bytecode", [path_prefix; path_suffix])
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pfield_computed _sem -> binary Getvectitem
    | Psetfield (n, _ptr, _init) -> binary (Setfield n)
    | Psetfield_computed (_ptr, _init) -> ternary Setvectitem
    (* In bytecode, float#s are boxed.  So, we can use the existing float
       instructions for the ufloat primitives. *)
    | Pfloatfield (n, _, _) | Pufloatfield (n, _) ->
      pseudo_event (unary (Getfloatfield n))
    | Psetfloatfield (n, _) | Psetufloatfield (n, _) -> binary (Setfloatfield n)
    | Pmixedfield ([], _, _) -> assert false
    | Pmixedfield ([n], _, _) ->
      (* CR layouts: This will need reworking if we ever want bytecode
         to unbox fields that are written with unboxed types in the source
         language. *)
      (* Note, non-value mixed fields are always boxed in bytecode; they
         aren't stored flat like they are in native code.
      *)
      unary (Getfield n)
    | Pmixedfield (hd :: tl, _, _) ->
      (* `Pmixedfield ([idx0, idx1, ..., idxn], [block])` is compiled to
         `Getfield (idxn, [... Getfield (idx1, [Getfield (idx0, [block])])])` *)
      List.fold_left
        (fun expr idx -> Blambda.Prim (Getfield idx, [expr]))
        (unary (Getfield hd)) tl
    | Psetmixedfield ([], _, _) -> assert false
    | Psetmixedfield ([n], _, _) ->
      (* See the comment in the [Pmixedfield] case. *)
      binary (Setfield n)
    | Psetmixedfield (path, _, _) -> (
      (* `Psetmixedfield ([idx0, idx1, ..., idxn], [block; value])` is compiled
         to
         `Setfield (idxn, [... Getfield (idx1, [Getfield (idx0, [block])]);
           value])`
         given the match case above, we know the path should have at least two
         elements. *)
      match args with
      | [] | [_] | _ :: _ :: _ :: _ -> wrong_arity ~expected:2
      | [block; value] -> (
        match List.rev path with
        | [] -> Misc.fatal_error "comp_expr: path must be non-empty"
        | last :: rest -> (
          match List.rev rest with
          | [] ->
            Misc.fatal_error
              "comp_expr: path is expected to have at least two elements"
          | hd :: tl ->
            let block =
              List.fold_left
                (fun expr idx -> Blambda.Prim (Getfield idx, [expr]))
                (Blambda.Prim (Getfield hd, [comp_expr block]))
                tl
            in
            Blambda.Prim (Setfield last, [block; comp_expr value]))))
    | Pduprecord _ -> unary (Ccall "caml_obj_dup")
    | Pccall p -> n_ary (Ccall p.prim_name) ~arity:p.prim_arity
    | Pperform -> context_switch Perform ~arity:1
    | Poffsetref n -> unary (Offsetref n)
    | Pstringlength -> unary (Ccall "caml_ml_string_length")
    | Pbyteslength -> unary (Ccall "caml_ml_bytes_length")
    | Pstringrefs -> binary (Ccall "caml_string_get")
    | Pbytesrefs -> binary (Ccall "caml_bytes_get")
    | Pbytessets -> ternary (Ccall "caml_bytes_set")
    | Pstringrefu | Pbytesrefu -> binary Getbyteschar
    | Pbytessetu -> ternary Setbyteschar
    | Pstring_load_16 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get16")
    | Pstring_load_32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get32")
    | Pstring_load_f32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_getf32")
    | Pstring_load_64 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_string_get64")
    | Pbytes_set_16 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set16")
    | Pbytes_set_32 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set32")
    | Pbytes_set_f32 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_setf32")
    | Pbytes_set_64 { index_kind; _ } ->
      ternary (indexing_primitive index_kind "caml_bytes_set64")
    | Pbytes_load_16 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get16")
    | Pbytes_load_32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get32")
    | Pbytes_load_f32 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_getf32")
    | Pbytes_load_64 { index_kind; _ } ->
      binary (indexing_primitive index_kind "caml_bytes_get64")
    | Parraylength _ -> unary Vectlength
    (* In bytecode, nothing is ever actually stack-allocated, so we ignore the
       array modes (allocation for [Parrayref{s,u}], modification for
       [Parrayset{s,u}]). *)
    | Parrayrefs (Pgenarray_ref _, index_kind, _)
    | Parrayrefs
        ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
          | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
          | Punboxedoruntaggedintarray_ref _ | Pgcscannableproductarray_ref _
          | Pgcignorableproductarray_ref _ ),
          (Punboxed_or_untagged_integer_index _ as index_kind),
          _ ) ->
      binary (indexing_primitive index_kind "caml_array_get")
    | Parrayrefs
        ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_floatarray_get")
    | Parrayrefs
        ( ( Punboxedfloatarray_ref Unboxed_float32
          | Punboxedoruntaggedintarray_ref _ | Paddrarray_ref | Pintarray_ref
          | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_array_get_addr")
    | Parraysets (Pgenarray_set _, index_kind)
    | Parraysets
        ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
          | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
          | Punboxedoruntaggedintarray_set _ | Pgcscannableproductarray_set _
          | Pgcignorableproductarray_set _ ),
          (Punboxed_or_untagged_integer_index _ as index_kind) ) ->
      ternary (indexing_primitive index_kind "caml_array_set")
    | Parraysets
        ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_floatarray_set")
    | Parraysets
        ( ( Punboxedfloatarray_set Unboxed_float32
          | Punboxedoruntaggedintarray_set _ | Paddrarray_set _ | Pintarray_set
          | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_array_set_addr")
    | Parrayrefu (Pgenarray_ref _, index_kind, _)
    | Parrayrefu
        ( ( Paddrarray_ref | Pintarray_ref | Pfloatarray_ref _
          | Punboxedfloatarray_ref (Unboxed_float64 | Unboxed_float32)
          | Punboxedoruntaggedintarray_ref _ | Pgcscannableproductarray_ref _
          | Pgcignorableproductarray_ref _ ),
          (Punboxed_or_untagged_integer_index _ as index_kind),
          _ ) ->
      binary (indexing_primitive index_kind "caml_array_unsafe_get")
    | Parrayrefu
        ( (Punboxedfloatarray_ref Unboxed_float64 | Pfloatarray_ref _),
          Ptagged_int_index,
          _ ) ->
      binary (Ccall "caml_floatarray_unsafe_get")
    | Parrayrefu
        ( ( Punboxedfloatarray_ref Unboxed_float32
          | Punboxedoruntaggedintarray_ref _ | Paddrarray_ref | Pintarray_ref
          | Pgcscannableproductarray_ref _ | Pgcignorableproductarray_ref _ ),
          Ptagged_int_index,
          _ ) ->
      binary Getvectitem
    | Parraysetu (Pgenarray_set _, index_kind)
    | Parraysetu
        ( ( Paddrarray_set _ | Pintarray_set | Pfloatarray_set
          | Punboxedfloatarray_set (Unboxed_float64 | Unboxed_float32)
          | Punboxedoruntaggedintarray_set _ | Pgcscannableproductarray_set _
          | Pgcignorableproductarray_set _ ),
          (Punboxed_or_untagged_integer_index _ as index_kind) ) ->
      ternary (indexing_primitive index_kind "caml_array_unsafe_set")
    | Parraysetu
        ( (Punboxedfloatarray_set Unboxed_float64 | Pfloatarray_set),
          Ptagged_int_index ) ->
      ternary (Ccall "caml_floatarray_unsafe_set")
    | Parraysetu
        ( ( Punboxedfloatarray_set Unboxed_float32
          | Punboxedoruntaggedintarray_set _ | Paddrarray_set _ | Pintarray_set
          | Pgcscannableproductarray_set _ | Pgcignorableproductarray_set _ ),
          Ptagged_int_index ) ->
      ternary Setvectitem
    | Parrayrefs (Punboxedvectorarray_ref _, _, _)
    | Parraysets (Punboxedvectorarray_set _, _)
    | Parrayrefu (Punboxedvectorarray_ref _, _, _)
    | Parraysetu (Punboxedvectorarray_set _, _) ->
      simd_is_not_supported ()
    | Pctconst c -> unary (caml_sys_const c)
    | Pisint _ -> unary Isint
    | Pisout -> binary (Intcomp Ultint)
    | Pbigarrayref (_, n, Pbigarray_float32_t, _) ->
      n_ary (Ccall ("caml_ba_float32_get_" ^ Int.to_string n)) ~arity:(n + 1)
    | Pbigarrayset (_, n, Pbigarray_float32_t, _) ->
      n_ary (Ccall ("caml_ba_float32_set_" ^ Int.to_string n)) ~arity:(n + 2)
    | Pbigarrayref (_, n, _, _) ->
      n_ary (Ccall ("caml_ba_get_" ^ Int.to_string n)) ~arity:(n + 1)
    | Pbigarrayset (_, n, _, _) ->
      n_ary (Ccall ("caml_ba_set_" ^ Int.to_string n)) ~arity:(n + 2)
    | Pbigarraydim n -> unary (Ccall ("caml_ba_dim_" ^ Int.to_string n))
    | Pbigstring_load_16 { unsafe = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get16")
    | Pbigstring_load_32 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get32")
    | Pbigstring_load_f32 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_getf32")
    | Pbigstring_load_64 { unsafe = _; mode = _; index_kind } ->
      binary (indexing_primitive index_kind "caml_ba_uint8_get64")
    | Pbigstring_set_16 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set16")
    | Pbigstring_set_32 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set32")
    | Pbigstring_set_f32 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_setf32")
    | Pbigstring_set_64 { unsafe = _; index_kind } ->
      ternary (indexing_primitive index_kind "caml_ba_uint8_set64")
    | Pint_as_pointer _ -> unary (Ccall "caml_int_as_pointer")
    | Pbytes_to_string -> unary (Ccall "caml_string_of_bytes")
    | Pbytes_of_string -> unary (Ccall "caml_bytes_of_string")
    | Parray_to_iarray -> unary (Ccall "caml_iarray_of_array")
    | Parray_of_iarray -> unary (Ccall "caml_array_of_iarray")
    | Pget_header _ -> unary (Ccall "caml_get_header")
    | Pobj_dup -> unary (Ccall "caml_obj_dup")
    | Patomic_load_field _ -> binary (Ccall "caml_atomic_load_field")
    | Patomic_set_field _ -> ternary (Ccall "caml_atomic_set_field")
    | Patomic_exchange_field _ -> ternary (Ccall "caml_atomic_exchange_field")
    | Patomic_compare_exchange_field _ ->
      n_ary ~arity:4 (Ccall "caml_atomic_compare_exchange_field")
    | Patomic_compare_set_field _ ->
      n_ary ~arity:4 (Ccall "caml_atomic_cas_field")
    | Patomic_fetch_add_field -> ternary (Ccall "caml_atomic_fetch_add_field")
    | Patomic_add_field -> ternary (Ccall "caml_atomic_add_field")
    | Patomic_sub_field -> ternary (Ccall "caml_atomic_sub_field")
    | Patomic_land_field -> ternary (Ccall "caml_atomic_land_field")
    | Patomic_lor_field -> ternary (Ccall "caml_atomic_lor_field")
    | Patomic_lxor_field -> ternary (Ccall "caml_atomic_lxor_field")
    | Pdls_get -> unary (Ccall "caml_domain_dls_get")
    | Ppoll -> unary (Ccall "caml_process_pending_actions_with_root")
    | Pcpu_relax -> unary (Ccall "caml_ml_domain_cpu_relax")
    | Pisnull -> unary (Ccall "caml_is_null")
    | Pstring_load_vec _ | Pbytes_load_vec _ | Pbytes_set_vec _
    | Pbigstring_load_vec _ | Pbigstring_set_vec _ | Pfloatarray_load_vec _
    | Pfloat_array_load_vec _ | Pint_array_load_vec _
    | Punboxed_float_array_load_vec _ | Punboxed_float32_array_load_vec _
    | Punboxed_int32_array_load_vec _ | Punboxed_int64_array_load_vec _
    | Punboxed_nativeint_array_load_vec _ | Pfloatarray_set_vec _
    | Pfloat_array_set_vec _ | Pint_array_set_vec _
    | Punboxed_float_array_set_vec _ | Punboxed_float32_array_set_vec _
    | Punboxed_int32_array_set_vec _ | Punboxed_int64_array_set_vec _
    | Punboxed_nativeint_array_set_vec _ | Pbox_vector _ | Punbox_vector _ ->
      simd_is_not_supported ()
    | Preinterpret_tagged_int63_as_unboxed_int64 ->
      if Target_system.is_64_bit ()
      then unary (Ccall "caml_reinterpret_tagged_int63_as_unboxed_int64")
      else
        Misc.fatal_error
          "Preinterpret_tagged_int63_as_unboxed_int64 can only be used on \
           64-bit targets"
    | Preinterpret_unboxed_int64_as_tagged_int63 ->
      if Target_system.is_64_bit ()
      then unary (Ccall "caml_reinterpret_unboxed_int64_as_tagged_int63")
      else
        Misc.fatal_error
          "Preinterpret_unboxed_int64_as_tagged_int63 can only be used on \
           64-bit targets"
    | Pmakearray_dynamic (kind, locality, With_initializer) -> (
      if List.compare_length_with args 2 <> 0
      then
        Misc.fatal_error
          "Bytegen.comp_primitive: Pmakearray_dynamic takes two arguments for \
           [With_initializer]";
      (* CR layouts v4.0: This is "wrong" for unboxed types. It should construct
         blocks that can't be marshalled. We've decided to ignore that problem
         in the short term, as it's unlikely to cause issues - see the internal
         arrays epic for out plan to deal with it. *)
      match kind with
      | Punboxedvectorarray _ -> simd_is_not_supported ()
      | Pgenarray | Pintarray | Paddrarray | Punboxedoruntaggedintarray _
      | Pfloatarray | Punboxedfloatarray _ | Pgcscannableproductarray _
      | Pgcignorableproductarray _ -> (
        match locality with
        | Alloc_heap -> binary (Ccall "caml_make_vect")
        | Alloc_local -> binary (Ccall "caml_make_local_vect")))
    | Parrayblit { src_mutability = _; dst_array_set_kind } -> (
      match dst_array_set_kind with
      | Punboxedvectorarray_set _ -> simd_is_not_supported ()
      | Pgenarray_set _ | Pintarray_set | Paddrarray_set _
      | Punboxedoruntaggedintarray_set _ | Pfloatarray_set
      | Punboxedfloatarray_set _ | Pgcscannableproductarray_set _
      | Pgcignorableproductarray_set _ ->
        n_ary (Ccall "caml_array_blit") ~arity:5)
    | Pprobe_is_enabled _ | Ppeek _ | Ppoke _ ->
      Misc.fatal_errorf "Blambda_of_lambda: %a is not supported in bytecode"
        Printlambda.primitive primitive
    | Pmakelazyblock Lazy_tag ->
      pseudo_event (variadic (Makeblock { tag = Config.lazy_tag }))
    | Pmakelazyblock Forward_tag ->
      pseudo_event (variadic (Makeblock { tag = Obj.forward_tag }))
    | Pscalar (Unary unary) -> (
      match args with
      | [x] -> comp_unary_scalar_intrinsic unary (comp_expr x)
      | [] | _ :: _ :: _ -> wrong_arity ~expected:1)
    | Pscalar (Binary binary) -> (
      match args with
      | [x; y] ->
        comp_binary_scalar_intrinsic binary (comp_expr x) (comp_expr y)
      | [] | [_] | _ :: _ :: _ -> wrong_arity ~expected:2))

and comp_binary_scalar_intrinsic op x y =
  let prim prim = Prim (prim, [x; y]) in
  let ccall fmt = kccallf prim fmt in
  match (op : _ Scalar.Operation.Binary.t) with
  | Integral (size, op) -> (
    match Scalar.Integral.width size with
    | Taggable taggable -> (
      match op with
      | Add ->
        (match y with
        | Const (Const_base (Const_int y)) when is_immed y ->
          Prim (Offsetint y, [x])
        | _ -> prim Addint)
        |> sign_extend taggable
      | Sub ->
        (match y with
        | Const (Const_base (Const_int y)) when is_immed (-y) ->
          Prim (Offsetint (-y), [x])
        | _ -> prim Subint)
        |> sign_extend taggable
      | Mul -> prim Mulint |> sign_extend taggable
      | Div (Safe | Unsafe) -> prim Divint |> sign_extend taggable
      | Mod (Safe | Unsafe) -> prim Modint |> sign_extend taggable
      | And -> prim Andint
      | Or -> prim Orint
      | Xor -> prim Xorint)
    | Boxable
        (( Int32 Any_locality_mode
         | Nativeint Any_locality_mode
         | Int64 Any_locality_mode ) as size) -> (
      let size = Scalar.Integral.Boxable.Width.to_string size in
      let c suffix = ccall "caml_%s_%s" size suffix in
      match op with
      | Add -> c "add"
      | Sub -> c "sub"
      | Mul -> c "mul"
      | Div (Safe | Unsafe) -> c "div"
      | Mod (Safe | Unsafe) -> c "mod"
      | And -> c "and"
      | Or -> c "or"
      | Xor -> c "xor"))
  | Floating (size, ((Add | Sub | Mul | Div) as op)) ->
    let op = Scalar.Operation.Binary.Float_op.to_string op in
    let size = Scalar.Floating.Width.to_string (Scalar.Floating.width size) in
    ccall "caml_%s_%s" op size
  | Shift (size, op, Int) -> (
    match Scalar.Integral.width size with
    | Taggable taggable -> (
      match op with
      | Asr -> prim Asrint
      | Lsl -> sign_extend taggable (prim Lslint)
      | Lsr -> sign_extend taggable (Prim (Lsrint, [zero_extend taggable x; y]))
      )
    | Boxable
        (( Int32 Any_locality_mode
         | Nativeint Any_locality_mode
         | Int64 Any_locality_mode ) as size) -> (
      let size = Scalar.Integral.Boxable.Width.to_string size in
      match op with
      | Lsl -> ccall "caml_%s_shift_left" size
      | Lsr -> ccall "caml_%s_shift_right_unsigned" size
      | Asr -> ccall "caml_%s_shift_right" size))
  | Icmp (size, cmp) -> (
    match Scalar.Integral.width size with
    | Taggable (Int | Int8 | Int16) ->
      if is_representable_const_int y && not (is_representable_const_int x)
      then
        Prim
          ( Intcomp
              (comp_integer_comparison (Scalar.Integer_comparison.swap cmp)),
            [y; x] )
      else prim (Intcomp (comp_integer_comparison cmp))
    | Boxable
        ( Int32 Any_locality_mode
        | Nativeint Any_locality_mode
        | Int64 Any_locality_mode ) -> (
      match cmp with
      | Ceq -> prim (Ccall "caml_equal")
      | Cne -> prim (Ccall "caml_notequal")
      | Clt -> prim (Ccall "caml_lessthan")
      | Cle -> prim (Ccall "caml_lessequal")
      | Cgt -> prim (Ccall "caml_greaterthan")
      | Cge -> prim (Ccall "caml_greaterequal")))
  | Fcmp (size, cmp) -> (
    match Scalar.Floating.width size with
    | (Float64 Any_locality_mode | Float32 Any_locality_mode) as size -> (
      let size = Scalar.Floating.Width.to_string size in
      let c name = ccall "caml_%s_%s" name size in
      match cmp with
      | CFeq -> c "eq"
      | CFneq -> c "neq"
      | CFlt -> c "lt"
      | CFnlt -> c "lt" |> boolnot
      | CFgt -> c "gt"
      | CFngt -> c "gt" |> boolnot
      | CFle -> c "le"
      | CFnle -> c "le" |> boolnot
      | CFge -> c "ge"
      | CFnge -> c "ge" |> boolnot))
  | Three_way_compare size -> (
    match Scalar.width size with
    | Integral (Taggable (Int | Int8 | Int16)) -> ccall "caml_int_compare"
    | ( Integral
          (Boxable
            ( Int32 Any_locality_mode
            | Nativeint Any_locality_mode
            | Int64 Any_locality_mode ))
      | Floating (Float64 Any_locality_mode | Float32 Any_locality_mode) ) as
      size ->
      ccall "caml_%s_compare" (Scalar.Width.to_string size))

and comp_unary_scalar_intrinsic op x =
  let prim prim = Prim (prim, [x]) in
  let ccall fmt = kccallf prim fmt in
  match (op : _ Scalar.Operation.Unary.t) with
  | Integral (size, op) -> (
    let comp_offset n =
      comp_binary_scalar_intrinsic
        (Scalar.Operation.Binary.Integral (size, Add))
        x
        (Const (const_int (Scalar.Integral.width size) n))
    in
    match op with
    | Succ -> comp_offset 1
    | Pred -> comp_offset (-1)
    | Neg -> (
      match Scalar.Integral.width size with
      | Boxable
          (( Int32 Any_locality_mode
           | Nativeint Any_locality_mode
           | Int64 Any_locality_mode ) as size) ->
        ccall "caml_%s_neg" (Scalar.Integral.Boxable.Width.to_string size)
      | Taggable taggable -> sign_extend taggable (prim Negint))
    | Bswap -> (
      match Scalar.Integral.width size with
      | Taggable Int8 -> x
      | Taggable ((Int16 | Int) as bswap16) ->
        sign_extend bswap16 (ccall "caml_bswap16")
      | Boxable boxed ->
        ccall "caml_%s_bswap" (Scalar.Integral.Boxable.Width.to_string boxed)))
  | Floating (size, ((Abs | Neg) as op)) -> (
    match Scalar.Floating.width size with
    | (Float32 Any_locality_mode | Float64 Any_locality_mode) as size ->
      ccall "caml_%s_%s"
        (Scalar.Operation.Unary.Float_op.to_string op)
        (Scalar.Floating.Width.to_string size))
  | Static_cast { src; dst } ->
    static_cast x ~src:(Scalar.width src) ~dst:(Scalar.width dst)

let blambda_of_lambda x = comp_expr x
