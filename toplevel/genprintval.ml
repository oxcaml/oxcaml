(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* To print values *)

open Misc
open Format
open Longident
open Path
open Types
open Data_types
open Outcometree

module type OBJ =
  sig
    type t
    val repr : 'a -> t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
    val raw_field : t -> int -> nativeint
    val double_array_tag : int
    val double_field : t -> int -> float
  end

module type EVALPATH =
  sig
    type valu
    val eval_address: Env.address -> valu
    exception Error
    val same_value: valu -> valu -> bool
  end

type ('a, 'b) gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

module type S =
  sig
    type t
    val module_field_for_printing :
      t -> Lambda.module_representation -> int -> t option
    val install_printer :
          Path.t -> Types.type_expr -> (formatter -> t -> unit) -> unit
    val install_generic_printer :
           Path.t -> Path.t ->
           (int -> (int -> t -> Outcometree.out_value,
                    t -> Outcometree.out_value) gen_printer) ->
           unit
    val install_generic_printer' :
           Path.t -> Path.t ->
           (formatter -> t -> unit,
            formatter -> t -> unit) gen_printer ->
           unit
    val remove_printer : Path.t -> unit
    val outval_of_untyped_exception : t -> Outcometree.out_value
    val outval_of_value :
          int -> int ->
          (int -> t -> Types.type_expr -> Outcometree.out_value option) ->
          Env.t -> t -> Types.Lpoly.t -> type_expr -> Outcometree.out_value
  end

module Make(O : OBJ)(EVP : EVALPATH with type valu = O.t) = struct

    type t = O.t

    (* Normally, [Obj.t] has layout [value], but we need to handle nullable
       values at toplevel. flambda2 is allowed to optimise calls to [is_null]
       on an argument with [value] layout to [false], so first convert
       (opaquely!) to a type with [value_or_null] layout. *)
    type obj_or_null : value_or_null

    external obj_or_null : t -> obj_or_null = "%opaque"

    external is_null : obj_or_null -> bool = "%is_null"

    let[@inline] is_null obj = is_null (obj_or_null obj)

    (* Normally, [Obj.is_block] can't be called on [value_or_null]s.
       But here we need to handle nullable values at toplevel. *)
    let is_real_block o = O.is_block o && not (is_null o)

    module ObjTbl = Hashtbl.Make(struct
        type t = O.t
        let equal = (==)
        let hash x =
          try
            Hashtbl.hash x
          with _exn -> 0
      end)

    let tree_of_name (name : string) =
      Oide_ident (Out_type.Out_name.create name)

    (* Given an exception value, we cannot recover its type,
       hence we cannot print its arguments in general.
       Here, we do a feeble attempt to print
       integer, string and float arguments... *)
    let outval_of_untyped_exception_args obj start_offset =
      if O.size obj > start_offset then begin
        let list = ref [] in
        for i = start_offset to O.size obj - 1 do
          let arg = O.field obj i in
          if is_null arg then
            list :=
              Oval_constr (Oide_ident (Out_type.Out_name.create "<null>"), [])
              :: !list
          else if not (O.is_block arg) then
            list := Oval_int (O.obj arg : int) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if O.tag arg = Obj.string_tag then
            list :=
              Oval_string ((O.obj arg : string), max_int, Ostr_string) :: !list
          else if O.tag arg = Obj.double_tag then
            list := Oval_float (O.obj arg : float) :: !list
          else
            list := Oval_constr (tree_of_name "_", []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      if O.tag bucket <> 0 then
        let name = (O.obj (O.field bucket 0) : string)in
        Oval_constr (tree_of_name name, [])
      else
      let name = (O.obj(O.field(O.field bucket 0) 0) : string) in
      let args =
        if (name = "Match_failure"
            || name = "Assert_failure"
            || name = "Undefined_recursive_module")
        && O.size bucket = 2
        && O.tag(O.field bucket 1) = 0
        then outval_of_untyped_exception_args (O.field bucket 1) 0
        else outval_of_untyped_exception_args bucket 1 in
      Oval_constr (tree_of_name name, args)

    (* The user-defined printers. Also used for some builtin types. *)

    type printer =
      | Simple of Types.type_expr * (O.t -> Outcometree.out_value)
      | Generic of Path.t * (int -> (int -> O.t -> Outcometree.out_value,
                                     O.t -> Outcometree.out_value) gen_printer)

    let printers = ref ([
      ( Pident(Ident.create_local "print_unboxed_unit"),
        Simple (Predef.type_unboxed_unit,
                (fun _ -> Oval_unboxed (Oval_stuff "()"))) );
      ( Pident(Ident.create_local "print_unboxed_int"),
        Simple (Predef.type_unboxed_int,
                (fun x -> Oval_unboxed (Oval_int (O.obj x : int)))) );
      ( Pident(Ident.create_local "print_unboxed_float"),
        Simple (Predef.type_unboxed_float,
                (fun x -> Oval_unboxed (Oval_float (O.obj x : float)))) );
      ( Pident(Ident.create_local "print_float32_u"),
        Simple (Predef.type_float32_u,
                (fun x -> Oval_unboxed (Oval_float32 (O.obj x : Obj.t)))) );
      ( Pident(Ident.create_local "print_unboxed_int8"),
        Simple (Predef.type_unboxed_int8,
                (fun x -> Oval_unboxed (Oval_int8 (O.obj x : int)))) );
      ( Pident(Ident.create_local "print_unboxed_int16"),
        Simple (Predef.type_unboxed_int16,
                (fun x -> Oval_unboxed (Oval_int16 (O.obj x : int)))) );
      ( Pident(Ident.create_local "print_int32_u"),
        Simple (Predef.type_int32_u,
                (fun x -> Oval_unboxed (Oval_int32 (O.obj x : int32)))) );
      ( Pident(Ident.create_local "print_nativeint_u"),
        Simple (Predef.type_nativeint_u,
                (fun x ->
                   Oval_unboxed (Oval_nativeint (O.obj x : nativeint)))) );
      ( Pident(Ident.create_local "print_int64_u"),
        Simple (Predef.type_int64_u,
                (fun x -> Oval_unboxed (Oval_int64 (O.obj x : int64)))) );
      ( Pident(Ident.create_local "print_int"),
        Simple (Predef.type_int,
                (fun x -> Oval_int (O.obj x : int))) );
      ( Pident(Ident.create_local "print_float"),
        Simple (Predef.type_float,
                (fun x -> Oval_float (O.obj x : float))) );
      ( Pident(Ident.create_local "print_float32"),
        Simple (Predef.type_float32,
                (fun x -> Oval_float32 (O.obj x : Obj.t))) );
      ( Pident(Ident.create_local "print_char"),
        Simple (Predef.type_char,
                (fun x -> Oval_char (O.obj x : char))) );
      ( Pident(Ident.create_local "print_int8"),
        Simple (Predef.type_int8,
                (fun x -> Oval_int8 (O.obj x : int))) );
      ( Pident(Ident.create_local "print_int16"),
        Simple (Predef.type_int16,
                (fun x -> Oval_int16 (O.obj x : int))) );
      ( Pident(Ident.create_local "print_int32"),
        Simple (Predef.type_int32,
                (fun x -> Oval_int32 (O.obj x : int32))) );
      ( Pident(Ident.create_local "print_nativeint"),
        Simple (Predef.type_nativeint,
                (fun x -> Oval_nativeint (O.obj x : nativeint))) );
      ( Pident(Ident.create_local "print_int64"),
        Simple (Predef.type_int64,
                (fun x -> Oval_int64 (O.obj x : int64)) ))
    ] : (Path.t * printer) list)

    let exn_printer path ppf exn =
      Format_doc.fprintf ppf "<printer %a raised an exception: %s>"
        Printtyp.Doc.path path
        (Printexc.to_string exn)

    let out_exn path exn =
      Oval_printer (fun ppf -> exn_printer path ppf exn)

    let user_printer path f ppf x =
      Format_doc.deprecated_printer
        (fun ppf ->
           try f ppf x with
           | exn -> Format_doc.compat1 exn_printer path ppf exn
        )
        ppf

    let install_printer path ty fn =
      let print_val ppf obj = user_printer path fn ppf obj in
      let printer obj = Oval_printer (fun ppf -> print_val ppf obj) in
      printers := (path, Simple (ty, printer)) :: !printers

    let install_generic_printer function_path constr_path fn =
      printers := (function_path, Generic (constr_path, fn))  :: !printers

    let install_generic_printer' function_path ty_path fn =
      let rec build gp depth =
        match gp with
        | Zero fn ->
            let out_printer obj =
              let printer ppf = user_printer function_path fn ppf obj in
              Oval_printer printer in
            Zero out_printer
        | Succ fn ->
            let print_val fn_arg =
              let print_arg ppf o =
                !Oprint.out_value ppf (fn_arg (depth+1) o) in
              build (fn print_arg) depth in
            Succ print_val in
      printers := (function_path, Generic (ty_path, build fn)) :: !printers

    let remove_printer path =
      let rec remove = function
      | [] -> raise Not_found
      | ((p, _) as printer) :: rem ->
          if Path.same p path then rem else printer :: remove rem in
      printers := remove !printers

    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

    let tree_of_qualified lookup_all get_path env ty_path name =
      (*First, we rewrite double underscore [__] into [.] whenever possible *)
      let ty_path = Out_type.rewrite_double_underscore_paths env ty_path in
      (* If [ty_path] is [M.N.t] and [name] is [Foo], we want to find
         a short name for [M.N.Foo] in the current typing environment.
         Our strategy is to try [Foo], [N.Foo] and [M.N.Foo] in
         turn. *)

      (* Start by transforming the path [M.N.t] into the Longident [M.N.Foo]. *)
      let lid = match Untypeast.lident_of_path ty_path with
        | Lident _ -> Lident name
        | Ldot (p,_) -> Ldot(p, Location.mknoloc name)
        | x -> x
      in

      (* [candidates exn M.N.Foo] is [Foo; N.Foo; M.N.Foo].
         @raise [exn] on functor application. *)
      let candidates apply_exn lid =
        (* [loop M.N [Foo]] is [[Foo]; [N; Foo]; [M; N; Foo]] *)
        let rec loop lid suff = match lid with
          | Lident last -> [suff; (last :: suff)]
          | Ldot({txt=p; _}, {txt=s; _}) -> suff :: loop p (s :: suff)
          | Lapply _ -> raise apply_exn
        in
        loop lid [] (* [[]; [Foo]; [N; Foo]; [M; N; Foo]] *)
        |> List.filter_map Longident.unflatten
      in

      (* A shorter name is correct (matches) if one of its possible
         interpretations (there may be several constructors with the
         same name at different types in a module) has the same type
         path as the one we are printing. *)
      let matches lid =
        match lookup_all lid env with
        | Error _ -> false
        | Ok cstrs ->
            List.exists (fun (cstr, _) ->
              Path.same (get_path cstr) ty_path
            ) cstrs
      in

      let rec tree_of_lident = function
        | Lident name ->
            tree_of_name name
        | Ldot ({txt=lid; _}, {txt=name; _}) ->
            Oide_dot (tree_of_lident lid, name)
        | Lapply ({txt=lid1; _}, {txt=lid2; _}) ->
            Oide_apply (tree_of_lident lid1, tree_of_lident lid2)
      in

      let exception Functor_application in
      match List.find matches (candidates Functor_application lid) with
      | exception (Functor_application | Not_found) ->
          tree_of_lident lid
      | best_lid ->
          tree_of_lident best_lid

    let tree_of_constr =
      tree_of_qualified
        (Env.lookup_all_constructors ~use:false ~loc:Location.none Env.Positive)
        (fun (cstr, _locks) -> Data_types.cstr_res_type_path cstr)

    and tree_of_label =
      tree_of_qualified
        (Env.lookup_all_labels ~use:false ~record_form:Legacy
           ~loc:Location.none Env.Construct)
        Data_types.lbl_res_type_path

    and tree_of_unboxed_product_label =
      tree_of_qualified
        (Env.lookup_all_labels ~use:false ~record_form:Unboxed_product
           ~loc:Location.none Env.Construct)
        Data_types.gen_lbl_res_type_path

    (* An abstract type *)

    let abstract_type =
      let id = Ident.create_local "abstract" in
      let ty = Btype.newgenty (Tconstr (Pident id, [], ref Mnil)) in
      ty

    (* The main printing function *)

    external float32_of_bits : int32 -> Obj.t =
      "caml_float32_of_bits_bytecode"

    let native_scalar_field obj pos
          (kind : unit Mixed_block_shape.Singleton_mixed_block_element.t) =
      let signed bits =
        let modulus = 1 lsl bits in
        let word = Nativeint.to_int (O.raw_field obj pos) land (modulus - 1) in
        Some (O.repr (if word < modulus / 2 then word else word - modulus))
      in
      match kind with
      | Value _ -> Some (O.field obj pos)
      | Float_boxed () | Float64 -> Some (O.repr (O.double_field obj pos))
      | Bits8 -> signed 8
      | Bits16 -> signed 16
      | Bits32 -> Some (O.repr (Nativeint.to_int32 (O.raw_field obj pos)))
      | Bits64 -> Some (O.repr (Int64.of_nativeint (O.raw_field obj pos)))
      | Word -> Some (O.repr (O.raw_field obj pos))
      | Untagged_immediate ->
          Some (O.repr (Nativeint.to_int (O.raw_field obj pos)))
      | Float32 ->
          Some (O.repr
            (float32_of_bits (Nativeint.to_int32 (O.raw_field obj pos))))
      | Vec128 | Vec256 | Vec512 | Mask -> None

    let native_mixed_field obj shape pos =
      match shape.(pos) with
      | Lambda.Product elts when Array.length elts <> 0 -> None
      | _ ->
          let reordered =
            Mixed_block_shape.of_mixed_block_elements shape
              ~print_locality:(fun ppf () -> Format.fprintf ppf "()")
          in
          match
            Mixed_block_shape.lookup_path_producing_new_indexes reordered [pos]
          with
          | [] -> Some (O.repr ())
          | [i] ->
              let counts =
                Mixed_product_bytes.Wrt_path.count_shape shape pos []
              in
              let { Mixed_product_bytes.Wrt_path.offset_bytes; _ } =
                Mixed_product_bytes.Wrt_path.offset_and_gap_unchecked counts
              in
              let word =
                Mixed_product_bytes.Byte_count.on_64_bit_arch offset_bytes / 8
              in
              native_scalar_field obj word
                (Mixed_block_shape.flattened_reordered_shape reordered).(i)
          | _ :: _ :: _ -> None

    let module_field_for_printing obj
          (rep : Lambda.module_representation) pos =
      match rep with
      | Module_value_only _ -> Some (O.field obj pos)
      | Module_mixed (shape, _) -> native_mixed_field obj shape pos

    let inherited_field obj (sort : Jkind.Sort.Const.t) =
      let block () =
        if !Clflags.native_code then
          let element = Lambda.mixed_block_element_of_layout
              (Lambda.layout_of_const_sort sort) in
          native_mixed_field obj [|element|] 0
        else
          match Lambda.layout_of_const_sort sort with
          | Punboxed_product _ -> Some obj
          | _ -> Some (O.field obj 0)
      in
      match Lambda.boxed_representation sort with
      | Block -> block ()
      | Float_block | Immediate_box -> Some obj
      | Immediate64_box ->
          if Sys.word_size <> 64 then block ()
          else
            let bits = Int32.of_int (O.obj obj : int) in
            match sort with
            | Base Bits32 -> Some (O.repr bits)
            | Base Float32 -> Some (O.repr (float32_of_bits bits))
            | _ -> Misc.fatal_error "inherited_field: expected 32-bit scalar"

    type outval_record_rep =
      | Outval_record_boxed
      | Outval_record_unboxed
      | Outval_record_inherited of Jkind.Sort.Const.t
      | Outval_record_mixed_block of Lambda.mixed_block_shape

    type printing_jkind =
      | Print_as_value (* can interpret as a value and print *)
      | Print_as of string (* can't print *)

    let rec print_sort : Jkind.Sort.Const.t -> _ = function
      | Base
          ( Scannable | Void | Float64 | Float32 | Bits8 | Bits16 | Bits32
          | Bits64 | Word | Untagged_immediate ) -> Print_as_value
      | Base (Vec128 | Vec256 | Vec512 | Mask) -> Print_as "<abstr>"
      | Product _ -> Print_as "<unboxed product>"
      | Addressable sort -> print_sort sort
      | Univar _ -> Print_as "<univar>"
      | Genvar _ -> Print_as "<genvar>"

    let print_sort_option : Jkind.Sort.Const.t option -> _ = function
      | None -> Print_as "<unknown>"
      | Some sort -> print_sort sort

    let sorts_of_types env tys =
      Misc.Stdlib.Array.all_somes
        (Array.map
           (fun ty ->
              Option.map (fun s -> s, ty)
                (Jkind.sort_option_of_jkind env (Ctype.type_jkind env ty)))
           tys)

    let sorts_of_labels env lbl_list type_params ty_list () =
      let label_params_and_types, record_params =
        Ctype.instance_label_declarations ~fixed:false
          (lbl_list |> Array.of_list) ~params:type_params
      in
      List.iter2 (Ctype.unify env) record_params
        (Ctype.instance_list ty_list);
      sorts_of_types env (Array.map snd label_params_and_types)

    let outval_mixed_block_rep shape =
      (* Mixed records are only represented as mixed blocks in native code. *)
      if not !Clflags.native_code then Some Outval_record_boxed
      else
        (* CR box: Update this read once addressability
           affects how elements are stored in blocks *)
        let shape = Lambda.transl_mixed_product_shape shape in
        Some (Outval_record_mixed_block shape)

    (* The position of the first field: an extension constructor's block
       starts with its extension slot. *)
    let first_field_pos : Types.variant_representation -> int = function
      | Variant_extensible -> 1
      | Variant_boxed _ | Variant_unboxed | Variant_with_null -> 0

    let outval_rep_of_constructor_shape
          (shape : Types.constructor_representation)
          (vrep : Types.variant_representation) =
      match shape, vrep with
      | Constructor_mixed _, (Variant_unboxed | Variant_with_null) ->
          Misc.fatal_error "a 'mixed' unboxed constructor is impossible"
      | Constructor_uniform_value, (Variant_unboxed | Variant_with_null) ->
          Some Outval_record_unboxed
      | Constructor_uniform_value, (Variant_boxed _ | Variant_extensible) ->
          Some Outval_record_boxed
      | Constructor_mixed shape, (Variant_boxed _ | Variant_extensible) ->
          outval_mixed_block_rep shape
      | (Constructor_undetermined | Constructor_variable _), _ ->
          Misc.fatal_error "variable constructor representation"

    (* Finalize the representation just to be able to print it *)
    let outval_rep_of_constructor env ~sorts_and_types
          (shape : Types.constructor_representation)
          (vrep : Types.variant_representation) =
      let shape : Types.constructor_representation option =
        match shape, vrep with
        | Constructor_undetermined, Variant_unboxed ->
            (* As in [Typedecl.instance_record_representation]: the shape of
               an unboxed constructor is always [Constructor_uniform_value]. *)
            Some Constructor_uniform_value
        | Constructor_undetermined,
          (Variant_boxed _ | Variant_extensible | Variant_with_null) ->
            Option.map (fun l -> Constructor_variable l) (sorts_and_types ())
        | Constructor_variable _, _ ->
            Misc.fatal_error "variable constructor representation"
        | (Constructor_uniform_value | Constructor_mixed _), _ ->
            Some shape
      in
      Option.bind shape (fun shape ->
        let shape =
          Typedecl.finalize_constructor_representation env Location.none shape
        in
        Option.map
          (fun rep -> rep, first_field_pos vrep)
          (outval_rep_of_constructor_shape shape vrep))

    let outval_rep_of_record env ~sorts_and_types
          (rep : Types.record_representation) =
      let finalize rep =
        match Typedecl.finalize_record_representation env Location.none rep with
        | Record_unboxed -> Some (Outval_record_unboxed, 0)
        | Record_boxed | Record_float | Record_ufloat ->
            Some (Outval_record_boxed, 0)
        | Record_mixed shape ->
            Option.map (fun rep -> rep, 0) (outval_mixed_block_rep shape)
        | Record_boxed_inherited_variable sort ->
            Some (Outval_record_inherited
                    (Jkind.Sort.default_for_transl_and_get sort), 0)
        | Record_inlined _ ->
            Misc.fatal_error "inlined record representation"
        | Record_dummy _ ->
            Misc.fatal_error "dummy record representation"
        | Record_undetermined | Record_variable _ | Record_boxed_inherited ->
            Misc.fatal_error "variable record representation"
      in
      match rep with
      | Record_inlined (_, shape, vrep) ->
          outval_rep_of_constructor env ~sorts_and_types shape vrep
      | Record_undetermined ->
          Option.bind (sorts_and_types ())
            (fun l -> finalize (Record_variable l))
      | Record_boxed_inherited ->
          Option.bind (sorts_and_types ()) (function
            | [|sort, _|] ->
                Some (Outval_record_inherited
                        (Jkind.Sort.default_for_transl_and_get sort), 0)
            | _ -> Misc.fatal_error "inherited record must have one field")
      | Record_variable _ ->
          Misc.fatal_error "variable record representation"
      | (Record_unboxed | Record_boxed | Record_float | Record_ufloat
        | Record_mixed _ | Record_dummy _
        | Record_boxed_inherited_variable _) as rep ->
          finalize rep

    let outval_of_value max_steps max_depth check_depth env obj lpoly ty =
      if not @@ Types.Lpoly.is_empty_exn lpoly then Oval_stuff "<lpoly>"
      else

      let printer_steps = ref max_steps in

      let is_value ty =
        match
          Ctype.check_type_jkind env ty (Jkind.Builtin.value_or_null ~why:Probe)
        with
        | Ok _ -> true
        | Error _ -> false
      in

      let nested_values = ObjTbl.create 8 in
      let nest_gen err f depth obj ty =
        let repr = obj in
        (* We can't store non-values in an [ObjTbl.t] when cycle-checking.
           As a result, non-values may be printed twice, but cycles will still
           be detected since every cycle contains at least one value. *)
        if not (is_value ty) || not (is_real_block repr)
           || (O.tag repr >= Obj.no_scan_tag)
        then
          f depth obj ty
        else
          if ObjTbl.mem nested_values repr then
            err
          else begin
            ObjTbl.add nested_values repr ();
            let ret = f depth obj ty in
            ObjTbl.remove nested_values repr;
            ret
          end
      in

      let nest f = nest_gen (Oval_stuff "<cycle>") f in

      let rec tree_of_val depth obj ty =
        decr printer_steps;
        if !printer_steps < 0 || depth < 0 then Oval_ellipsis
        else begin
        try
          find_printer depth env ty obj
        with Not_found ->
          match get_desc ty with
          | Tvar _ | Tunivar _ ->
              Oval_stuff "<poly>"
          | Tarrow _ ->
              Oval_stuff "<fun>"
          | Ttuple(labeled_tys) ->
              (* Mixed tuples are only represented as mixed blocks in native
                 code. Using the [Obj] module here to check would let print out
                 flattened mixed tuples as normal tuples. *)
              if !Clflags.native_code
                 && not (List.for_all (fun (_, ty) -> is_value ty) labeled_tys)
              then Oval_stuff "<abstr>"
              else
                Oval_tuple (tree_of_labeled_val_list 0 depth obj labeled_tys)
          | Tunboxed_tuple(labeled_tys) ->
              Oval_unboxed_tuple
                (tree_of_labeled_val_list 0 depth obj labeled_tys)
          | Tconstr(path, ty_list, _) -> begin
              match get_desc (Ctype.expand_head env ty) with
              | Tconstr(path, [ty_arg], _)
                when Path.same path Predef.path_list ->
                  tree_of_list depth obj ty_arg

              | Tconstr(path, [ty_arg], _)
                when Path.same path Predef.path_array ->
                  tree_of_generic_array Asttypes.Mutable depth obj ty_arg

              | Tconstr(path, [ty_arg], _)
                when Path.same path Predef.path_iarray ->
                  tree_of_generic_array Asttypes.Immutable depth obj ty_arg

              | Tconstr(path, [], _)
                  when Path.same path Predef.path_string ->
                Oval_string ((O.obj obj : string), !printer_steps, Ostr_string)

              | Tconstr (path, [], _)
                  when Path.same path Predef.path_bytes ->
                let s = Bytes.to_string (O.obj obj : bytes) in
                Oval_string (s, !printer_steps, Ostr_bytes)

              | Tconstr(path, [], _)
                  when Path.same path Predef.path_floatarray ->
                Oval_floatarray (O.obj obj : floatarray)

              | Tconstr (path, [ty_arg], _)
                when Path.same path Predef.path_lazy_t ->
                tree_of_lazy depth obj ty_arg

              | Tconstr (path, [_], _)
                when Path.same path Predef.path_expr ->
                let quote : CamlinternalQuote.Code.t = O.obj obj in
                Oval_printer (Format_doc.deprecated_printer (fun ppf ->
                  CamlinternalQuote.Code.print ppf quote))

              | _ ->
                match Env.find_type path env with
                | exception Not_found
                | {type_kind = Type_abstract _; type_manifest = None} ->
                    Oval_stuff "<abstr>"
                | {type_kind = Type_abstract _; type_manifest = Some body;
                   type_params} ->
                    tree_of_val depth obj
                      (instantiate_type env type_params ty_list body)
                | {type_kind = Type_variant (constr_list,rep,_); type_params} ->
                    tree_of_variant depth path type_params ty_list obj
                      constr_list rep
                | {type_kind = Type_record(lbl_list, rep,_); type_params} ->
                    tree_of_record depth path type_params ty_list obj
                      lbl_list rep
                | {type_kind = Type_record_unboxed_product
                                 (lbl_list, _, _);
                  type_params} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        tree_of_record_unboxed_product_fields depth
                          env path type_params ty_list
                          lbl_list 0 obj
                    end
                | {type_kind = Type_open} ->
                    tree_of_extension path ty_list depth obj
            end
          | Tvariant row ->
              tree_of_polyvariant depth obj row
          | Tobject (_, _) ->
              Oval_stuff "<obj>"
          | Tquote _ -> begin
              match Ctype.expand_head env ty with
              | ty' when eq_type ty ty' ->
                  fatal_error "Ill-staged value of quote type"
              | ty -> tree_of_val depth obj ty
              end
          | Tsplice _ -> begin
              match Ctype.expand_head env ty with
              | ty' when eq_type ty ty' ->
                fatal_error "Ill-staged value of splice type"
              | ty -> tree_of_val depth obj ty
              end
          | Tquote_eval _ -> begin
              match Ctype.expand_head env ty with
              | ty' when eq_type ty ty' ->
                Oval_stuff "<eval>"
              | ty -> tree_of_val depth obj ty
              end
          | Tbox _ -> begin
              (* CR box: print the inner value once we support the box
                 primitive *)
              match Ctype.expand_head env ty with
              | ty' when eq_type ty ty' ->
                Oval_stuff "<box>"
              | ty -> tree_of_val depth obj ty
              end
          | Tsubst _ | Tfield(_, _, _, _) | Tnil | Tlink _ | Tof_kind _
          | Tmod _ ->
              fatal_error "Printval.outval_of_value"
          | Tpoly (ty, _) ->
              tree_of_val (depth - 1) obj ty
          | Trepr (ty, _) ->
              tree_of_val (depth - 1) obj ty
          | Tpackage _ ->
              Oval_stuff "<module>"
        end

      and tree_of_list depth obj ty_arg =
        if not (is_real_block obj) then Oval_list []
        else match check_depth depth obj ty with
          | Some x -> x
          | None ->
              let rec tree_of_conses tree_list depth obj ty_arg =
                if !printer_steps < 0 || depth < 0 then
                  Oval_ellipsis :: tree_list
                else if is_real_block obj then
                  let tree = nest tree_of_val (depth - 1)
                                (O.field obj 0) ty_arg
                  in
                  let next_obj = O.field obj 1 in
                  nest_gen (Oval_stuff "<cycle>" :: tree :: tree_list)
                    (tree_of_conses (tree :: tree_list))
                    depth next_obj ty_arg
                else tree_list
              in
              Oval_list
                  (List.rev (tree_of_conses [] depth obj ty_arg))

      and tree_of_generic_array am depth obj ty_arg =
        let obj_block = Obj.Uniform_or_mixed.of_block (O.obj obj) in
        if Obj.Uniform_or_mixed.is_mixed obj_block then
          Oval_stuff "<abstr array>"
        else
          let length = O.size obj in
          if length = 0 then Oval_array ([], am)
          else match check_depth depth obj ty with
            | Some x -> x
            | None ->
                let rec tree_of_items tree_list i =
                  if !printer_steps < 0 || depth < 0 then
                    Oval_ellipsis :: tree_list
                  else if i < length then
                    let elt =
                      let is_double_array = O.tag obj = O.double_array_tag in
                      if is_double_array then O.repr (O.double_field obj i)
                      else O.field obj i
                    in
                    let tree = nest tree_of_val (depth - 1) elt ty_arg in
                    tree_of_items (tree :: tree_list) (i + 1)
                  else tree_list
                in
                Oval_array (List.rev (tree_of_items [] 0), am)

      and tree_of_lazy depth obj ty_arg =
        let obj_tag = O.tag obj in
        (* Lazy values are represented in several possible ways:

            1. a lazy thunk that is not yet forced has tag
              Obj.lazy_tag

            1bis. a lazy thunk that is in the process of
               being forced has tag Obj.forcing_tag

            2. a lazy thunk that has just been forced has tag
              Obj.forward_tag; its first field is the forced
              result, which we can print

            3. when the GC moves a forced trunk with forward_tag,
              or when a thunk is directly created from a value,
              we get a third representation where the value is
              directly exposed, without the Obj.forward_tag
              (if its own tag is not ambiguous, that is neither
              lazy_tag nor forward_tag)

            Note that using Lazy.is_val and Lazy.force would be
            unsafe, because they use the Obj.* functions rather
            than the O.* functions of the functor argument, and
            would thus crash if called from the toplevel
            (debugger/printval instantiates Genprintval.Make with
            an Obj module talking over a socket).
          *)
        if obj_tag = Obj.lazy_tag then Oval_stuff "<lazy>"
        else if obj_tag = Obj.forcing_tag then Oval_stuff "<lazy (forcing)>"
        else begin
            let forced_obj =
              if obj_tag = Obj.forward_tag then O.field obj 0 else obj
            in
            (* calling oneself recursively on forced_obj risks
                having a false positive for cycle detection;
                indeed, in case (3) above, the value is stored
                as-is instead of being wrapped in a forward
                pointer. It means that, for (lazy "foo"), we have
                  forced_obj == obj
                and it is easy to wrongly print (lazy <cycle>) in such
                a case (PR#6669).

                Unfortunately, there is a corner-case that *is*
                a real cycle: using unboxed types one can define

                  type t = T : t Lazy.t -> t [@@unboxed]
                  let rec x = lazy (T x)

                which creates a Forward_tagged block that points to
                itself. For this reason, we still "nest"
                (detect head cycles) on forward tags.
              *)
            let v =
              if obj_tag = Obj.forward_tag
              then nest tree_of_val depth forced_obj ty_arg
              else      tree_of_val depth forced_obj ty_arg
            in
            Oval_lazy v
          end

      and tree_of_variant depth path type_params ty_list obj constr_list rep =
       (* Here we work backwards from the actual runtime value to
          find the appropriate `constructor_declaration` in
          `constr_list`.  `Datarepr.find_constr_by_tag` does most
          of the work, but needs two pieces of information in
          addition to the tag:
          1) Whether the value is a block or immediate (because tags
            are only unique within a category).
          2) The `constructor_description`s, because the declarations
            don't record the jkind information needed to determine
            which constructors are immediate due to void arguments. *)
        let cstrs =
          Env.lookup_all_constructors_from_type ~use:false
            ~loc:Location.none Positive path env
        in
        let constant, tag =
          (* CR dkalinichenko: the null case being represented
             by [-1] is hacky, but there's no simple fix. *)
          if is_null obj then
            true, -1
          else if O.is_block obj then
            false, O.tag obj
          else
            true, O.obj obj
        in
        let analyse cstr {cd_id;cd_args;cd_res} =
          let type_params =
            match cd_res with
              Some t ->
                begin match get_desc t with
                  Tconstr (_,params,_) ->
                    params
                | _ -> assert false end
            | None -> type_params
          in
          let outval_rep ~sorts_and_types =
            outval_rep_of_constructor env ~sorts_and_types cstr.cstr_shape rep
          in
          match cd_args with
          | Cstr_tuple l ->
              let ty_args =
                instantiate_types env type_params ty_list l in
              let ty_args =
                List.map2
                  (fun { ca_sort } ty_arg ->
                     let sort =
                       match ca_sort with
                       | Some sort -> Some sort
                       | None ->
                           (match
                              Ctype.type_sort env ty_arg ~fixed:true
                                ~why:Constructor_arg_projection
                            with
                            | Ok sort ->
                                Some
                                  (Jkind.Sort.default_for_transl_and_get sort)
                            | Error _ -> None)
                     in
                     (ty_arg, print_sort_option sort)
                  ) l ty_args
              in
              let sorts_and_types () =
                sorts_of_types env (Array.of_list (List.map fst ty_args))
              in
              begin match outval_rep ~sorts_and_types with
              | None -> Oval_stuff "<abstr>"
              | Some (rep, pos) ->
                  tree_of_constr_with_args (tree_of_constr env path)
                    (Ident.name cd_id) pos depth obj ty_args rep
              end
          | Cstr_record lbls ->
              let sorts_and_types =
                sorts_of_labels env lbls type_params ty_list
              in
              begin match outval_rep ~sorts_and_types with
              | None -> Oval_stuff "<abstr>"
              | Some (rep, pos) ->
                  let r =
                    tree_of_record_fields depth
                      env path type_params ty_list
                      lbls pos obj rep
                  in
                  Oval_constr(tree_of_constr env path (Ident.name cd_id),
                              [ r ])
              end
        in
        let cstr =
          match rep, cstrs with
          | Variant_unboxed, [((cstr, _locks), _use)] -> Some cstr
          | Variant_unboxed, _ -> None
          | (Variant_boxed _ | Variant_with_null | Variant_extensible), _ ->
          match Datarepr.find_constr_by_tag ~constant tag cstrs with
          | cstr -> Some cstr
          | exception Datarepr.Constr_not_found ->
            match rep with
            | Variant_with_null ->
              begin match
                Datarepr.find_variant_with_null_payload constr_list
              with
              | Some { payload_cstr; _ } ->
                  List.find_map
                    (fun ((cstr, _locks), _use) ->
                       if Uid.equal cstr.cstr_uid payload_cstr.cd_uid
                       then Some cstr else None)
                    cstrs
              | None -> None
              end
            | _ -> None
        in
        let decl_of {cstr_uid; _} =
          List.find_opt (fun {cd_uid; _} -> Uid.equal cd_uid cstr_uid)
            constr_list
        in
        match Option.bind cstr (fun cstr ->
          Option.map (fun cd -> cstr, cd) (decl_of cstr))
        with
        | None -> Oval_stuff "<unknown constructor>"
        | Some (cstr, cd) -> analyse cstr cd

      and tree_of_record depth path type_params ty_list obj lbl_list rep =
        match check_depth depth obj ty with
        | Some x -> x
        | None ->
            let sorts_and_types =
              sorts_of_labels env lbl_list type_params ty_list
            in
            match outval_rep_of_record env ~sorts_and_types rep with
            | None -> Oval_stuff "<abstr>"
            | Some (rep, pos) ->
            tree_of_record_fields depth
              env path type_params ty_list
              lbl_list pos obj rep

      and tree_of_record_fields depth env path type_params ty_list
          lbl_list pos obj rep =
        let rec tree_of_fields first pos = function
          | [] -> []
          | {ld_id; ld_type; ld_sort} :: remainder ->
              let ty_arg = instantiate_type env type_params ty_list ld_type in
              let name = Ident.name ld_id in
              (* PR#5722: print full module path only
                 for first record field *)
              let is_void =
                match ld_sort with
                  | None -> false
                  | Some ld_sort -> Jkind.Sort.Const.(equal void ld_sort)
              in
              let lid =
                if first then tree_of_label env path name
                else tree_of_name name
              and v =
                if is_void then tree_of_val (depth - 1) (O.repr ()) ty_arg
                else tree_of_field rep obj pos depth ty_arg
              in
              (lid, v) :: tree_of_fields false (pos + 1) remainder
        in
        Oval_record (tree_of_fields (pos = 0) pos lbl_list)

      and tree_of_field rep obj pos depth ty_arg =
        let nested fld = nest tree_of_val (depth - 1) fld ty_arg in
        let optional = function
          | Some fld -> nested fld
          | None -> Oval_stuff "<abstr>"
        in
        match Option.map Jkind_types.Sort.strip_head_addressable
                (Jkind.sort_option_of_jkind env (Ctype.type_jkind env ty_arg))
        with
        | Some (Base Void) -> nested (O.repr ())
        | _ ->
        match rep with
        | Outval_record_unboxed -> tree_of_val (depth - 1) obj ty_arg
        | Outval_record_inherited sort -> optional (inherited_field obj sort)
        | Outval_record_boxed ->
            nested
              (if O.tag obj = O.double_array_tag then
                 O.repr (O.double_field obj pos)
               else
                 O.field obj pos)
        | Outval_record_mixed_block shape ->
            optional (native_mixed_field obj shape pos)

      (* CR lmaurer: *Pretty please* let's cut down on the duplication here. *)
      and tree_of_record_unboxed_product_fields depth env path type_params
            ty_list lbl_list pos obj =
        let rec tree_of_fields first pos = function
          | [] -> []
          | {ld_id; ld_type; ld_sort} :: remainder ->
              let ty_arg = instantiate_type env type_params ty_list ld_type in
              let name = Ident.name ld_id in
              (* PR#5722: print full module path only
                 for first record field *)
              let lid =
                if first then tree_of_unboxed_product_label env path name
                else tree_of_name name
              and v =
                match print_sort_option ld_sort with
                | Print_as msg -> Oval_stuff msg
                | Print_as_value ->
                  match lbl_list with
                  | [_] ->
                    (* singleton unboxed records are erased *)
                    tree_of_val (depth - 1) obj ty_arg
                  | _ -> nest tree_of_val (depth - 1) (O.field obj pos) ty_arg
              in
              (lid, v) :: tree_of_fields false (pos + 1) remainder
        in
        Oval_record_unboxed_product (tree_of_fields (pos = 0) pos lbl_list)

      and tree_of_polyvariant depth obj row =
        if is_real_block obj then
          let tag : int = O.obj (O.field obj 0) in
          let rec find = function
            | (l, f) :: fields ->
                if Btype.hash_variant l = tag then
                  match row_field_repr f with
                  | Rpresent(Some ty) | Reither(_,[ty],_) ->
                      let args =
                        nest tree_of_val (depth - 1) (O.field obj 1) ty
                      in
                        Oval_variant (l, Some args)
                  | _ -> find fields
                else find fields
            | [] -> Oval_stuff "<variant>" in
          find (row_fields row)
        else
          let tag : int = O.obj obj in
          let rec find = function
            | (l, _) :: fields ->
                if Btype.hash_variant l = tag then
                  Oval_variant (l, None)
                else find fields
            | [] -> Oval_stuff "<variant>" in
          find (row_fields row)

      and tree_of_labeled_val_list start depth obj labeled_tys =
        let rec tree_list i = function
          | [] -> []
          | (label, ty) :: labeled_tys ->
              let tree = nest tree_of_val (depth - 1) (O.field obj i) ty in
              (label, tree) :: tree_list (i + 1) labeled_tys in
      tree_list start labeled_tys

      (* CR layouts v4: When we allow other jkinds in tuples, this should be
         generalized to take a list or array of jkinds, rather than just
         pairing each type with a bool indicating whether it is void *)
      and tree_of_val_list start depth obj ty_list rep =
        let rec tree_list i = function
          | [] -> []
          | (_, Print_as msg) :: ty_list ->
              Oval_stuff msg :: tree_list (i + 1) ty_list
          | (ty, Print_as_value) :: ty_list ->
              tree_of_field rep obj i depth ty :: tree_list (i + 1) ty_list
        in
      tree_list start ty_list

      and tree_of_constr_with_args
             tree_of_cstr cstr_name start depth obj ty_args rep =
        Oval_constr
          (tree_of_cstr cstr_name, tree_of_val_list start depth obj ty_args rep)

    and tree_of_extension type_path ty_list depth bucket =
      let slot =
        if O.tag bucket <> 0 then bucket
        else O.field bucket 0
      in
      let name = (O.obj(O.field slot 0) : string) in
      try
        (* Attempt to recover the constructor description for the exn
           from its name *)
        let lid =
          try Parse.longident (Lexing.from_string name) with
          (* The syntactic class for extension constructor names
             is an extended form of constructor "Longident.t"s
             that also includes module application (e.g [F(X).A]) *)
           | Syntaxerr.Error _ | Lexer.Error _ -> raise Not_found in
        let cstr = Env.find_constructor_by_name lid env in
        let path =
          match cstr.cstr_tag with
              Extension p -> p
            | _ -> raise Not_found
        in
        let addr = Env.find_constructor_address path env in
        (* Make sure this is the right exception and not an homonym,
           by evaluating the exception found and comparing with the
           identifier contained in the exception bucket *)
        if not (EVP.same_value slot (EVP.eval_address addr))
        then raise Not_found;
        let type_params =
          match get_desc cstr.cstr_res with
            Tconstr (_,params,_) ->
             params
          | _ -> assert false
        in
        let args = instantiate_types env type_params ty_list cstr.cstr_args in
        let args = List.map2 (fun { ca_sort } arg ->
            (arg, print_sort_option ca_sort))
            cstr.cstr_args args
        in
        let rep =
          match cstr.cstr_inlined with
          | Some _ -> Outval_record_unboxed (* the argument is the block *)
          | None -> Outval_record_boxed
        in
        tree_of_constr_with_args tree_of_name name 1 depth bucket args rep
      with Not_found | EVP.Error ->
        match check_depth depth bucket ty with
          Some x -> x
        | None when Path.same type_path Predef.path_exn->
            outval_of_untyped_exception bucket
        | None ->
            Oval_stuff "<extension>"

    and instantiate_type env type_params ty_list ty =
      try Ctype.apply env type_params ty ty_list
      with Ctype.Cannot_apply -> abstract_type

    and instantiate_types env type_params ty_list args =
      List.map (fun {ca_type=ty; _} -> instantiate_type env type_params ty_list ty) args

    and find_printer depth env ty =
      let rec find = function
      | [] -> raise Not_found
      | (_name, Simple (sch, printer)) :: remainder ->
          if Ctype.is_moregeneral env false sch ty
          then printer
          else find remainder
      | (_name, Generic (path, fn)) :: remainder ->
          begin match get_desc (Ctype.expand_head env ty) with
          | Tconstr (p, args, _) when Path.same p path ->
              begin try apply_generic_printer path (fn depth) args
              with exn -> (fun _obj -> out_exn path exn) end
          | _ -> find remainder end in
      find !printers

    and apply_generic_printer path printer args =
      match (printer, args) with
      | (Zero fn, []) ->
          (fun (obj : O.t)-> try fn obj with exn -> out_exn path exn)
      | (Succ fn, arg :: args) ->
          let printer = fn (fun depth obj -> tree_of_val depth obj arg) in
          apply_generic_printer path printer args
      | _ ->
          (fun _obj ->
            let printer ppf =
              Format_doc.fprintf ppf
                "<internal error: incorrect arity for '%a'>"
                Printtyp.Doc.path path in
            Oval_printer printer)


    in nest tree_of_val max_depth obj ty

end
