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
open Outcometree
module Out_name = Printtyp.Out_name

module type OBJ =
  sig
    type t
    val repr : 'a -> t
    val obj : t -> 'a
    val is_block : t -> bool
    val tag : t -> int
    val size : t -> int
    val field : t -> int -> t
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
          Env.t -> t -> type_expr -> Outcometree.out_value
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
            list := Oval_constr (Oide_ident (Out_name.create "<null>"), []) :: !list
          else if not (O.is_block arg) then
            list := Oval_int (O.obj arg : int) :: !list
               (* Note: this could be a char or a constant constructor... *)
          else if O.tag arg = Obj.string_tag then
            list :=
              Oval_string ((O.obj arg : string), max_int, Ostr_string) :: !list
          else if O.tag arg = Obj.double_tag then
            list := Oval_float (O.obj arg : float) :: !list
          else
            list := Oval_constr (Oide_ident (Out_name.create "_"), []) :: !list
        done;
        List.rev !list
      end
      else []

    let outval_of_untyped_exception bucket =
      if O.tag bucket <> 0 then
        let name = Out_name.create (O.obj (O.field bucket 0) : string) in
        Oval_constr (Oide_ident name, [])
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
      Oval_constr (Oide_ident (Out_name.create name), args)

    (* The user-defined printers. Also used for some builtin types. *)

    type printer =
      | Simple of Types.type_expr * (O.t -> Outcometree.out_value)
      | Generic of Path.t * (int -> (int -> O.t -> Outcometree.out_value,
                                     O.t -> Outcometree.out_value) gen_printer)

    (* CR mixed blocks v1: It would be good, and not hard, to add proper
       printing support for unboxed values. *)
    let printers = ref ([
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
                (fun x -> Oval_int (O.obj x : int))) );
      ( Pident(Ident.create_local "print_int16"),
        Simple (Predef.type_int16,
                (fun x -> Oval_int (O.obj x : int))) );
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

    let exn_printer ppf path exn =
      fprintf ppf "<printer %a raised an exception: %s>" Printtyp.path path
        (Printexc.to_string exn)

    let out_exn path exn =
      Oval_printer (fun ppf -> exn_printer ppf path exn)

    let install_printer path ty fn =
      let print_val ppf obj =
        try fn ppf obj with exn -> exn_printer ppf path exn in
      let printer obj = Oval_printer (fun ppf -> print_val ppf obj) in
      printers := (path, Simple (ty, printer)) :: !printers

    let install_generic_printer function_path constr_path fn =
      printers := (function_path, Generic (constr_path, fn))  :: !printers

    let install_generic_printer' function_path ty_path fn =
      let rec build gp depth =
        match gp with
        | Zero fn ->
            let out_printer obj =
              let printer ppf =
                try fn ppf obj with exn -> exn_printer ppf function_path exn in
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

    let tree_of_qualified find env ty_path name =
      match ty_path with
      | Pident _ ->
          Oide_ident name
      | Pdot(p, _s) ->
          if
            match get_desc (find (Lident (Out_name.print name)) env) with
            | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
            | _ -> false
            | exception Not_found -> false
          then Oide_ident name
          else Oide_dot (Printtyp.tree_of_path p, Out_name.print name)
      | Papply _ ->
          Printtyp.tree_of_path ty_path
      | Pextra_ty _ ->
          (* These can only appear directly inside of the associated
             constructor so we can just drop the prefix *)
          Oide_ident name

    let tree_of_constr =
      tree_of_qualified
        (fun lid env ->
          (Env.find_constructor_by_name lid env).cstr_res)

    and tree_of_label =
      tree_of_qualified
        (fun lid env ->
          (Env.find_label_by_name Legacy lid env).lbl_res)

    (* An abstract type *)

    let abstract_type =
      let id = Ident.create_local "abstract" in
      let ty = Btype.newgenty (Tconstr (Pident id, [], ref Mnil)) in
      ty

    (* The main printing function *)

    type outval_record_rep =
      | Outval_record_boxed
      | Outval_record_unboxed
      | Outval_record_mixed_block of mixed_product_shape

    type printing_jkind =
      | Print_as_value (* can interpret as a value and print *)
      | Print_as of string (* can't print *)

    let print_sort : Jkind.Sort.Const.t -> _ = function
      | Base Value -> Print_as_value
      | Base Void -> Print_as "<void>"
      | Base (Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 |
              Vec128 | Vec256 | Vec512 | Word) -> Print_as "<abstr>"
      | Product _ -> Print_as "<unboxed product>"

    let outval_of_value max_steps max_depth check_depth env obj ty =

      let printer_steps = ref max_steps in

      let nested_values = ObjTbl.create 8 in
      let nest_gen err f depth obj ty =
        let repr = obj in
        if not (is_real_block repr) then
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
              Oval_tuple (tree_of_labeled_val_list 0 depth obj labeled_tys)
          | Tunboxed_tuple(labeled_tys) ->
              Oval_unboxed_tuple
                (tree_of_labeled_val_list 0 depth obj labeled_tys)
          | Tconstr(path, [ty_arg], _)
            when Path.same path Predef.path_list ->
              if is_real_block obj then
                match check_depth depth obj ty with
                  Some x -> x
                | None ->
                    let rec tree_of_conses tree_list depth obj ty_arg =
                      if !printer_steps < 0 || depth < 0 then
                        Oval_ellipsis :: tree_list
                      else if is_real_block obj then
                        let tree =
                          nest tree_of_val (depth - 1) (O.field obj 0) ty_arg
                        in
                        let next_obj = O.field obj 1 in
                        nest_gen (Oval_stuff "<cycle>" :: tree :: tree_list)
                          (tree_of_conses (tree :: tree_list))
                          depth next_obj ty_arg
                      else tree_list
                    in
                    Oval_list (List.rev (tree_of_conses [] depth obj ty_arg))
              else
                Oval_list []
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

          | Tconstr (path, [ty_arg], _)
            when Path.same path Predef.path_lazy_t ->
             let obj_tag = O.tag obj in
             (* Lazy values are represented in three possible ways:

                1. a lazy thunk that is not yet forced has tag
                   Obj.lazy_tag

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
          | Tconstr(path, ty_list, _) -> begin
              try
                let decl = Env.find_type path env in
                match decl with
                | {type_kind = Type_abstract _; type_manifest = None} ->
                    Oval_stuff "<abstr>"
                | {type_kind = Type_abstract _; type_manifest = Some body} ->
                    tree_of_val depth obj
                      (instantiate_type env decl.type_params ty_list body)
                | {type_kind = Type_variant (constr_list, rep, _)} ->
                  (* Here we work backwards from the actual runtime value to
                     find the appropriate `constructor_declaration` in
                     `constr_list`.  `Datarepr.find_constr_by_tag` does most
                     of the work, but needs two pieces of information in
                     addition to the tag:
                     1) Whether the value is a block or immediate (because tags
                        are only unique within a category).
                     2) The `constructor_description`s, because the declarations
                        don't record the jkind information needed to determine
                        which constructors are immediate due to void arguments.
                  *)
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
                    let {cd_id;cd_args;cd_res} =
                      try
                        (* CR dkalinichenko: this is broken for unboxed variants:
                           unless the tag of the inner value just happens to be 0,
                           [Datarepr.find_constr_by_tag] will fail. *)
                        let {cstr_uid} =
                          Datarepr.find_constr_by_tag ~constant tag cstrs
                        in
                        List.find (fun {cd_uid} -> Uid.equal cd_uid cstr_uid)
                          constr_list
                      with
                      | Datarepr.Constr_not_found | Not_found ->
                        (* If a [Variant_with_null] is not a [Null],
                            it's guaranteed to be [This value]. *)
                        match rep with
                        | Variant_with_null -> List.nth constr_list 1
                        | _ -> raise Datarepr.Constr_not_found
                    in
                    let type_params =
                      match cd_res with
                        Some t ->
                          begin match get_desc t with
                            Tconstr (_,params,_) ->
                              params
                          | _ -> assert false end
                      | None -> decl.type_params
                    in
                    let unbx =
                      match rep with
                      | Variant_unboxed -> true
                      | Variant_with_null when tag = -1 -> false
                      | Variant_with_null -> true
                      | Variant_boxed _ | Variant_extensible -> false
                    in
                    begin
                      match cd_args with
                      | Cstr_tuple l ->
                          let ty_args =
                            instantiate_types env type_params ty_list l in
                          let ty_args =
                            List.map2
                              (fun { ca_sort } ty_arg ->
                                 (ty_arg, print_sort ca_sort)
                              ) l ty_args
                          in
                          tree_of_constr_with_args (tree_of_constr env path)
                            (Ident.name cd_id) false 0 depth obj
                            ty_args unbx
                      | Cstr_record lbls ->
                          let rep =
                            if unbx then
                              Outval_record_unboxed
                            else
                              Outval_record_boxed
                          in
                          let r =
                            tree_of_record_fields depth
                              env path type_params ty_list
                              lbls 0 obj rep
                          in
                          Oval_constr(tree_of_constr env path
                                        (Out_name.create (Ident.name cd_id)),
                                      [ r ])
                    end
                | {type_kind = Type_record(lbl_list, rep, _)} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let pos =
                          match rep with
                          | Record_inlined (_, _, Variant_extensible) -> 1
                          | _ -> 0
                        in
                        let rep =
                          match rep with
                          | Record_inlined (_, Constructor_mixed _,
                                            Variant_unboxed) ->
                              Misc.fatal_error
                                "a 'mixed' unboxed record is impossible"
                          | Record_inlined (_, Constructor_uniform_value,
                                            Variant_unboxed)
                          | Record_unboxed
                              -> Outval_record_unboxed
                          | Record_boxed _ | Record_float | Record_ufloat
                          | Record_inlined (_, Constructor_uniform_value, _)
                              -> Outval_record_boxed
                          | Record_inlined (_, Constructor_mixed mixed, _)
                          | Record_mixed mixed
                              ->
                                (* Mixed records are only represented as
                                   mixed blocks in native code.
                                *)
                                if !Clflags.native_code
                                then Outval_record_mixed_block mixed
                                else Outval_record_boxed
                        in
                        tree_of_record_fields depth
                          env path decl.type_params ty_list
                          lbl_list pos obj rep
                    end
                | {type_kind = Type_record_unboxed_product
                                 (lbl_list, Record_unboxed_product, _)} ->
                    begin match check_depth depth obj ty with
                      Some x -> x
                    | None ->
                        let pos = 0 in
                        tree_of_record_unboxed_product_fields depth
                          env path decl.type_params ty_list
                          lbl_list pos obj
                    end
                | {type_kind = Type_open} ->
                    tree_of_extension path ty_list depth obj
              with
                Not_found ->                (* raised by Env.find_type *)
                  Oval_stuff "<abstr>"
              | Datarepr.Constr_not_found -> (* raised by find_constr_by_tag *)
                  Oval_stuff "<unknown constructor>"
              end
          | Tvariant row ->
              if O.is_block obj then
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
          | Tobject (_, _) ->
              Oval_stuff "<obj>"
          | Tsubst _ | Tfield(_, _, _, _) | Tnil | Tlink _ | Tof_kind _  ->
              fatal_error "Printval.outval_of_value"
          | Tpoly (ty, _) ->
              tree_of_val (depth - 1) obj ty
          | Tpackage _ ->
              Oval_stuff "<module>"
        end

      and tree_of_record_fields depth env path type_params ty_list
          lbl_list pos obj rep =
        let rec tree_of_fields first pos = function
          | [] -> []
          | {ld_id; ld_type; ld_sort} :: remainder ->
              let ty_arg = instantiate_type env type_params ty_list ld_type in
              let name = Ident.name ld_id in
              (* PR#5722: print full module path only
                 for first record field *)
              let is_void = Jkind.Sort.Const.(equal void ld_sort) in
              let lid =
                if first then tree_of_label env path (Out_name.create name)
                else Oide_ident (Out_name.create name)
              and v =
                if is_void then Oval_stuff "<void>"
                else match rep with
                  | Outval_record_unboxed -> tree_of_val (depth - 1) obj ty_arg
                  | Outval_record_boxed ->
                      let fld =
                        if O.tag obj = O.double_array_tag then
                          O.repr (O.double_field obj pos)
                        else
                          O.field obj pos
                      in
                      nest tree_of_val (depth - 1) fld ty_arg
                  | Outval_record_mixed_block shape ->
                      let fld =
                        match shape.(pos) with
                        | Value -> `Continue (O.field obj pos)
                        | Float_boxed | Float64 ->
                            `Continue (O.repr (O.double_field obj pos))
                        | Float32 | Bits8 | Bits16 | Bits32 | Bits64
                        | Vec128 | Vec256 | Vec512 | Word | Product _ ->
                            `Stop (Oval_stuff "<abstr>")
                        | Void ->
                            `Stop (Oval_stuff "<void>")
                      in
                      match fld with
                      | `Continue fld ->
                          nest tree_of_val (depth - 1) fld ty_arg
                      | `Stop result -> result
              in
              (lid, v) :: tree_of_fields false (pos + 1) remainder
        in
        Oval_record (tree_of_fields (pos = 0) pos lbl_list)

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
                if first then tree_of_label env path (Out_name.create name)
                else Oide_ident (Out_name.create name)
              and v =
                match print_sort ld_sort with
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
      and tree_of_val_list start depth obj ty_list =
        let rec tree_list i = function
          | [] -> []
          | (_, Print_as msg) :: ty_list ->
              Oval_stuff msg :: tree_list (i + 1) ty_list
          | (ty, Print_as_value) :: ty_list ->
              let tree = nest tree_of_val (depth - 1) (O.field obj i) ty in
              tree :: tree_list (i + 1) ty_list
        in
      tree_list start ty_list

      and tree_of_generic_array am depth obj ty_arg =
        let obj_block = Obj.Uniform_or_mixed.of_block (O.obj obj) in
        if Obj.Uniform_or_mixed.is_mixed obj_block then
          Oval_stuff "<abstr array>"
        else
          let oval elts = Oval_array (elts, am) in
          let length = O.size obj in
          if length > 0 then
            match check_depth depth obj ty with
              Some x -> x
            | None ->
                let rec tree_of_items tree_list i =
                  if !printer_steps < 0 || depth < 0 then
                    Oval_ellipsis :: tree_list
                  else if i < length then
                    let tree =
                      nest tree_of_val (depth - 1) (O.field obj i) ty_arg
                    in
                    tree_of_items (tree :: tree_list) (i + 1)
                  else tree_list
                in
                oval (List.rev (tree_of_items [] 0))
          else
            oval []

      and tree_of_constr_with_args
             tree_of_cstr cstr_name inlined start depth obj ty_args unboxed =
        let lid = tree_of_cstr (Out_name.create cstr_name) in
        let args =
          if inlined || unboxed then
            match ty_args with
            | [_,Print_as msg] -> [ Oval_stuff msg ]
            | [ty,Print_as_value] -> [ tree_of_val (depth - 1) obj ty ]
            | _ -> assert false
          else
            tree_of_val_list start depth obj ty_args
        in
        Oval_constr (lid, args)

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
            (arg, print_sort ca_sort))
            cstr.cstr_args args
        in
        tree_of_constr_with_args
           (fun x -> Oide_ident x) name (cstr.cstr_inlined <> None)
           1 depth bucket
           args false
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
              fprintf ppf "<internal error: incorrect arity for '%a'>"
                Printtyp.path path in
            Oval_printer printer)


    in nest tree_of_val max_depth obj (Ctype.correct_levels ty)

end
