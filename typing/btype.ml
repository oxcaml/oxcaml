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

(* Basic operations on core types *)

open Asttypes
open Types

open Local_store

(**** Forward declarations ****)

let print_raw =
  ref (fun _ -> assert false : Format.formatter -> type_expr -> unit)

(**** Sets, maps and hashtables of types ****)

let wrap_repr f ty = f (Transient_expr.repr ty)
let wrap_type_expr f tty = f (Transient_expr.type_expr tty)

module TransientTypeSet = Set.Make(TransientTypeOps)
module TypeSet = struct
  include TransientTypeSet
  let add = wrap_repr add
  let mem = wrap_repr mem
  let singleton = wrap_repr singleton
  let exists p = TransientTypeSet.exists (wrap_type_expr p)
  let elements set =
    List.map Transient_expr.type_expr (TransientTypeSet.elements set)
  let debug_print ppf t =
    Format.(
      fprintf ppf "{ %a }"
        (pp_print_seq
           ~pp_sep:(fun ppf () -> fprintf ppf ";@,")
           !print_raw)
        (to_seq t |> Seq.map Transient_expr.type_expr))
end
module TransientTypeMap = Map.Make(TransientTypeOps)
module TypeMap = struct
  include TransientTypeMap
  let add ty = wrap_repr add ty
  let find ty = wrap_repr find ty
  let singleton ty = wrap_repr singleton ty
  let fold f = TransientTypeMap.fold (wrap_type_expr f)
end
module TypeHash = struct
  include TransientTypeHash
  let mem hash = wrap_repr (mem hash)
  let add hash = wrap_repr (add hash)
  let replace hash = wrap_repr (replace hash)
  let remove hash = wrap_repr (remove hash)
  let find hash = wrap_repr (find hash)
  let find_opt hash = wrap_repr (find_opt hash)
  let iter f = TransientTypeHash.iter (wrap_type_expr f)
end
module TransientTypePairs =
  Hashtbl.Make (struct
    type t = transient_expr * transient_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)
module TypePairs = struct
  module H = TransientTypePairs
  open Transient_expr

  type t = {
    set : unit H.t;
    mutable elems : (transient_expr * transient_expr) list;
    (* elems preserves the (reversed) insertion order of elements *)
  }

  let create n =
    { elems = []; set = H.create n }

  let clear t =
    t.elems <- [];
    H.clear t.set

  let repr2 (t1, t2) = (repr t1, repr t2)

  let add t p =
    let p = repr2 p in
    if H.mem t.set p then () else begin
      H.add t.set p ();
      t.elems <- p :: t.elems
    end

  let mem t p = H.mem t.set (repr2 p)

  let iter f t =
    (* iterate in insertion order, not Hashtbl.iter order *)
    List.rev t.elems
    |> List.iter (fun (t1,t2) ->
        f (type_expr t1, type_expr t2))
end


(**** Type level management ****)

let generic_level = Ident.highest_scope
let lowest_level = Ident.lowest_scope

(**** Some type creators ****)

let newgenty desc = newty2 ~level:generic_level desc
let newgenvar ?name jkind = newgenty (Tvar { name; jkind })
let newgenstub ~scope jkind =
  newty3 ~level:generic_level ~scope (Tvar { name=None; jkind })

(**** Check some types ****)

let is_Tvar ty = match get_desc ty with Tvar _ -> true | _ -> false
let is_Tunivar ty = match get_desc ty with Tunivar _ -> true | _ -> false
let is_Tconstr ty = match get_desc ty with Tconstr _ -> true | _ -> false
let is_Tpoly ty = match get_desc ty with Tpoly _ -> true | _ -> false
let type_kind_is_abstract decl =
  match decl.type_kind with Type_abstract _ -> true | _ -> false
let type_origin decl =
  match decl.type_kind with
  | Type_abstract origin -> origin
  | Type_variant _ | Type_record _ | Type_record_unboxed_product _
  | Type_open ->
      Definition

let dummy_method = "*dummy method*"

(**** Representative of a type ****)

let merge_fixed_explanation fixed1 fixed2 =
  match fixed1, fixed2 with
  | Some Univar _ as x, _ | _, (Some Univar _ as x) -> x
  | Some Fixed_private as x, _ | _, (Some Fixed_private as x) -> x
  | Some Reified _ as x, _ | _, (Some Reified _ as x) -> x
  | Some Rigid as x, _ | _, (Some Rigid as x) -> x
  | Some Fixed_existential as x, _ | _, (Some Fixed_existential as x) -> x
  | None, None -> None


let fixed_explanation row =
  match row_fixed row with
  | Some _ as x -> x
  | None ->
      let ty = row_more row in
      match get_desc ty with
      | Tvar _ | Tnil -> None
      | Tunivar _ -> Some (Univar ty)
      | Tconstr (p,_,_) -> Some (Reified p)
      | Tof_kind _ -> Some Fixed_existential
      | _ -> assert false

let is_fixed row = match row_fixed row with
  | None -> false
  | Some _ -> true

let has_fixed_explanation row = fixed_explanation row <> None

let static_row row =
  row_closed row &&
  List.for_all
    (fun (_,f) -> match row_field_repr f with Reither _ -> false | _ -> true)
    (row_fields row)

let tvariant_not_immediate row =
  (* if all labels are devoid of arguments, not a pointer *)
  (* CR layouts v5: Polymorphic variants with all void args can probably
     be immediate, but we don't allow them to have void args right now. *)
  not (row_closed row)
  || List.exists
    (fun (_,field) -> match row_field_repr field with
      | Rpresent (Some _) | Reither (false, _, _) -> true
      | _ -> false)
    (row_fields row)

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let proxy ty =
  match get_desc ty with
  | Tvariant row when not (static_row row) ->
      row_more row
  | Tobject (ty, _) ->
      let rec proxy_obj ty =
        match get_desc ty with
          Tfield (_, _, _, ty) -> proxy_obj ty
        | Tvar _ | Tunivar _ | Tconstr _ -> ty
        | Tnil -> ty
        | _ -> assert false
      in proxy_obj ty
  | _ -> ty

(**** Utilities for fixed row private types ****)

let row_of_type t =
  match get_desc t with
    Tobject(t,_) ->
      let rec get_row t =
        match get_desc t with
          Tfield(_,_,_,t) -> get_row t
        | _ -> t
      in get_row t
  | Tvariant row ->
      row_more row
  | _ ->
      t

let has_constr_row t =
  not (is_Tconstr t) && is_Tconstr (row_of_type t)

let is_row_name s =
  let l = String.length s in
  (* PR#10661: when l=4 and s is "#row", this is not a row name
     but the valid #-type name of a class named "row". *)
  l > 4 && String.sub s (l-4) 4 = "#row"

let is_constr_row ~allow_ident t =
  match get_desc t with
    Tconstr (Path.Pident id, _, _) when allow_ident ->
      is_row_name (Ident.name id)
  | Tconstr (Path.Pdot (_, s), _, _) -> is_row_name s
  | _ -> false

(* TODO: where should this really be *)
(* Set row_name in Env, cf. GPR#1204/1329 *)
let set_static_row_name decl path =
  match decl.type_manifest with
    None -> ()
  | Some ty ->
      match get_desc ty with
        Tvariant row when static_row row ->
          let row =
            set_row_name row (Some (path, decl.type_params)) in
          set_type_desc ty (Tvariant row)
      | _ -> ()

                  (**********************************)
                  (*  Utilities for type traversal  *)
                  (**********************************)

let fold_row f init row =
  let result =
    List.fold_left
      (fun init (_, fi) ->
         match row_field_repr fi with
         | Rpresent(Some ty) -> f init ty
         | Reither(_, tl, _) -> List.fold_left f init tl
         | _ -> init)
      init
      (row_fields row)
  in
  match get_desc (row_more row) with
  | Tvar _ | Tunivar _ | Tsubst _ | Tconstr _ | Tnil
    (* Tof_kind can appear in [row_more] in case the row's row variable was existentially
       quantified in a GADT *)
  | Tof_kind _ ->
    begin match
      Option.map (fun (_,l) -> List.fold_left f result l) (row_name row)
    with
    | None -> result
    | Some result -> result
    end
  | _ -> assert false

let iter_row f row =
  fold_row (fun () v -> f v) () row


let fold_type_expr f init ty =
  match get_desc ty with
    Tvar _              -> init
  | Tarrow (_, ty1, ty2, _) ->
      let result = f init ty1 in
      f result ty2
  | Ttuple l            -> List.fold_left f init (List.map snd l)
  | Tunboxed_tuple l    -> List.fold_left f init (List.map snd l)
  | Tconstr (_, l, _)   -> List.fold_left f init l
  | Tobject(ty, {contents = Some (_, p)}) ->
      let result = f init ty in
      List.fold_left f result p
  | Tobject (ty, _)     -> f init ty
  | Tvariant row        ->
      let result = fold_row f init row in
      f result (row_more row)
  | Tquote ty           -> f init ty
  | Tsplice ty          -> f init ty
  | Tfield (_, _, ty1, ty2) ->
      let result = f init ty1 in
      f result ty2
  | Tnil                -> init
  | Tlink _
  | Tsubst _            -> assert false
  | Tunivar _           -> init
  | Tpoly (ty, tyl)     ->
    let result = f init ty in
    List.fold_left f result tyl
  | Tpackage (_, fl)  ->
    List.fold_left (fun result (_n, ty) -> f result ty) init fl
  | Tof_kind _ -> init

let iter_type_expr f ty =
  fold_type_expr (fun () v -> f v) () ty

let rec iter_abbrev f = function
    Mnil                   -> ()
  | Mcons(_, _, ty, ty', rem) -> f ty; f ty'; iter_abbrev f rem
  | Mlink rem              -> iter_abbrev f !rem

let iter_type_expr_cstr_args f = function
  | Cstr_tuple tl -> List.iter (fun ca -> f ca.ca_type) tl
  | Cstr_record lbls -> List.iter (fun d -> f d.ld_type) lbls

let map_type_expr_cstr_args f = function
  | Cstr_tuple tl -> Cstr_tuple (List.map (fun ca -> {ca with ca_type=f ca.ca_type}) tl)
  | Cstr_record lbls ->
      Cstr_record (List.map (fun d -> {d with ld_type=f d.ld_type}) lbls)

let iter_type_expr_kind f = function
  | Type_abstract _ -> ()
  | Type_variant (cstrs, _, _) ->
      List.iter
        (fun cd ->
           iter_type_expr_cstr_args f cd.cd_args;
           Option.iter f cd.cd_res
        )
        cstrs
  | Type_record(lbls, _, _) ->
      List.iter (fun d -> f d.ld_type) lbls
  | Type_record_unboxed_product(lbls, _, _) ->
      List.iter (fun d -> f d.ld_type) lbls
  | Type_open ->
      ()

                  (**********************************)
                  (*     Utilities for marking      *)
                  (**********************************)

let rec mark_type mark ty =
  if try_mark_node mark ty then iter_type_expr (mark_type mark) ty

let mark_type_params mark ty =
  iter_type_expr (mark_type mark) ty

                  (**********************************)
                  (*  (Object-oriented) iterator    *)
                  (**********************************)

type 'a type_iterators =
  { it_signature: 'a type_iterators -> signature -> unit;
    it_signature_item: 'a type_iterators -> signature_item -> unit;
    it_value_description: 'a type_iterators -> value_description -> unit;
    it_type_declaration: 'a type_iterators -> type_declaration -> unit;
    it_extension_constructor:
        'a type_iterators -> extension_constructor -> unit;
    it_module_declaration: 'a type_iterators -> module_declaration -> unit;
    it_modtype_declaration: 'a type_iterators -> modtype_declaration -> unit;
    it_class_declaration: 'a type_iterators -> class_declaration -> unit;
    it_class_type_declaration:
        'a type_iterators -> class_type_declaration -> unit;
    it_functor_param: 'a type_iterators -> functor_parameter -> unit;
    it_module_type: 'a type_iterators -> module_type -> unit;
    it_class_type: 'a type_iterators -> class_type -> unit;
    it_type_kind: 'a type_iterators -> type_decl_kind -> unit;
    it_do_type_expr: 'a type_iterators -> 'a;
    it_type_expr: 'a type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }

type type_iterators_full = (type_expr -> unit) type_iterators
type type_iterators_without_type_expr = (unit -> unit) type_iterators

let type_iterators_without_type_expr =
  let it_signature it =
    List.iter (it.it_signature_item it)
  and it_signature_item it = function
      Sig_value (_, vd, _)          -> it.it_value_description it vd
    | Sig_type (_, td, _, _)        -> it.it_type_declaration it td
    | Sig_typext (_, td, _, _)      -> it.it_extension_constructor it td
    | Sig_module (_, _, md, _, _)   -> it.it_module_declaration it md
    | Sig_modtype (_, mtd, _)       -> it.it_modtype_declaration it mtd
    | Sig_class (_, cd, _, _)       -> it.it_class_declaration it cd
    | Sig_class_type (_, ctd, _, _) -> it.it_class_type_declaration it ctd
  and it_value_description it vd =
    it.it_type_expr it vd.val_type
  and it_type_declaration it td =
    List.iter (it.it_type_expr it) td.type_params;
    Option.iter (it.it_type_expr it) td.type_manifest;
    Option.iter (it.it_type_declaration it) td.type_unboxed_version;
    it.it_type_kind it td.type_kind
  and it_extension_constructor it td =
    it.it_path td.ext_type_path;
    List.iter (it.it_type_expr it) td.ext_type_params;
    iter_type_expr_cstr_args (it.it_type_expr it) td.ext_args;
    Option.iter (it.it_type_expr it) td.ext_ret_type
  and it_module_declaration it md =
    it.it_module_type it md.md_type
  and it_modtype_declaration it mtd =
    Option.iter (it.it_module_type it) mtd.mtd_type
  and it_class_declaration it cd =
    List.iter (it.it_type_expr it) cd.cty_params;
    it.it_class_type it cd.cty_type;
    Option.iter (it.it_type_expr it) cd.cty_new;
    it.it_path cd.cty_path
  and it_class_type_declaration it ctd =
    List.iter (it.it_type_expr it) ctd.clty_params;
    it.it_class_type it ctd.clty_type;
    it.it_path ctd.clty_path
  and it_functor_param it = function
    | Unit -> ()
    | Named (_, mt, _) -> it.it_module_type it mt
  and it_module_type it = function
      Mty_ident p
    | Mty_alias p -> it.it_path p
    | Mty_signature sg -> it.it_signature it sg
    | Mty_functor (p, mt, _) ->
        it.it_functor_param it p;
        it.it_module_type it mt
    | Mty_strengthen (mty, p, _) ->
        it.it_module_type it mty;
        it.it_path p
  and it_class_type it = function
      Cty_constr (p, tyl, cty) ->
        it.it_path p;
        List.iter (it.it_type_expr it) tyl;
        it.it_class_type it cty
    | Cty_signature cs ->
        it.it_type_expr it cs.csig_self;
        it.it_type_expr it cs.csig_self_row;
        Vars.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_vars;
        Meths.iter (fun _ (_,_,ty) -> it.it_type_expr it ty) cs.csig_meths
    | Cty_arrow  (_, ty, cty) ->
        it.it_type_expr it ty;
        it.it_class_type it cty
  and it_type_kind it kind =
    iter_type_expr_kind (it.it_type_expr it) kind
  and it_path _p = ()
  in
  { it_path; it_type_expr = (fun _ _ -> ()); it_do_type_expr = (fun _ _ -> ());
    it_type_kind; it_class_type; it_functor_param; it_module_type;
    it_signature; it_class_type_declaration; it_class_declaration;
    it_modtype_declaration; it_module_declaration; it_extension_constructor;
    it_type_declaration; it_value_description; it_signature_item; }

let type_iterators mark =
  let it_type_expr it ty =
    if try_mark_node mark ty then it.it_do_type_expr it ty
  and it_do_type_expr it ty =
    iter_type_expr (it.it_type_expr it) ty;
    match get_desc ty with
      Tconstr (p, _, _)
    | Tobject (_, {contents=Some (p, _)})
    | Tpackage (p, _) ->
        it.it_path p
    | Tvariant row ->
        Option.iter (fun (p,_) -> it.it_path p) (row_name row)
    | _ -> ()
  in
  {type_iterators_without_type_expr with it_type_expr; it_do_type_expr}

                  (**********************************)
                  (*  Utilities for copying         *)
                  (**********************************)

let copy_row f fixed row keep more =
  let Row {fields = orig_fields; fixed = orig_fixed; closed; name = orig_name} =
    row_repr row in
  let fields = List.map
      (fun (l, fi) -> l,
        match row_field_repr fi with
        | Rpresent oty -> rf_present (Option.map f oty)
        | Reither(c, tl, m) ->
            let use_ext_of = if keep then Some fi else None in
            let m = if is_fixed row then fixed else m in
            let tl = List.map f tl in
            rf_either tl ?use_ext_of ~no_arg:c ~matched:m
        | Rabsent -> rf_absent)
      orig_fields in
  let name =
    match orig_name with
    | None -> None
    | Some (path, tl) -> Some (path, List.map f tl) in
  let fixed = if fixed then orig_fixed else None in
  create_row ~fields ~more ~fixed ~closed ~name

let copy_commu c = if is_commu_ok c then commu_ok else commu_var ()

let rec copy_type_desc ?(keep_names=false) f = function
    Tvar { jkind; _ } as tv ->
     if keep_names then tv else Tvar { name=None; jkind }
  | Tarrow (p, ty1, ty2, c)-> Tarrow (p, f ty1, f ty2, copy_commu c)
  | Ttuple l            -> Ttuple (List.map (fun (label, t) -> label, f t) l)
  | Tunboxed_tuple l    ->
    Tunboxed_tuple (List.map (fun (label, t) -> label, f t) l)
  | Tconstr (p, l, _)   -> Tconstr (p, List.map f l, ref Mnil)
  | Tobject(ty, {contents = Some (p, tl)})
                        -> Tobject (f ty, ref (Some(p, List.map f tl)))
  | Tobject (ty, _)     -> Tobject (f ty, ref None)
  | Tvariant _          -> assert false (* too ambiguous *)
  | Tquote ty           -> Tquote (f ty)
  | Tsplice ty          -> Tsplice (f ty)
  | Tfield (p, k, ty1, ty2) ->
      Tfield (p, field_kind_internal_repr k, f ty1, f ty2)
      (* the kind is kept shared, with indirections removed for performance *)
  | Tnil                -> Tnil
  | Tlink ty            -> copy_type_desc f (get_desc ty)
  | Tsubst _            -> assert false
  | Tunivar _ as ty     -> ty (* always keep the name *)
  | Tpoly (ty, tyl)     ->
      let tyl = List.map f tyl in
      Tpoly (f ty, tyl)
  | Tpackage (p, fl)  -> Tpackage (p, List.map (fun (n, ty) -> (n, f ty)) fl)
  | Tof_kind jk -> Tof_kind jk

(* TODO: rename to [module Copy_scope] *)
module For_copy : sig
  type copy_scope

  val redirect_desc: copy_scope -> type_expr -> type_desc -> unit

  val with_scope: (copy_scope -> 'a) -> 'a
end = struct
  type copy_scope = {
    mutable saved_desc : (transient_expr * type_desc) list;
    (* Save association of generic nodes with their description. *)
  }

  let redirect_desc copy_scope ty desc =
    let ty = Transient_expr.repr ty in
    copy_scope.saved_desc <- (ty, ty.desc) :: copy_scope.saved_desc;
    Transient_expr.set_desc ty desc

  (* Restore type descriptions. *)
  let cleanup { saved_desc; _ } =
    List.iter (fun (ty, desc) -> Transient_expr.set_desc ty desc) saved_desc

  let with_scope f =
    let scope = { saved_desc = [] } in
    Fun.protect ~finally:(fun () -> cleanup scope) (fun () -> f scope)

end

                  (*******************************************)
                  (*  Memorization of abbreviation expansion *)
                  (*******************************************)

(* Search whether the expansion has been memorized. *)

let lte_public p1 p2 =  (* Private <= Public *)
  match p1, p2 with
  | Private, _ | _, Public -> true
  | Public, Private -> false

let rec find_expans priv p1 = function
    Mnil -> None
  | Mcons (priv', p2, _ty0, ty, _)
    when lte_public priv priv' && Path.same p1 p2 -> Some ty
  | Mcons (_, _, _, _, rem)   -> find_expans priv p1 rem
  | Mlink {contents = rem} -> find_expans priv p1 rem

(* debug: check for cycles in abbreviation. only works with -principal
let rec check_expans visited ty =
  let ty = repr ty in
  assert (not (List.memq ty visited));
  match ty.desc with
    Tconstr (path, args, abbrev) ->
      begin match find_expans path !abbrev with
        Some ty' -> check_expans (ty :: visited) ty'
      | None -> ()
      end
  | _ -> ()
*)

let memo = s_ref []
        (* Contains the list of saved abbreviation expansions. *)

let cleanup_abbrev () =
        (* Remove all memorized abbreviation expansions. *)
  List.iter (fun abbr -> abbr := Mnil) !memo;
  memo := []

let memorize_abbrev mem priv path v v' =
        (* Memorize the expansion of an abbreviation. *)
  mem := Mcons (priv, path, v, v', !mem);
  (* check_expans [] v; *)
  memo := mem :: !memo

let rec forget_abbrev_rec mem path =
  match mem with
    Mnil ->
      mem
  | Mcons (_, path', _, _, rem) when Path.same path path' ->
      rem
  | Mcons (priv, path', v, v', rem) ->
      Mcons (priv, path', v, v', forget_abbrev_rec rem path)
  | Mlink mem' ->
      mem' := forget_abbrev_rec !mem' path;
      raise Exit

let forget_abbrev mem path =
  try mem := forget_abbrev_rec !mem path with Exit -> ()

(* debug: check for invalid abbreviations
let rec check_abbrev_rec = function
    Mnil -> true
  | Mcons (_, ty1, ty2, rem) ->
      repr ty1 != repr ty2
  | Mlink mem' ->
      check_abbrev_rec !mem'

let check_memorized_abbrevs () =
  List.for_all (fun mem -> check_abbrev_rec !mem) !memo
*)

(* Re-export backtrack *)

let snapshot = snapshot
let backtrack = backtrack ~cleanup_abbrev

                  (**********************************)
                  (*  Utilities for labels          *)
                  (**********************************)

let is_optional_parsetree : Parsetree.arg_label -> bool = function
    Optional _ -> true
  | _ -> false

let is_optional = function Optional _ -> true | _ -> false

let is_position = function Position _ -> true | _ -> false

let is_omittable = function
  Optional _
| Position _ -> true
| Nolabel | Labelled _ -> false

let label_name = function
    Nolabel -> ""
  | Labelled s
  | Optional s
  | Position s -> s

let prefixed_label_name = function
    Nolabel -> ""
  | Labelled s | Position s -> "~" ^ s
  | Optional s -> "?" ^ s

let rec extract_label_aux hd l = function
  | [] -> None
  | (l',t as p) :: ls ->
      if label_name l' = l then
        Some (l', t, hd <> [], List.rev_append hd ls)
      else
        extract_label_aux (p::hd) l ls

let extract_label l ls = extract_label_aux [] l ls

                              (*******************************)
                              (*  Operations on class types  *)
                              (*******************************)

let rec signature_of_class_type =
  function
    Cty_constr (_, _, cty) -> signature_of_class_type cty
  | Cty_signature sign     -> sign
  | Cty_arrow (_, _, cty)   -> signature_of_class_type cty


let rec class_body cty =
  match cty with
    Cty_constr _ ->
      cty (* Only class bodies can be abbreviated *)
  | Cty_signature _ ->
      cty
  | Cty_arrow (_, _, cty) ->
      class_body cty

(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Cty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

let rec class_type_arity =
  function
    Cty_constr (_, _, cty) ->  class_type_arity cty
  | Cty_signature _        ->  0
  | Cty_arrow (_, _, cty)    ->  1 + class_type_arity cty

let rec abbreviate_class_type path params cty =
  match cty with
    Cty_constr (_, _, _) | Cty_signature _ ->
      Cty_constr (path, params, cty)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, ty, abbreviate_class_type path params cty)

let self_type cty =
  (signature_of_class_type cty).csig_self

let self_type_row cty =
  (signature_of_class_type cty).csig_self_row

(* Return the methods of a class signature *)
let methods sign =
  Meths.fold
    (fun name _ l -> name :: l)
    sign.csig_meths []

(* Return the virtual methods of a class signature *)
let virtual_methods sign =
  Meths.fold
    (fun name (_priv, vr, _ty) l ->
       match vr with
       | Virtual -> name :: l
       | Concrete -> l)
    sign.csig_meths []

(* Return the concrete methods of a class signature *)
let concrete_methods sign =
  Meths.fold
    (fun name (_priv, vr, _ty) s ->
       match vr with
       | Virtual -> s
       | Concrete -> MethSet.add name s)
    sign.csig_meths MethSet.empty

(* Return the public methods of a class signature *)
let public_methods sign =
  Meths.fold
    (fun name (priv, _vr, _ty) l ->
       match priv with
       | Mprivate _ -> l
       | Mpublic -> name :: l)
    sign.csig_meths []

(* Return the instance variables of a class signature *)
let instance_vars sign =
  Vars.fold
    (fun name _ l -> name :: l)
    sign.csig_vars []

(* Return the virtual instance variables of a class signature *)
let virtual_instance_vars sign =
  Vars.fold
    (fun name (_mut, vr, _ty) l ->
       match vr with
       | Virtual -> name :: l
       | Concrete -> l)
    sign.csig_vars []

(* Return the concrete instance variables of a class signature *)
let concrete_instance_vars sign =
  Vars.fold
    (fun name (_mut, vr, _ty) s ->
       match vr with
       | Virtual -> s
       | Concrete -> VarSet.add name s)
    sign.csig_vars VarSet.empty

let method_type label sign =
  match Meths.find label sign.csig_meths with
  | (_, _, ty) -> ty
  | exception Not_found -> assert false

let instance_variable_type label sign =
  match Vars.find label sign.csig_vars with
  | (_, _, ty) -> ty
  | exception Not_found -> assert false

                  (********************************)
                  (*  Utilities for poly types    *)
                  (********************************)

let tpoly_is_mono ty =
  match get_desc ty with
  | Tpoly(_, []) -> true
  | Tpoly(_, _ :: _) -> false
  | _ -> assert false

let tpoly_get_poly ty =
  match get_desc ty with
  | Tpoly(ty, vars) -> (ty, vars)
  | _ -> assert false

let tpoly_get_mono ty =
  match get_desc ty with
  | Tpoly(ty, []) -> ty
  | _ -> assert false

                  (**********)
                  (*  Misc  *)
                  (**********)

(**** Type information getter ****)

let cstr_type_path cstr =
  match get_desc cstr.cstr_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false

                  (************)
                  (*  Jkinds  *)
                  (************)

module Jkind0 = struct
  open Allowance

  module Mod_bounds = struct
    module Crossing = Mode.Crossing
    module Externality = Jkind_axis.Externality
    module Nullability = Jkind_axis.Nullability
    module Separability = Jkind_axis.Separability

    type t = mod_bounds =
      { crossing : Mode.Crossing.t;
        externality: Jkind_axis.Externality.t;
        nullability: Jkind_axis.Nullability.t;
        separability: Jkind_axis.Separability.t;
      }

    let crossing t = t.crossing

    let[@inline] modal ax t =
      t |> crossing |> (Crossing.proj [@inlined hint]) ax
    let areality = Crossing.Axis.Comonadic Areality
    let linearity = Crossing.Axis.Comonadic Linearity
    let uniqueness = Crossing.Axis.Monadic Uniqueness
    let portability = Crossing.Axis.Comonadic Portability
    let contention = Crossing.Axis.Monadic Contention
    let forkable = Crossing.Axis.Comonadic Forkable
    let yielding = Crossing.Axis.Comonadic Yielding
    let statefulness = Crossing.Axis.Comonadic Statefulness
    let visibility = Crossing.Axis.Monadic Visibility
    let staticity = Crossing.Axis.Monadic Staticity
    let[@inline] externality t = t.externality
    let[@inline] nullability t = t.nullability
    let[@inline] separability t = t.separability

    let[@inline] create
        crossing
        ~externality
        ~nullability
        ~separability =
      {
        crossing;
        externality;
        nullability;
        separability;
      }

    let[@inline] set_crossing crossing t = { t with crossing }
    let[@inline] set_externality externality t = { t with externality }
    let[@inline] set_nullability nullability t = { t with nullability }
    let[@inline] set_separability separability t = { t with separability }

    let[@inline] set_max_in_set t max_axes =
      let open Jkind_axis.Axis_set in
      let[@inline] modal ax =
        if mem max_axes (Modal ax)
        then (Crossing.Per_axis.max [@inlined hint]) ax
        else modal ax t
      in
      (* a little optimization *)
      if is_empty max_axes then t else
      let regionality = modal areality in
      let linearity = modal linearity in
      let uniqueness = modal uniqueness in
      let portability = modal portability in
      let contention = modal contention in
      let forkable = modal forkable in
      let yielding = modal yielding in
      let statefulness = modal statefulness in
      let visibility = modal visibility in
      let staticity = modal staticity in
      let externality =
        if mem max_axes (Nonmodal Externality)
        then Externality.max
        else t.externality
      in
      let nullability =
        if mem max_axes (Nonmodal Nullability)
        then Nullability.max
        else t.nullability
      in
      let separability =
        if mem max_axes (Nonmodal Separability)
        then Separability.max
        else t.separability
      in
      let monadic =
        Crossing.Monadic.create ~uniqueness ~contention ~visibility ~staticity
      in
      let comonadic =
        Crossing.Comonadic.create ~regionality ~linearity ~portability ~yielding
          ~forkable ~statefulness
      in
      let crossing : Mode.Crossing.t = { monadic; comonadic } in
      {
        crossing;
        externality;
        nullability;
        separability;
      }

    let[@inline] set_min_in_set t min_axes =
      let open Jkind_axis.Axis_set in
      let modal ax =
        if mem min_axes (Modal ax)
        then (Crossing.Per_axis.min [@inlined hint]) ax
        else modal ax t
      in
      (* a little optimization *)
      if is_empty min_axes then t else
      let regionality = modal areality in
      let linearity = modal linearity in
      let uniqueness = modal uniqueness in
      let portability = modal portability in
      let contention = modal contention in
      let forkable = modal forkable in
      let yielding = modal yielding in
      let statefulness = modal statefulness in
      let visibility = modal visibility in
      let staticity = modal staticity in
      let externality =
        if mem min_axes (Nonmodal Externality)
        then Externality.min
        else t.externality
      in
      let nullability =
        if mem min_axes (Nonmodal Nullability)
        then Nullability.min
        else t.nullability
      in
      let separability =
        if mem min_axes (Nonmodal Separability)
        then Separability.min
        else t.separability
      in
      let monadic =
        Crossing.Monadic.create ~uniqueness ~contention ~visibility ~staticity
      in
      let comonadic =
        Crossing.Comonadic.create ~regionality ~linearity ~portability ~yielding
          ~forkable ~statefulness
      in
      let crossing : Mode.Crossing.t = { monadic; comonadic } in
      {
        crossing;
        externality;
        nullability;
        separability;
      }

    let[@inline] is_max_within_set t axes =
      let open Jkind_axis.Axis_set in
      let modal ax =
        not (mem axes (Modal ax)) ||
        Crossing.Per_axis.((le [@inlined hint]) ax ((max [@inlined hint]) ax)
          (modal ax t))
      in
      modal areality &&
      modal linearity &&
      modal uniqueness &&
      modal portability &&
      modal contention &&
      modal forkable &&
      modal yielding &&
      modal statefulness &&
      modal visibility &&
      modal staticity &&
      (not (mem axes (Nonmodal Externality)) ||
       Externality.(le max (externality t))) &&
      (not (mem axes (Nonmodal Nullability)) ||
       Nullability.(le max (nullability t))) &&
      (not (mem axes (Nonmodal Separability)) ||
       Separability.(le max (separability t)))

    let min =
      create Crossing.min ~externality:Externality.min
        ~nullability:Nullability.min ~separability:Separability.min

    let max =
      create Crossing.max ~externality:Externality.max
        ~nullability:Nullability.max ~separability:Separability.max

    let[@inline] is_max m = m = max

    let for_arrow =
      let crossing =
        Crossing.create ~linearity:false ~regionality:false ~uniqueness:true
          ~portability:false ~contention:true ~forkable:false ~yielding:false
          ~statefulness:false ~visibility:true ~staticity:false
      in
      create crossing ~externality:Externality.max
        ~nullability:Nullability.Non_null ~separability:Separability.Non_float

    let debug_print ppf
          { crossing;
            externality;
            nullability;
            separability } =
      Format.fprintf ppf "@[{ crossing = %a;@ externality = %a;@ \
        nullability = %a;@ separability = %a }@]"
        Crossing.print crossing
        Externality.print externality
        Nullability.print nullability
        Separability.print separability

    let equal t1 t2 =
      Misc.Le_result.equal ~le:Crossing.le (crossing t1) (crossing t2)
      && Externality.equal (externality t1) (externality t2)
      && Nullability.equal (nullability t1) (nullability t2)
      && Separability.equal (separability t1) (separability t2)

    let join t1 t2 =
      let crossing = Crossing.join (crossing t1) (crossing t2) in
      let externality = Externality.join (externality t1) (externality t2) in
      let nullability = Nullability.join (nullability t1) (nullability t2) in
      let separability =
        Separability.join (separability t1) (separability t2)
      in
      create crossing ~externality ~nullability ~separability

    (* Returns the set of axes that is relevant under a given modality. For
       example, under the [global] modality, the areality axis is *not*
       relevant. *)
    let relevant_axes_of_modality ~relevant_for_shallow ~modality =
      Jkind_axis.Axis_set.create ~f:(fun ~axis:(Pack axis) ->
        match axis with
        | Modal axis ->
          let (P axis) = P axis |> Mode.Crossing.Axis.to_modality in
          let modality = Mode.Modality.Const.proj axis modality in
           not (Mode.Modality.Per_axis.is_constant axis modality)
        (* The kind-inference.md document (in the repo) discusses both constant
           modalities and identity modalities. Of course, reality has modalities
           (such as [shared]) that are neither constants nor identities. Here,
           we treat all non-constant modalities the way that the design treats
           identity modalities. This is safe, because it leads to a minimum of
           mode-crossing. In the future, we may want to complexify the
           modal-kinds setup to allow for more mode-crossing in the presence of
           non-constant non-identity modalities. *)
        | Nonmodal Externality -> true
        | Nonmodal Nullability -> (
          match relevant_for_shallow with
          | `Relevant -> true
          | `Irrelevant -> false)
        | Nonmodal Separability -> (
          match relevant_for_shallow with
          | `Relevant -> true
          | `Irrelevant -> false))
  end

  module Quality = struct
    include Allowance.Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd jkind_quality constraint 'd = 'l * 'r

      let disallow_left :
          type l r. (l * r) jkind_quality -> (disallowed * r) jkind_quality =
        function
        | Not_best -> Not_best
        | Best -> Best

      let disallow_right :
          type l r. (l * r) jkind_quality -> (l * disallowed) jkind_quality =
        function
        | Not_best -> Not_best
        | Best -> Best

      let allow_left :
          type l r. (allowed * r) jkind_quality -> (l * r) jkind_quality =
        function
        | Not_best -> Not_best
        | Best -> Best

      let allow_right :
          type l r. (l * allowed) jkind_quality -> (l * r) jkind_quality =
        function
        | Not_best -> Not_best
    end)

    let try_allow_r :
        type l r. (l * r) jkind_quality -> (l * allowed) jkind_quality option =
      function
      | Not_best -> Some Not_best
      | Best -> None
  end

  module With_bounds = struct
    type 'd t = 'd with_bounds constraint 'd = 'l * 'r

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

      let disallow_left : type l r. (l * r) t -> (disallowed * r) t = function
        | No_with_bounds -> No_with_bounds
        | With_bounds _ as b -> b

      let disallow_right : type l r. (l * r) t -> (l * disallowed) t = function
        | No_with_bounds -> No_with_bounds
        | With_bounds _ as b -> b

      let allow_left : type l r. (allowed * r) t -> (l * r) t = function
        | No_with_bounds -> No_with_bounds
        | With_bounds _ as b -> b

      let allow_right : type l r. (l * allowed) t -> (l * r) t = function
        | No_with_bounds -> No_with_bounds
    end)

    let try_allow_l : type l r. (l * r) t -> (allowed * r) t option = function
      | No_with_bounds -> Some No_with_bounds
      | With_bounds _ as b -> Some b

    let try_allow_r : type l r. (l * r) t -> (l * allowed) t option = function
      | No_with_bounds -> Some No_with_bounds
      | With_bounds _ -> None

    let map_type_expr (type l r) f : (l * r) t -> (l * r) t = function
      | No_with_bounds -> No_with_bounds
      | With_bounds tys ->
        With_bounds (With_bounds_types.map_with_key (fun ty ti -> f ty, ti) tys)

    let add_bound type_expr type_info tys =
      With_bounds_types.update type_expr
        (function
          | None -> Some type_info
          | Some ti -> Some (With_bounds_type_info.join ti type_info))
        tys

    let add_modality ~relevant_for_shallow ~modality ~type_expr
        (t : (allowed * 'r) t) : (allowed * 'r) t =
      let relevant_axes =
        Mod_bounds.relevant_axes_of_modality ~relevant_for_shallow ~modality
      in
      match t with
      | No_with_bounds ->
        With_bounds
          (With_bounds_types.singleton type_expr
             ({ relevant_axes } : With_bounds_type_info.t))
      | With_bounds tys ->
        With_bounds (add_bound type_expr { relevant_axes } tys)
  end

  module Layout_and_axes = struct
    module Allow_disallow = Allowance.Magic_allow_disallow (struct
      type (_, 'layout, 'd) sided = ('layout, 'd) layout_and_axes

      let disallow_left t =
        { t with with_bounds = With_bounds.disallow_left t.with_bounds }

      let disallow_right t =
        { t with with_bounds = With_bounds.disallow_right t.with_bounds }

      let allow_left t =
        { t with with_bounds = With_bounds.allow_left t.with_bounds }

      let allow_right t =
        { t with with_bounds = With_bounds.allow_right t.with_bounds }
    end)

    include Allow_disallow

    let map f t = { t with layout = f t.layout }

    let map_option f t =
      match f t.layout with None -> None | Some layout -> Some { t with layout }

    let map_type_expr f t =
      { t with with_bounds = With_bounds.map_type_expr f t.with_bounds }

    let try_allow_l :
        type l r.
        ('layout, l * r) layout_and_axes ->
        ('layout, Allowance.allowed * r) layout_and_axes option =
     fun { layout; mod_bounds; with_bounds } ->
      match With_bounds.try_allow_l with_bounds with
      | None -> None
      | Some with_bounds ->
        Some { layout; mod_bounds = Obj.magic mod_bounds; with_bounds }

    let try_allow_r { layout; mod_bounds; with_bounds } =
      match With_bounds.try_allow_r with_bounds with
      | Some with_bounds ->
        Some { layout; mod_bounds = Obj.magic mod_bounds; with_bounds }
      | None -> None
  end

  module Const = struct
    type 'd t = (Jkind_types.Layout.Const.t, 'd) layout_and_axes

    include Allowance.Magic_allow_disallow (struct
      include Layout_and_axes.Allow_disallow

      type (_, _, 'd) sided = 'd t
    end)

    let max =
      { layout = Jkind_types.Layout.Const.max;
        mod_bounds = Mod_bounds.max;
        with_bounds = No_with_bounds
      }

    (* This function is shallow in the sense that it does not expand abstract
       kinds. That's fine for the places where it is used (printing and a memo
       table), but it is not semantic equality.  This function _must not_ expand
       abstract kinds, because doing so would require a dependency on [Env],
       which creates a cycle. *)
    let shallow_no_with_bounds_and_equal t1 t2 =
      let open Misc.Stdlib.Monad.Option.Syntax in
      let t1_t2 =
        let* t1 = Layout_and_axes.try_allow_l t1 in
        let* t1 = Layout_and_axes.try_allow_r t1 in
        let* t2 = Layout_and_axes.try_allow_l t2 in
        let* t2 = Layout_and_axes.try_allow_r t2 in
        Some (t1, t2)
      in
      match t1_t2 with
      | Some (t1, t2) ->
        Jkind_types.Layout.Const.equal t1.layout t2.layout
        && Mod_bounds.equal t1.mod_bounds t2.mod_bounds
      | None -> false

    (* CR layouts: Remove this once we have a better story for printing with
       jkind abbreviations. *)
    module Builtin = struct
      type nonrec t =
        { jkind : (allowed * allowed) t;
          name : string
        }

      (* Mode crossing that crosses everything except staticity *)
      let cross_all_except_staticity =
        let ax : _ Mode.Crossing.Axis.t = Monadic Staticity in
        Mode.Crossing.(set ax (Per_axis.max ax) min)

      let mk_jkind ~crossing ~nullability ~separability ~externality
          (layout : Jkind_types.Layout.Const.t) =
        let mod_bounds =
          Mod_bounds.create crossing ~nullability ~separability ~externality
        in
        { layout; mod_bounds; with_bounds = No_with_bounds }

      let any =
        { jkind =
            mk_jkind Any ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max
              ~nullability:Maybe_null ~separability:Maybe_separable;
          name = "any"
        }

      let any_mod_everything =
        { jkind =
            mk_jkind Any ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min
              ~nullability:Maybe_null
              ~separability:Maybe_separable;
          name = "any mod everything"
        }

      let value_or_null =
        { jkind =
            mk_jkind (Base Value) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max
              ~nullability:Maybe_null
              ~separability:Maybe_separable;
          name = "value_or_null"
        }

      let value_or_null_mod_everything =
        { jkind =
            mk_jkind (Base Value) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min
              ~nullability:Maybe_null
              ~separability:Maybe_separable;
          name = "value_or_null mod everything"
        }

      let value =
        { jkind =
            mk_jkind (Base Value) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max
              ~nullability:Non_null
              ~separability:Separable;
          name = "value"
        }

      let immutable_data =
        let open Mod_bounds in
        { jkind =
            { layout = Base Value;
              mod_bounds =
                (let crossing =
                   Mode.Crossing.create ~regionality:false ~linearity:true
                     ~portability:true ~forkable:true ~yielding:true
                     ~uniqueness:false ~contention:true ~statefulness:true
                     ~visibility:true ~staticity:false
                 in
                 create crossing ~externality:Externality.max
                   ~nullability:Nullability.Non_null
                   ~separability:Separability.Non_float);
              with_bounds = No_with_bounds
            };
          name = "immutable_data"
        }

      let exn =
        let open Mod_bounds in
        { jkind =
            { layout = Base Value;
              mod_bounds =
                (let crossing =
                   Crossing.create ~regionality:false ~linearity:false
                     ~portability:true ~forkable:false ~yielding:false
                     ~uniqueness:false ~contention:true ~statefulness:true
                     ~visibility:true ~staticity:false
                 in
                 create crossing ~externality:Externality.max
                   ~nullability:Nullability.Non_null
                   ~separability:Separability.Non_float);
              with_bounds = No_with_bounds
            };
          name = "exn"
        }

      let sync_data =
        let open Mod_bounds in
        { jkind =
            { layout = Base Value;
              mod_bounds =
                (let crossing =
                   Mode.Crossing.create ~regionality:false ~linearity:true
                     ~portability:true ~forkable:true ~yielding:true
                     ~uniqueness:false ~contention:true ~statefulness:true
                     ~visibility:false ~staticity:false
                 in
                 create crossing ~externality:Externality.max
                   ~nullability:Nullability.Non_null
                   ~separability:Separability.Non_float);
              with_bounds = No_with_bounds
            };
          name = "sync_data"
        }

      let mutable_data =
        let open Mod_bounds in
        { jkind =
            { layout = Base Value;
              mod_bounds =
                (let crossing =
                   Crossing.create ~regionality:false ~linearity:true
                     ~portability:true ~forkable:true ~yielding:true
                     ~contention:false ~uniqueness:false ~statefulness:true
                     ~visibility:false ~staticity:false
                 in
                 create crossing ~externality:Externality.max
                   ~nullability:Nullability.Non_null
                   ~separability:Separability.Non_float);
              with_bounds = No_with_bounds
            };
          name = "mutable_data"
        }

      let void =
        { jkind =
            mk_jkind (Base Void) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "void"
        }

      let void_mod_everything =
        { jkind =
            mk_jkind (Base Void) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "void mod everything"
        }

      let immediate =
        { jkind =
            mk_jkind (Base Value) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "immediate"
        }

      let immediate_or_null =
        { jkind =
            mk_jkind (Base Value) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min
              ~nullability:Maybe_null
              ~separability:Non_float;
          name = "immediate_or_null"
        }

      (* [immediate64] describes types that are stored directly (no indirection)
         on 64-bit platforms but indirectly on 32-bit platforms. The key
         question: along which modes should a [immediate64] cross? As of today,
         all of them, but the reasoning for each is independent and somewhat
         subtle:

         * Areality: This is fine, because we do not have stack-allocation on
         32-bit platforms. Thus mode-crossing is sound at any type on 32-bit,
         including immediate64 types.

         * Linearity: This is fine, because linearity matters only for function
         types, and an immediate64 cannot be a function type and cannot store
         one either.

         * Uniqueness: This is fine, because uniqueness matters only for
         in-place update, and no record supporting in-place update is an
         immediate64. ([@@unboxed] records do not support in-place update.)

         * Syncness: This is fine, because syncness matters only for function
         types, and an immediate64 cannot be a function type and cannot store
         one either.

         * Contention: This is fine, because contention matters only for
         types with mutable fields, and an immediate64 does not have immutable
         fields.

         In practice, the functor that creates immediate64s,
         [Stdlib.Sys.Immediate64.Make], will require these conditions on its
         argument. But the arguments that we expect here will have no trouble
         meeting the conditions.
      *)
      let immediate64 =
        let open Mod_bounds in
        { jkind =
            { immediate.jkind with
              mod_bounds =
                set_externality External64 immediate.jkind.mod_bounds
            };
          name = "immediate64"
        }

      let immediate64_or_null =
        let open Mod_bounds in
        { jkind =
            { immediate_or_null.jkind with
              mod_bounds =
                set_externality External64 immediate_or_null.jkind.mod_bounds
            };
          name = "immediate64_or_null"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let float64 =
        { jkind =
            mk_jkind (Base Float64) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          (* [separability] is intentionally [Non_float]:
             only boxed floats are relevant for separability. *)
          name = "float64"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_float =
        { jkind =
            mk_jkind (Base Float64) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          (* [separability] is intentionally [Non_float]:
             only boxed floats are relevant for separability. *)
          name = "float64 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let float32 =
        { jkind =
            mk_jkind (Base Float32) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          (* [separability] is intentionally [Non_float]:
             only boxed floats are relevant for separability. *)
          name = "float32"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_float32 =
        { jkind =
            mk_jkind (Base Float32) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          (* [separability] is intentionally [Non_float]:
             only boxed floats are relevant for separability. *)
          name = "float32 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let word =
        { jkind =
            mk_jkind (Base Word) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "word"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_nativeint =
        { jkind =
            mk_jkind (Base Word) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "word mod everything"
        }

      let untagged_immediate =
        { jkind =
            mk_jkind (Base Untagged_immediate) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "untagged_immediate"
        }

      let kind_of_untagged_immediate =
        { jkind =
            mk_jkind (Base Untagged_immediate)
              ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min
              ~nullability:Non_null ~separability:Non_float;
          name = "untagged_immediate mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let bits8 =
        { jkind =
            mk_jkind (Base Bits8) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "bits8"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_int8 =
        { jkind =
            mk_jkind (Base Bits8) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "bits8 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let bits16 =
        { jkind =
            mk_jkind (Base Bits16) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "bits16"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_int16 =
        { jkind =
            mk_jkind (Base Bits16) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "bits16 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let bits32 =
        { jkind =
            mk_jkind (Base Bits32) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "bits32"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_int32 =
        { jkind =
            mk_jkind (Base Bits32) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "bits32 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let bits64 =
        { jkind =
            mk_jkind (Base Bits64) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "bits64"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_int64 =
        { jkind =
            mk_jkind (Base Bits64) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "bits64 mod everything"
        }

      let kind_of_idx =
        { jkind =
            mk_jkind (Base Bits64) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "bits64 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let vec128 =
        { jkind =
            mk_jkind (Base Vec128) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "vec128"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let vec256 =
        { jkind =
            mk_jkind (Base Vec256) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "vec256"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let vec512 =
        { jkind =
            mk_jkind (Base Vec512) ~crossing:Mode.Crossing.max
              ~externality:Mod_bounds.Externality.max ~nullability:Non_null
              ~separability:Non_float;
          name = "vec512"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_128bit_vectors =
        { jkind =
            mk_jkind (Base Vec128) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "vec128 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_256bit_vectors =
        { jkind =
            mk_jkind (Base Vec256) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "vec256 mod everything"
        }

      (* CR or_null: nullability here should be [Maybe_null], but is set
         to [Non_null] for now due to inference limitations. *)
      let kind_of_unboxed_512bit_vectors =
        { jkind =
            mk_jkind (Base Vec512) ~crossing:cross_all_except_staticity
              ~externality:Mod_bounds.Externality.min ~nullability:Non_null
              ~separability:Non_float;
          name = "vec512 mod everything"
        }

      let all =
        [ any;
          any_mod_everything;
          value_or_null;
          value_or_null_mod_everything;
          value;
          immutable_data;
          sync_data;
          mutable_data;
          void;
          void_mod_everything;
          immediate;
          immediate_or_null;
          immediate64;
          immediate64_or_null;
          float64;
          kind_of_unboxed_float;
          float32;
          kind_of_unboxed_float32;
          word;
          kind_of_unboxed_nativeint;
          bits8;
          kind_of_unboxed_int8;
          bits16;
          kind_of_unboxed_int16;
          bits32;
          kind_of_unboxed_int32;
          bits64;
          kind_of_unboxed_int64;
          vec128;
          kind_of_unboxed_128bit_vectors;
          vec256;
          kind_of_unboxed_256bit_vectors;
          vec512;
          kind_of_unboxed_512bit_vectors ]

      let of_attribute : Builtin_attributes.jkind_attribute -> t = function
        | Immediate -> immediate
        | Immediate64 -> immediate64
    end
  end

  module Jkind_desc = struct
    let of_const t = Layout_and_axes.map Jkind_types.Layout.of_const t

    let max = of_const Const.max

    let map_type_expr f t = Layout_and_axes.map_type_expr f t

    let add_with_bounds ~relevant_for_shallow ~type_expr ~modality t =
      match get_desc type_expr with
      | Tarrow (_, _, _, _) ->
        (* Optimization: all arrow types have the same (with-bound-free) jkind,
           so we can just eagerly do a join on the mod-bounds here rather than
           having to add them to our with bounds only to be normalized away
           later. *)
        { t with
          mod_bounds =
            Mod_bounds.join t.mod_bounds
              (Mod_bounds.set_min_in_set Mod_bounds.for_arrow
                 (Jkind_axis.Axis_set.complement
                    (Mod_bounds.relevant_axes_of_modality ~modality
                       ~relevant_for_shallow)))
        }
      | _ ->
        { t with
          with_bounds =
            With_bounds.add_modality ~relevant_for_shallow ~type_expr ~modality
              t.with_bounds
        }

    module Builtin = struct
      let any = max

      let value_or_null = of_const Const.Builtin.value_or_null.jkind

      let value = of_const Const.Builtin.value.jkind

      let immutable_data = of_const Const.Builtin.immutable_data.jkind

      let sync_data = of_const Const.Builtin.sync_data.jkind

      let mutable_data = of_const Const.Builtin.mutable_data.jkind

      let void = of_const Const.Builtin.void.jkind

      let immediate = of_const Const.Builtin.immediate.jkind

      let immediate_or_null =
        of_const Const.Builtin.immediate_or_null.jkind
    end

    let product tys_modalities layouts =
      let layout = Jkind_types.Layout.product layouts in
      let relevant_for_shallow =
        (* Shallow axes like nullability or separability are relevant for
           1-field unboxed records and irrelevant for everything else. *)
        match List.length layouts with 1 -> `Relevant | _ -> `Irrelevant
      in
      let mod_bounds = Mod_bounds.min in
      let with_bounds =
        List.fold_right
          (fun (type_expr, modality) bounds ->
            With_bounds.add_modality ~relevant_for_shallow ~type_expr ~modality
              bounds)
          tys_modalities No_with_bounds
      in
      { layout; mod_bounds; with_bounds }

    let get_const t =
      Layout_and_axes.map_option Jkind_types.Layout.get_const t
  end

  module Violation = struct
    module Sub_failure_reason = struct
      type t =
        | Axis_disagreement of Jkind_axis.Axis.packed
        | Layout_disagreement
        | Constrain_ran_out_of_fuel
    end

    type violation =
      | Not_a_subjkind :
          (allowed * 'r1) jkind * ('l * 'r2) jkind * Sub_failure_reason.t list
          -> violation
      | No_intersection : 'd jkind * ('l * allowed) jkind -> violation

    type nonrec t =
      { violation : violation;
        missing_cmi : Path.t option
      }
    (* [missing_cmi]: is this error a result of a missing cmi file?
       This is stored separately from the [violation] because it's
       used to change the behavior of [value_kind], and we don't
       want that function to inspect something that is purely about
       the choice of error message. (Though the [Path.t] payload *is*
       indeed just about the payload.) *)
  end

  module Jkind = struct
    include Allowance.Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd jkind

      let disallow_right t =
        { t with
          jkind = Layout_and_axes.disallow_right t.jkind;
          quality = Quality.disallow_right t.quality
        }

      let disallow_left t =
        { t with
          jkind = Layout_and_axes.disallow_left t.jkind;
          quality = Quality.disallow_left t.quality
        }

      let allow_right t =
        { t with
          jkind = Layout_and_axes.allow_right t.jkind;
          quality = Quality.allow_right t.quality
        }

      let allow_left t =
        { t with
          jkind = Layout_and_axes.allow_left t.jkind;
          quality = Quality.allow_left t.quality
        }
    end)

    let combine_mutability mut1 mut2 =
      match mut1, mut2 with
      | (Mutable { atomic = Nonatomic; mode = _ } as x), _
      | _, (Mutable { atomic = Nonatomic; mode = _ } as x) ->
        x
      | (Mutable { atomic = Atomic; mode = _ } as x), _
      | _, (Mutable { atomic = Atomic; mode = _ } as x) ->
        x
      | (Immutable as x), Immutable -> x

    let try_allow_r t =
      let open Misc.Stdlib.Monad.Option.Syntax in
      let* jkind = Layout_and_axes.try_allow_r t.jkind in
      let* quality = Quality.try_allow_r t.quality in
      Some { t with jkind; quality }

    let fresh_jkind jkind ~annotation ~why =
      { jkind;
        annotation;
        history = Creation why;
        has_warned = false;
        ran_out_of_fuel_during_normalize = false;
        quality = Not_best
      }
      |> allow_left |> allow_right

     (* This version propagates the allowances from the [jkind] to the
        output. *)
     let fresh_jkind_poly jkind ~annotation ~why =
       { jkind;
         annotation;
         history = Creation why;
         has_warned = false;
         ran_out_of_fuel_during_normalize = false;
         quality = Not_best
       }

    (* every context where this is used actually wants an [option] *)
    let mk_annot name =
      Some
        Parsetree.
          { pjkind_loc = Location.none; pjkind_desc = Pjk_abbreviation name }

    let mark_best (type l r) (t : (l * r) jkind) =
      { (disallow_right t) with quality = Best }

    module Builtin = struct
      let any_dummy_jkind =
        { jkind = Jkind_desc.max;
          annotation = None;
          (* this should never get printed: it's a dummy *)
          history = Creation (Any_creation Dummy_jkind);
          has_warned = false;
          ran_out_of_fuel_during_normalize = false;
          quality = Not_best
        }

      (* CR layouts: Should we be doing more memoization here? *)
      let any ~(why : Jkind_intf.History.any_creation_reason) =
        match why with
        | Dummy_jkind ->
          any_dummy_jkind (* share this one common case *) |> allow_left
          |> allow_right
        | _ ->
          fresh_jkind Jkind_desc.Builtin.any
            ~annotation:(mk_annot "any") ~why:(Any_creation why)

      let value_v1_safety_check =
        { jkind = Jkind_desc.Builtin.value_or_null;
          annotation = mk_annot "value";
          history = Creation (Value_or_null_creation V1_safety_check);
          has_warned = false;
          ran_out_of_fuel_during_normalize = false;
          quality = Not_best
        }

      let void ~why =
        fresh_jkind Jkind_desc.Builtin.void ~annotation:(mk_annot "void")
          ~why:(Void_creation why)
        |> mark_best

      let value_or_null ~why =
        match (why : Jkind_intf.History.value_or_null_creation_reason) with
        | V1_safety_check -> value_v1_safety_check |> allow_left |> allow_right
        | _ ->
          fresh_jkind Jkind_desc.Builtin.value_or_null
            ~annotation:(mk_annot "value_or_null")
            ~why:(Value_or_null_creation why)

      let value ~(why : Jkind_intf.History.value_creation_reason) =
        fresh_jkind Jkind_desc.Builtin.value ~annotation:(mk_annot "value")
          ~why:(Value_creation why)

      let immutable_data ~(why : Jkind_intf.History.value_creation_reason) =
        fresh_jkind Jkind_desc.Builtin.immutable_data
          ~annotation:(mk_annot "immutable_data")
          ~why:(Value_creation why)

      let sync_data ~(why : Jkind_intf.History.value_creation_reason) =
        fresh_jkind Jkind_desc.Builtin.sync_data
          ~annotation:(mk_annot "sync_data") ~why:(Value_creation why)

      let mutable_data ~(why : Jkind_intf.History.value_creation_reason) =
        fresh_jkind Jkind_desc.Builtin.mutable_data
          ~annotation:(mk_annot "mutable_data") ~why:(Value_creation why)

      let immediate ~why =
        fresh_jkind Jkind_desc.Builtin.immediate
          ~annotation:(mk_annot "immediate") ~why:(Immediate_creation why)
        |> mark_best

      let immediate_or_null ~why =
        fresh_jkind Jkind_desc.Builtin.immediate_or_null
          ~annotation:(mk_annot "immediate_or_null")
          ~why:(Immediate_or_null_creation why)

      let product ~why tys_modalities layouts =
        let desc = Jkind_desc.product tys_modalities layouts in
        fresh_jkind_poly desc ~annotation:None ~why:(Product_creation why)
        (* [mark_best] is correct here because the with-bounds of a product
           jkind include all the components of the product. Accordingly, looking
           through the product, by one step, never loses any information. *)
        |> mark_best

      let product_of_sorts ~why ~level arity =
        let layout =
          Jkind_types.Layout.product
            (List.init arity
               (fun _ -> fst (Jkind_types.Layout.of_new_sort_var ~level)))
        in
        let desc : _ jkind_desc =
          { layout;
            mod_bounds = Mod_bounds.max;
            with_bounds = No_with_bounds }
        in
        fresh_jkind_poly desc ~annotation:None ~why:(Product_creation why)
      (* We do not [mark_best] here because the resulting jkind is used (only)
         in the middle of type-checking mutually recursive type
         declarations. See Note [Default jkind in transl_declaration] for more
         commentary on why we don't want [Best] jkinds there. *)
    end

    let has_with_bounds (type r) (t : (_ * r) jkind) =
      match t.jkind.with_bounds with
      | No_with_bounds -> false
      | With_bounds tys -> not (With_bounds_types.is_empty tys)

    let of_const (type l r) ~annotation ~why ~(quality : (l * r) jkind_quality)
      ~ran_out_of_fuel_during_normalize (c : (l * r) Const.t) =
      { jkind = Layout_and_axes.map Jkind_types.Layout.of_const c;
        annotation;
        history = Creation why;
        has_warned = false;
        ran_out_of_fuel_during_normalize;
        quality
      }

    let of_builtin ~why Const.Builtin.{ jkind; name } =
      jkind
      |> Layout_and_axes.allow_left
      |> Layout_and_axes.disallow_right
      |> of_const ~annotation:(mk_annot name)
           ~why
             (* The [Best] is OK here because this function is used only in
                Predef. *)
           ~quality:Best ~ran_out_of_fuel_during_normalize:false

    let get_const t = Jkind_desc.get_const t.jkind

    let map_type_expr f t =
      if has_with_bounds t
      then { t with jkind = Jkind_desc.map_type_expr f t.jkind }
      else t (* short circuit this common case *)

    let add_with_bounds ~modality ~type_expr t =
      { t with
        jkind =
          Jkind_desc.add_with_bounds
          (* We only care about types in fields of unboxed products for the
             nullability of the overall kind *)
            ~relevant_for_shallow:`Irrelevant ~type_expr ~modality t.jkind
      }

    let jkind_of_mutability mutability ~why =
      (match mutability with
      | Immutable -> Builtin.immutable_data
      | Mutable { atomic = Atomic; _ } -> Builtin.sync_data
      | Mutable { atomic = Nonatomic; _ } -> Builtin.mutable_data)
        ~why

    let all_void_labels lbls =
      List.for_all
        (fun (lbl : label_declaration) ->
           Jkind_types.Sort.Const.(all_void lbl.ld_sort))
        lbls

    let add_labels_as_with_bounds lbls jkind =
      List.fold_right
        (fun (lbl : label_declaration) ->
          add_with_bounds ~type_expr:lbl.ld_type ~modality:lbl.ld_modalities)
        lbls jkind

    let for_boxed_record lbls =
      if all_void_labels lbls
      then Builtin.immediate ~why:Empty_record
      else
        let base =
          lbls
          |> List.map (fun (ld : label_declaration) -> ld.ld_mutable)
          |> List.fold_left combine_mutability Immutable
          |> jkind_of_mutability ~why:Boxed_record
          |> mark_best
        in
        add_labels_as_with_bounds lbls base

    let for_non_float ~(why : Jkind_intf.History.value_creation_reason) =
      let mod_bounds =
        Mod_bounds.create Mode.Crossing.max
          ~externality:Mod_bounds.Externality.max
          ~nullability:Non_null ~separability:Non_float
      in
      fresh_jkind
        { layout = Sort (Base Value); mod_bounds; with_bounds = No_with_bounds }
        ~annotation:None ~why:(Value_creation why)

  (* Note [With-bounds for GADTs]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     Inferring the with-bounds for a variant requires gathering bounds from each
     constructor. We thus loop over each constructor:

     A. If a constructor is not a GADT constructor, just add its fields and
     their modalities as with-bounds.

     B. If a constructor uses GADT syntax:

     GADT constructors introduce their own local scope. That is, when we see

     {[
       type 'a t = K : 'b option -> 'b t
     ]}

     the ['b] in the constructor is distinct from the ['a] in the type header.
     This would be true even if we wrote ['a] in the constructor: the variables
     introduced in the type head never scope over GADT constructors.

     So in order to get properly-scoped with-bounds, we must substitute.  But
     what, exactly, do we substitute? The domain is the bare variables that
     appear as arguments in the return type. The range is the corresponding
     variables in the type head (even if those are written as [_]s; which are
     turned into proper type variables by now).

     We use [Ctype.apply] (passed in as [type_apply]) to perform the
     substitution.

     We thus have

     * STEP B1. Gather such variables from the result type, matching them with
     their corresponding variables in the type head. We'll call these B1
     variables.

     We do not actually substitute quite yet.

     There may still be other free type variables in the constructor type. Here
     are some examples:

     {[
       type 'a t =
         | K1 : 'o -> int t
         | K2 : 'o -> 'o option t
         | K3 : 'o -> 'b t
     ]}

     In each constructor, the type variable ['o] is not a B1 variable.  (The
     ['b] in [K3] /is/ a B1 variable.) We call these variables /orphaned/. All
     existential variables are orphans (as we see in [K1] and [K3]), but even
     non-existential variables can be orphan (as we see in [K2]; note that ['o]
     appears in the result).

     We wish to replace each orphaned type variable with a [Tof_kind], holding
     just its kind. Since [Tof_kind] has a *best* kind, they'll just get
     normalized away during normalization, except in the case that they show up
     as an argument to a type constructor representing an abstract type - in
     which case, they still end up in the (fully normalized) with-bounds. For
     example, the following type:

     {[
       type t : A : ('a : value mod portable). 'a abstract -> t
     ]}

     has kind:

     {[
       immutable_data with (type : value mod portable) abstract
     ]}

     This use of the [(type : <<kind>>)] construct is the reason we have
     [Tof_kind] in the first place.

     We thus have

     * STEP B2. Gather the orphaned variables
     * STEP B3. Build the [Tof_kind] types to use in the substitution
     * STEP B4. Perform the substitution

     There are wrinkles:

     BW1. For repeated types on arguments, e.g. in the following type:

     {[
       type ('x, 'y) t = A : 'a -> ('a, 'a) t
     ]}

     we substitute only the *first* time we see an argument.  That means that in
     the above type, we'll map all instances of ['a] to ['x] and infer a kind of
     [immutable_data with 'x]. This is sound, but somewhat restrictive; in a
     perfect world, we'd infer a kind of [immutable_data with ('x OR 'y)], but
     that goes beyond what with-bounds can describe (which, if we implemented
     it, would introduce a disjunction in type inference, requiring
     backtracking). At some point in the future, we should at least change the
     subsumption algorithm to accept either [immutable_data with 'x] or
     [immutable_data with 'y] (* CR layouts v2.8: do that *)

     BW2. All of the above applies for row variables. Here is an example:

     {[
       type t = K : [> `A] -> t
     ]}

     The row variable in the [ [> `A] ] is existential, and thus gets
     transformed into a [(type : value)] when computing the kind of [t].

     This fact has a few consequences:

     * [Tof_kind] can appear as a [row_more].
     * When [Tof_kind] is a [row_more], that row is considered fixed; it thus
       needs a [fixed_explanation]. The [fixed_explanation] is [Existential],
       used only for this purpose.
  *)
  let for_boxed_variant ~loc ~decl_params ~type_apply ~free_vars cstrs =
    let base =
      let all_args_void =
        List.for_all
          (fun cstr ->
            match cstr.cd_args with
            | Cstr_tuple args ->
              List.for_all
                (fun arg -> Jkind_types.Sort.Const.(all_void arg.ca_sort)) args
            | Cstr_record lbls -> all_void_labels lbls)
          cstrs
      in
      if all_args_void
      then (
        let has_args =
          List.exists
            (fun cstr ->
              match cstr.cd_args with
              | Cstr_tuple (_ :: _) | Cstr_record (_ :: _) -> true
              | Cstr_tuple [] | Cstr_record [] -> false)
            cstrs
        in
        if has_args && Language_extension.erasable_extensions_only ()
        then
          Location.prerr_warning loc
            (Warnings.Incompatible_with_upstream
               Warnings.Immediate_void_variant);
        Builtin.immediate ~why:Enumeration)
      else
        List.concat_map
          (fun cstr ->
            match cstr.cd_args with
            | Cstr_tuple _ -> [Immutable]
            | Cstr_record lbls -> List.map (fun ld -> ld.ld_mutable) lbls)
          cstrs
        |> List.fold_left combine_mutability Immutable
        |> jkind_of_mutability ~why:Boxed_variant
    in
    let base = mark_best base in
    let add_with_bounds_for_cstr jkind_so_far cstr =
      let cstr_arg_tys, cstr_arg_modalities =
        match cstr.cd_args with
        | Cstr_tuple args ->
          List.fold_left
            (fun (tys, ms) arg -> arg.ca_type :: tys, arg.ca_modalities :: ms)
            ([], []) args
        | Cstr_record lbls ->
          List.fold_left
            (fun (tys, ms) lbl -> lbl.ld_type :: tys, lbl.ld_modalities :: ms)
            ([], []) lbls
      in
      let cstr_arg_tys =
        match cstr.cd_res with
        | None -> cstr_arg_tys
        | Some res ->
          (* See Note [With-bounds for GADTs] for an overview *)
          let apply_subst domain range tys =
            if Misc.Stdlib.List.is_empty domain
            then tys
            else List.map (fun ty -> type_apply domain ty range) tys
          in
          (* STEP B1 from Note [With-bounds for GADTs]: *)
          let res_args =
            match get_desc res with
            | Tconstr (_, args, _) -> args
            | _ -> Misc.fatal_error "cd_res must be Tconstr"
          in
          let domain, range, seen =
            List.fold_left2
              (* CR ocaml-5.4: Use labeled tuples for the accumulator here *)
                (fun ((domain, range, seen) as acc) arg param ->
                if TypeSet.mem arg seen
                then
                  (* We've already seen this type parameter, so don't add it
                     again.  See wrinkle BW1 from Note [With-bounds for GADTs]
                  *)
                  acc
                else
                  match get_desc arg with
                  | Tvar _ ->
                    (* Only add types which are direct variables. Note that
                       types which aren't variables might themselves /contain/
                       variables; if those variables don't show up on another
                       parameter, they're treated as orphaned. See example K2
                       from Note [With-bounds for GADTs] *)
                    arg :: domain, param :: range, TypeSet.add arg seen
                  | _ -> acc)
              ([], [], TypeSet.empty)
              res_args decl_params
          in
          (* STEP B2 from Note [With-bounds for GADTs]: *)
          let free_var_set = free_vars cstr_arg_tys in
          let orphaned_type_var_set = TypeSet.diff free_var_set seen in
          let orphaned_type_var_list = TypeSet.elements orphaned_type_var_set in
          (* STEP B3 from Note [With-bounds for GADTs]: *)
          let mk_type_of_kind ty =
            match get_desc ty with
            (* use [newgenty] not [newty] here because we've already
               generalized the decl and want to keep things at
               generic_level *)
            | Tvar { jkind; name = _ } -> newgenty (Tof_kind jkind)
            | _ ->
              Misc.fatal_error
                "post-condition of [free_variable_set_of_list] violated"
          in
          let type_of_kind_list =
            List.map mk_type_of_kind orphaned_type_var_list
          in
          (* STEP B4 from Note [With-bounds for GADTs]: *)
          let cstr_arg_tys =
            apply_subst
              (orphaned_type_var_list @ domain)
              (type_of_kind_list @ range)
              cstr_arg_tys
          in
          cstr_arg_tys
      in
      List.fold_left2
        (fun jkind type_expr modality ->
          add_with_bounds ~modality ~type_expr jkind)
        jkind_so_far cstr_arg_tys cstr_arg_modalities
    in
    List.fold_left add_with_bounds_for_cstr base cstrs

    let for_float ident =
      let crossing =
        Mode.Crossing.create ~regionality:false ~linearity:true
          ~portability:true ~forkable:true ~yielding:true ~uniqueness:false
          ~contention:true ~statefulness:true ~visibility:true ~staticity:false
      in
      let mod_bounds =
        Mod_bounds.create crossing ~externality:Mod_bounds.Externality.max
          ~nullability:Non_null ~separability:Separable
      in
      fresh_jkind
        { layout = Sort (Base Value); mod_bounds; with_bounds = No_with_bounds }
        ~annotation:None ~why:(Primitive ident)
      |> mark_best

    let for_array_argument =
      let mod_bounds =
        Mod_bounds.create Mode.Crossing.max
          ~externality:Mod_bounds.Externality.max
          ~nullability:Maybe_null ~separability:Separable
      in
      fresh_jkind
        { layout = Any; mod_bounds; with_bounds = No_with_bounds }
        ~annotation:None ~why:(Any_creation Array_type_argument)

    let for_or_null_argument ident =
      let why : Jkind_intf.History.value_creation_reason =
        Type_argument
          { parent_path = Path.Pident ident; position = 1; arity = 1 }
      in
      let mod_bounds =
        Mod_bounds.create Mode.Crossing.max
          ~externality:Mod_bounds.Externality.max
          ~nullability:Non_null ~separability:Maybe_separable
      in
      fresh_jkind
        { layout = Sort (Base Value);
          mod_bounds;
          with_bounds = No_with_bounds }
        ~annotation:None ~why:(Value_creation why)
  end

  include Jkind
end
