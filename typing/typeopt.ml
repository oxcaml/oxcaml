(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Path
open Types
open Typedtree
open Lambda

type error =
    Non_value_layout of type_expr * Jkind.Violation.t option
  | Sort_without_extension of
      Jkind.Sort.t * Language_extension.maturity * type_expr option
  | Small_number_sort_without_extension of Jkind.Sort.t * type_expr option
  | Simd_sort_without_extension of Jkind.Sort.t * type_expr option
  | Not_a_sort of type_expr * Jkind.Violation.t
  | Unsupported_product_in_lazy of Jkind.Sort.Const.t
  | Unsupported_vector_in_product_array
  | Mixed_product_array of Jkind.Sort.Const.t * type_expr
  | Unsupported_void_in_array
  | Product_iarrays_unsupported
  | Opaque_array_non_value of
      { array_type: type_expr;
        elt_kinding_failure: (type_expr * Jkind.Violation.t) option }

exception Error of Location.t * error

(* Expand a type, looking through ordinary synonyms, private synonyms, links,
   and [@@unboxed] types. The returned type will be therefore be none of these
   cases (except in case of missing cmis).

   If we fail to fully scrape the type due to missing a missing cmi file, we
   return the original, rather than a partially expanded one.  The original may
   have cached jkind information that is more accurate than can be computed
   from its expanded form. *)
let scrape_ty env ty =
  let ty =
    match get_desc ty with
    | Tpoly(ty, _) -> ty
    | _ -> ty
  in
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.correct_levels ty in
      let ty' = Ctype.expand_head_opt env ty in
      begin match get_desc ty' with
      | Tconstr (p, _, _) ->
          begin match find_unboxed_type (Env.find_type p env) with
          | Some _ -> (Ctype.get_unboxed_type_approximation env ty').ty
          | None -> ty'
          | exception Not_found -> ty (* missing cmi file *)
          end
      | _ ->
          ty'
      end
  | _ -> ty

(* See [scrape_ty]; this returns the [type_desc] of a scraped [type_expr]. *)
let scrape env ty =
  get_desc (scrape_ty env ty)

let scrape_poly env ty =
  let ty = scrape_ty env ty in
  match get_desc ty with
  | Tpoly (ty, _) -> get_desc ty
  | d -> d

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (_, lhs, rhs, _) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr(p, _, _) -> Path.same p base_ty_path
  | _ -> false

let is_always_gc_ignorable env ty =
  let ext : Jkind_axis.Externality.t =
    (* We check that we're compiling to (64-bit) native code before counting
       External64 types as gc_ignorable, because bytecode is intended to be
       platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64
    then External64
    else External
  in
  Ctype.check_type_externality env ty ext

let maybe_pointer_type env ty =
  let ty = scrape_ty env ty in
  let immediate_or_pointer =
    match is_always_gc_ignorable env ty with
    | true -> Immediate
    | false -> Pointer
  in
  let nullable =
    match Ctype.check_type_nullability env ty Non_null with
    | true -> Non_nullable
    | false -> Nullable
  in
  immediate_or_pointer, nullable

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

(* CR layouts v2.8: Calling [type_legacy_sort] in [typeopt] is not ideal
   and this function should be removed at some point. To do that, there
   needs to be a way to store sort vars on [Tconstr]s. That means
   either introducing a [Tpoly_constr], allow type parameters with
   sort info, or do something else. *)
(* CR layouts v3.0: have a better error message
   for nullable jkinds.*)
let type_legacy_sort ~why env loc ty =
  match Ctype.type_legacy_sort ~why env ty with
  | Ok sort -> sort
  | Error err -> raise (Error (loc, Not_a_sort (ty, err)))

(* [classification]s are used for two things: things in arrays, and things in
   lazys. In the former case, we need detailed information about unboxed
   products and in the latter it would be wasteful to compute that information,
   so this type is polymorphic in what it remembers about products. *)
type 'a classification =
  | Int   (* any immediate type *)
  | Float
  | Void
  | Unboxed_float of unboxed_float
  | Unboxed_int of unboxed_integer
  | Unboxed_vector of unboxed_vector
  | Lazy
  | Addr  (* any value except a float or a lazy *)
  | Any
  | Product of 'a

(* Classify a ty into a [classification]. Looks through synonyms, using
   [scrape_ty].  Returning [Any] is safe, though may skip some optimizations.
   See comment on [classification] above to understand [classify_product]. *)
let classify ~classify_product env ty sort : _ classification =
  let ty = scrape_ty env ty in
  match (sort : Jkind.Sort.Const.t) with
  | Base Value -> begin
  if is_always_gc_ignorable env ty then Int
  else match get_desc ty with
  | Tvar _ | Tunivar _ ->
      Any
  | Tconstr (p, _args, _abbrev) ->
      if Path.same p Predef.path_float then Float
      else if Path.same p Predef.path_lazy_t then Lazy
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_bytes
           || Path.same p Predef.path_array
           || Path.same p Predef.path_iarray
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_float32
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64
           || Path.same p Predef.path_int8x16
           || Path.same p Predef.path_int16x8
           || Path.same p Predef.path_int32x4
           || Path.same p Predef.path_int64x2
           || Path.same p Predef.path_float32x4
           || Path.same p Predef.path_float64x2
           || Path.same p Predef.path_int8x32
           || Path.same p Predef.path_int16x16
           || Path.same p Predef.path_int32x8
           || Path.same p Predef.path_int64x4
           || Path.same p Predef.path_float32x8
           || Path.same p Predef.path_float64x4
           || Path.same p Predef.path_int8x64
           || Path.same p Predef.path_int16x32
           || Path.same p Predef.path_int32x16
           || Path.same p Predef.path_int64x8
           || Path.same p Predef.path_float32x16
           || Path.same p Predef.path_float64x8
           then Addr
      else begin
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract _ ->
              Any
          | Type_record _ | Type_variant _ | Type_open ->
              Addr
          | Type_record_unboxed_product _ ->
              Any
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any
      end
  | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ ->
      Addr
  | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ | Tunboxed_tuple _ | Tof_kind _ ->
      assert false
  end
  | Base Float64 -> Unboxed_float Unboxed_float64
  | Base Float32 -> Unboxed_float Unboxed_float32
  | Base Bits8 -> Unboxed_int Unboxed_int8
  | Base Bits16 -> Unboxed_int Unboxed_int16
  | Base Bits32 -> Unboxed_int Unboxed_int32
  | Base Bits64 -> Unboxed_int Unboxed_int64
  | Base Vec128 -> Unboxed_vector Unboxed_vec128
  | Base Vec256 -> Unboxed_vector Unboxed_vec256
  | Base Vec512 -> Unboxed_vector Unboxed_vec512
  | Base Word -> Unboxed_int Unboxed_nativeint
  | Base Void -> Void
  | Product c -> Product (classify_product ty c)

let rec scannable_product_array_kind elt_ty_for_error loc sorts =
  List.map (sort_to_scannable_product_element_kind elt_ty_for_error loc) sorts

and sort_to_scannable_product_element_kind elt_ty_for_error loc
      (s : Jkind.Sort.Const.t) =
  (* Unfortunate: this never returns `Pint_scannable`.  Doing so would require
     this to traverse the type, rather than just the kind, or to add product
     kinds. *)
  match s with
  | Base Value -> Paddr_scannable
  | Base (Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Word |
          Vec128 | Vec256 | Vec512) as c ->
    raise (Error (loc, Mixed_product_array (c, elt_ty_for_error)))
  | Base Void ->
    raise (Error (loc, Unsupported_void_in_array))
  | Product sorts ->
    Pproduct_scannable (scannable_product_array_kind elt_ty_for_error loc sorts)

let rec ignorable_product_array_kind loc sorts =
  List.map (sort_to_ignorable_product_element_kind loc) sorts

and sort_to_ignorable_product_element_kind loc (s : Jkind.Sort.Const.t) =
  match s with
  | Base Value -> Pint_ignorable
  | Base Float64 -> Punboxedfloat_ignorable Unboxed_float64
  | Base Float32 -> Punboxedfloat_ignorable Unboxed_float32
  | Base Bits8 -> Punboxedint_ignorable Unboxed_int8
  | Base Bits16 -> Punboxedint_ignorable Unboxed_int16
  | Base Bits32 -> Punboxedint_ignorable Unboxed_int32
  | Base Bits64 -> Punboxedint_ignorable Unboxed_int64
  | Base Word -> Punboxedint_ignorable Unboxed_nativeint
  | Base (Vec128 | Vec256 | Vec512) ->
    raise (Error (loc, Unsupported_vector_in_product_array))
  | Base Void -> raise (Error (loc, Unsupported_void_in_array))
  | Product sorts -> Pproduct_ignorable (ignorable_product_array_kind loc sorts)

let array_kind_of_elt ~elt_sort env loc ty =
  let elt_sort =
    match elt_sort with
    | Some s -> s
    | None ->
      Jkind.Sort.default_for_transl_and_get
        (type_legacy_sort ~why:Array_element env loc ty)
  in
  let elt_ty_for_error = ty in (* report the un-scraped ty in errors *)
  let classify_product ty sorts =
    if is_always_gc_ignorable env ty then
      Pgcignorableproductarray (ignorable_product_array_kind loc sorts)
    else
      Pgcscannableproductarray
        (scannable_product_array_kind elt_ty_for_error loc sorts)
  in
  (* CR dkalinichenko: many checks in [classify] are redundant
     with separability. *)
  match classify ~classify_product env ty elt_sort with
  | Any ->
    if Config.flat_float_array
      && not (Language_extension.is_at_least Separability ()
          && Ctype.check_type_separability env ty Non_float)
    then Pgenarray
    else Paddrarray
  | Float -> if Config.flat_float_array then Pfloatarray else Paddrarray
  | Addr | Lazy -> Paddrarray
  | Int -> Pintarray
  | Unboxed_float f -> Punboxedfloatarray f
  | Unboxed_int i -> Punboxedintarray i
  | Unboxed_vector v -> Punboxedvectorarray v
  | Product c -> c
  | Void ->
    raise (Error (loc, Unsupported_void_in_array))

let array_type_kind ~elt_sort ~elt_ty env loc ty =
  match scrape_poly env ty with
  | Tconstr(p, [elt_ty], _) when Path.same p Predef.path_array ->
      array_kind_of_elt ~elt_sort env loc elt_ty
  | Tconstr(p, [elt_ty], _) when Path.same p Predef.path_iarray ->
      let kind = array_kind_of_elt ~elt_sort env loc elt_ty in
      (* CR layouts v7.1: allow iarrays of products. *)
      begin match kind with
      | Pgcscannableproductarray _ | Pgcignorableproductarray _ ->
        raise (Error (loc, Product_iarrays_unsupported))
      | Pgenarray | Paddrarray | Pintarray | Pfloatarray | Punboxedfloatarray _
      | Punboxedintarray _ | Punboxedvectorarray _  ->
        kind
      end
  | Tconstr(p, [], _) when Path.same p Predef.path_floatarray ->
      Pfloatarray
  | _ ->
    begin match elt_ty with
    | Some elt_ty ->
      let rhs = Jkind.Builtin.value ~why:Array_type_kind in
      begin match Ctype.constrain_type_jkind env elt_ty rhs with
      | Ok _ -> Pgenarray
      | Error e ->
        (* CR layouts v4: rather than constraining [elt_ty]'s jkind to be value,
           we could instead use its jkind to determine a non-value array kind.

           We are choosing to error in this case for now because it is safer,
           and because it could be potentially confusing that there is a second
           source of information used to determine array type kinds (in addition
           to the type kind of the array parameter). See PR #4098.

           Using its jkind to determine a non-value array kind would also only
           be useful for explicit user-written primitives. In other cases where
           we compute an array kind (array matching, array comprehension),
           [elt_ty] is [None].
        *)
        raise (Error(loc,
          Opaque_array_non_value {
            array_type = ty;
            elt_kinding_failure = Some (elt_ty, e);
          }))
      end
    | None ->
      raise (Error(loc,
        Opaque_array_non_value {
          array_type = ty;
          elt_kinding_failure = None;
        }))
    end

let array_type_mut env ty =
  match scrape_poly env ty with
  | Tconstr(p, [_], _) when Path.same p Predef.path_iarray -> Immutable
  | _ -> Mutable

let array_kind exp elt_sort =
  array_type_kind
    ~elt_sort:(Some elt_sort) ~elt_ty:None
    exp.exp_env exp.exp_loc exp.exp_type

let array_pattern_kind pat elt_sort =
  array_type_kind
    ~elt_sort:(Some elt_sort) ~elt_ty:None
    pat.pat_env pat.pat_loc pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name), [], _)
    when Ident.name mod_id = "Stdlib__Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float16_elt", Pbigarray_float16;
   "float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_specialize_kind_and_layout env ~kind ~layout typ =
  match scrape env typ with
  | Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev) ->
      let kind =
        match kind with
        | Pbigarray_unknown ->
          bigarray_decode_type env elt_type kind_table Pbigarray_unknown
        | _ -> kind
      in
      let layout =
        match layout with
        | Pbigarray_unknown_layout ->
          bigarray_decode_type env layout_type layout_table Pbigarray_unknown_layout
        | _ -> layout
      in
      (kind, layout)
  | _ ->
      (kind, layout)

let value_kind_of_value_jkind env jkind =
  let layout = Jkind.get_layout_defaulting_to_value jkind in
  (* In other places, we use [Ctype.type_jkind_purely_if_principal]. Here, we omit
     the principality check, as we're just trying to compute optimizations. *)
  let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
  let externality_upper_bound = Jkind.get_externality_upper_bound ~jkind_of_type jkind in
  match layout, externality_upper_bound with
  | Base Value, External -> Pintval
  | Base Value, External64 ->
    if !Clflags.native_code && Sys.word_size = 64 then Pintval else Pgenval
  | Base Value, Internal -> Pgenval
  | Any, _
  | Product _, _
  | Base (Void | Float64 | Float32 | Word | Bits8 | Bits16 | Bits32 | Bits64 |
          Vec128 | Vec256 | Vec512) , _ ->
    Misc.fatal_error "expected a layout of value"

(* [value_kind] has a pre-condition that it is only called on values.  With the
   current set of sort restrictions, there are two reasons this invariant may
   be violated:

   1) A bug in the type checker or the translation to lambda.
   2) A missing cmi file, so that we can't accurately compute the sort of
      some type.

   In case 1, we have a bug and should fail loudly.

   In case 2, we could issue an error and make the user add the dependency
   explicitly.  But because [value_kind] looks at the subcomponents of your type,
   this can lead to some surprising and unnecessary errors.  Suppose we're
   computing the value kind for some type:

     type t = int * M.t

   If we're missing the cmi for [M], we can't verify the invariant that
   [value_kind] is only called on values.  However, we still know the pair
   itself is a value, so a sound thing to do is fall back and return [Pgenval]
   for [t].

   On the other hand, if we're asked to compute the value kind for [M.t]
   directly and are missing the cmi for [M], we really do need to issue an error.
   This is a bug in the typechecker, which should have checked that the type
   in question has layout value.

   To account for these possibilities, [value_kind] can not simply assume its
   precondition holds, and must check.  This is implemented as calls to
   [check_type_jkind] at the start of its implementation.  If this check
   encounters layout [any] and it arises from a missing cmi, it raises
   [Missing_cmi_fallback].  If it encounters [any] that didn't arise from a
   missing cmi, or any other non-value layout, it fails loudly.

   In places where we're computing value_kinds for a bunch of subcomponents of a
   type, we catch [Missing_cmi_fallback] and just return [Pgenval] for the outer
   type.  If it escapes unhandled from value-kind, we catch it and issue the
   loud error.

   We used to believe we would eventually drop the layout check from
   [value_kind], because we thought it was just a sanity check.  This is wrong.
   We'll always need it to make sure we're sound in the event of a missing cmi
   (at least, as long as [value_kind] continues to inspect types more deeply
   than is otherwise needed for typechecking).  Even if the build system always
   passed cmis for all transitive dependencies, we shouldn't be unsound in the
   event the compiler is invoked manually without them.

   (But, if we ever do find a way to get rid of the safety check: Note that the
   it is currently doing some defaulting of sort variables, as in cases like:

     let () =
       match assert false  with
       | _ -> assert false

   There is a sort variable for the scrutinee of the match in typedtree that is
   still a sort variable after checking this.  It's fine to default this to
   anything - void would be ideal, but for now it gets value.  If the safety check
   goes away, think about whether we should add defaulting elsewhere.)
*)
exception Missing_cmi_fallback

let non_nullable raw_kind = { raw_kind; nullable = Non_nullable }

let nullable raw_kind = { raw_kind; nullable = Nullable }

(* CR layouts v3: This file has two approaches for checking
   nullability. [representation_properties_type] does this by calling
   [Ctype.check_type_nullability] (which is just [constrain_type_jkind] on [any
   mod non_null]), while [add_nullability_from_jkind] just pulls it out of a
   kind (and sometimes we compute a jkind with [estimate_type_jkind] for that
   purpose).

   The former is a bit more expensive (though quite cheap in the places where we
   are doing it now, as the type has already been scraped) but will give a fully
   accurate nullability. The later is conservative but cheaper when we already
   have a jkind. We should pick one, or rationalize why there are two.
*)
let add_nullability_from_jkind env jkind raw_kind =
  let jkind_of_type ty = Some (Ctype.type_jkind_purely env ty) in
  let nullable =
    match Jkind.get_nullability ~jkind_of_type jkind with
    | Non_null -> Non_nullable
    | Maybe_null -> Nullable
  in
  { raw_kind; nullable }

let fallback_if_missing_cmi ~default f =
  try f () with Missing_cmi_fallback -> default

(* CR layouts v2.5: It will be possible for subcomponents of types to be
   non-values for non-error reasons (e.g., [type t = { x : float# }
   [@@unboxed]).  And in later releases, this will also happen in normal
   records, variants, tuples...

   The current layout checks are overly conservative in those cases, because
   they are currently errors.  Instead, recursive calls to value kind should
   check the sorts of the relevant types.  Ideally this wouldn't involve
   expensive layout computation, because the sorts are stored somewhere (e.g.,
   [record_representation]).  But that's not currently the case for tuples. *)
let rec value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
  : int * value_kind =
  let[@inline] cannot_proceed () =
    Numbers.Int.Set.mem (get_id ty) visited
    || depth >= 2
    || num_nodes_visited >= 30
  in
  let scty = scrape_ty env ty in
  begin
    (* CR layouts: We want to avoid correcting levels twice, and scrape_ty will
       correct levels for us.  But it may be the case that we could do the
       layout check on the original type but not the scraped type, because of
       missing cmis.  So we try the scraped type, and fall back to correcting
       levels a second time if that doesn't work.

       It would be nice to correct levels once at the beginning and pass that
       type to both scrape_ty and the safety check, but I found this causes an
       infinite loop in the typechecker.  Whichever you do second, the layout
       check or scrape_ty, that thing will loop.  This is the test case that
       triggers it:

       (* Check for a potential infinite loop in the typing algorithm. *)
       type 'a t12 = M of 'a t12 [@@ocaml.unboxed] [@@value];;

       This should be understood, but for now the simple fall back thing is
       sufficient.  *)
    match Ctype.check_type_jkind env scty (Jkind.Builtin.value_or_null ~why:V1_safety_check)
    with
    | Ok _ -> ()
    | Error _ ->
      match
        Ctype.(check_type_jkind env
                 (correct_levels ty) (Jkind.Builtin.value_or_null ~why:V1_safety_check))
      with
      | Ok _ -> ()
      | Error violation ->
        if (Jkind.Violation.is_missing_cmi violation)
        then raise Missing_cmi_fallback
        else raise (Error (loc, Non_value_layout (ty, Some violation)))
  end;
  match get_desc scty with
  | Tconstr(p, _, _) when Path.same p Predef.path_int ->
    num_nodes_visited, non_nullable Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_char ->
    num_nodes_visited, non_nullable Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_int8 ->
    num_nodes_visited, non_nullable Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_int16 ->
    num_nodes_visited, non_nullable Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_floatarray ->
    num_nodes_visited, non_nullable (Parrayval Pfloatarray)
  | Tconstr(p, _, _) when Path.same p Predef.path_float ->
    num_nodes_visited, non_nullable (Pboxedfloatval Boxed_float64)
  | Tconstr(p, _, _) when Path.same p Predef.path_float32 ->
    num_nodes_visited, non_nullable (Pboxedfloatval Boxed_float32)
  | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
    num_nodes_visited, non_nullable (Pboxedintval Boxed_int32)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
    num_nodes_visited, non_nullable (Pboxedintval Boxed_int64)
  | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
    num_nodes_visited, non_nullable (Pboxedintval Boxed_nativeint)
  | Tconstr(p, _, _) when Path.same p Predef.path_int8x16 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_int16x8 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_int32x4 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64x2 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_float32x4 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_float64x2 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec128)
  | Tconstr(p, _, _) when Path.same p Predef.path_int8x32 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_int16x16 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_int32x8 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64x4 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_float32x8->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_float64x4 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec256)
  | Tconstr(p, _, _) when Path.same p Predef.path_int8x64 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, _, _) when Path.same p Predef.path_int16x32 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, _, _) when Path.same p Predef.path_int32x16 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, _, _) when Path.same p Predef.path_int64x8 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, _, _) when Path.same p Predef.path_float32x16->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, _, _) when Path.same p Predef.path_float64x8 ->
    num_nodes_visited, non_nullable (Pboxedvectorval Boxed_vec512)
  | Tconstr(p, [arg], _)
    when (Path.same p Predef.path_array
          || Path.same p Predef.path_iarray) ->
    (* CR layouts: [~elt_sort:None] here is bad for performance. To
       fix it, we need a place to store the sort on a [Tconstr]. *)
    let ak = array_type_kind ~elt_ty:(Some arg) ~elt_sort:None env loc ty in
    num_nodes_visited, non_nullable (Parrayval ak)
  | Tconstr(p, _, _) -> begin
      (* CR layouts v2.8: The uses of [decl.type_jkind] here are suspect:
         with with-kinds, [decl.type_jkind] will mention variables bound
         by the parameters of the declaration. The code below loses this
         connection and will continue processing with e.g. ['a : value]
         instead of [string] when looking at a [string list]. This should
         probably just call a [type_jkind] function. *)
      let decl =
        try Env.find_type p env with Not_found -> raise Missing_cmi_fallback
      in
      if cannot_proceed () then
        num_nodes_visited,
        add_nullability_from_jkind env decl.type_jkind
          (value_kind_of_value_jkind env decl.type_jkind)
      else
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        (* Default of [Pgenval] is currently safe for the missing cmi fallback
           in the case of @@unboxed variant and records, due to the precondition
           of [value_kind]. Conservatively saying that types from missing
           cmis might be nullable, which is possible in the case of @@unboxed
           types. *)
        match decl.type_kind with
        | Type_variant (cstrs, rep, _) ->
          fallback_if_missing_cmi
            ~default:(num_nodes_visited, nullable Pgenval)
            (fun () -> value_kind_variant env ~loc ~visited ~depth
                         ~num_nodes_visited cstrs rep)
        | Type_record (labels, rep, _) ->
          let depth = depth + 1 in
          fallback_if_missing_cmi
            ~default:(num_nodes_visited, nullable Pgenval)
            (fun () -> value_kind_record env ~loc ~visited ~depth
                         ~num_nodes_visited labels rep)
        | Type_record_unboxed_product ([{ld_type}], Record_unboxed_product, _) ->
          let depth = depth + 1 in
          fallback_if_missing_cmi
            ~default:(num_nodes_visited, nullable Pgenval)
            (fun () ->
               value_kind env ~loc ~visited ~depth ~num_nodes_visited ld_type)
        | Type_record_unboxed_product (([] | _::_::_),
                                       Record_unboxed_product, _) ->
          Misc.fatal_error
            "Typeopt.value_kind: non-unary unboxed record can't have kind value"
        | Type_abstract _ ->
          num_nodes_visited,
          add_nullability_from_jkind env decl.type_jkind
            (value_kind_of_value_jkind env decl.type_jkind)
        | Type_open -> num_nodes_visited, non_nullable Pgenval
    end
  | Ttuple labeled_fields ->
    if cannot_proceed () then
      num_nodes_visited, non_nullable Pgenval
    else
      fallback_if_missing_cmi
        ~default:(num_nodes_visited, non_nullable Pgenval) (fun () ->
        let visited = Numbers.Int.Set.add (get_id ty) visited in
        let depth = depth + 1 in
        let num_nodes_visited, fields =
          List.fold_left_map (fun num_nodes_visited (_, field) ->
            let num_nodes_visited = num_nodes_visited + 1 in
            (* CR layouts v5 - this is fine because voids are not allowed in
               tuples.  When they are, we'll need to make sure that elements
               are values before recurring.
            *)
            value_kind env ~loc ~visited ~depth ~num_nodes_visited field)
            num_nodes_visited labeled_fields
        in
        num_nodes_visited,
        non_nullable
          (Pvariant { consts = [];
                      non_consts = [0, Constructor_uniform fields] }))
  | Tvariant row ->
    num_nodes_visited,
    if Btype.tvariant_not_immediate row
    then non_nullable Pgenval
    else non_nullable Pintval
  | _ ->
    num_nodes_visited,
    add_nullability_from_jkind env (Ctype.estimate_type_jkind env ty) Pgenval

and value_kind_mixed_block_field env ~loc ~visited ~depth ~num_nodes_visited
      (field : Types.mixed_block_element) ty
  : int * unit Lambda.mixed_block_element =
  match field with
  | Value ->
    begin match ty with
    | Some ty ->
      let num_nodes_visited, kind =
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
      in
      num_nodes_visited, Value kind
    | None -> num_nodes_visited, Value (nullable Pgenval)
    (* CR layouts v7.1: assess whether it is important for performance to
       support deep value_kinds here *)
    end
  | Float_boxed -> num_nodes_visited, Float_boxed ()
  | Float64 -> num_nodes_visited, Float64
  | Float32 -> num_nodes_visited, Float32
  | Bits8 -> num_nodes_visited, Bits8
  | Bits16 -> num_nodes_visited, Bits16
  | Bits32 -> num_nodes_visited, Bits32
  | Bits64 -> num_nodes_visited, Bits64
  | Vec128 -> num_nodes_visited, Vec128
  | Vec256 -> num_nodes_visited, Vec256
  | Vec512 -> num_nodes_visited, Vec512
  | Word -> num_nodes_visited, Word
  | Product fs ->
    let num_nodes_visited, kinds =
      Array.fold_left_map (fun num_nodes_visited field ->
        value_kind_mixed_block_field env ~loc ~visited ~depth ~num_nodes_visited
          field None
      ) num_nodes_visited fs
    in
    num_nodes_visited, Product kinds
  | Void -> num_nodes_visited, Product [||]

and value_kind_mixed_block
      env ~loc ~visited ~depth ~num_nodes_visited ~shape types =
  let (_, num_nodes_visited), shape =
    List.fold_left_map
      (fun (i, num_nodes_visited) typ ->
         let num_nodes_visited, kind =
           value_kind_mixed_block_field env ~loc ~visited ~depth
             ~num_nodes_visited shape.(i) typ
         in
         (i+1, num_nodes_visited), kind)
      (0, num_nodes_visited) types
  in
  num_nodes_visited, Constructor_mixed (Array.of_list shape)

and value_kind_variant env ~loc ~visited ~depth ~num_nodes_visited
      (cstrs : Types.constructor_declaration list) rep =
  match rep with
  | Variant_extensible -> assert false
  | Variant_with_null -> begin
    match cstrs with
    | [_; {cd_args=Cstr_tuple [{ca_type=ty}]}] ->
      let num_nodes_visited, kind =
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
      in
      num_nodes_visited + 1, { kind with nullable = Nullable }
    | _ -> assert false
    end
  | Variant_unboxed -> begin
      (* CR layouts v1.5: This should only be reachable in the case of a missing
         cmi, according to the comment on scrape_ty.  Reevaluate whether it's
         needed when we deal with missing cmis. *)
      match cstrs with
      | [{cd_args=Cstr_tuple [{ca_type=ty}]}]
      | [{cd_args=Cstr_record [{ld_type=ty}]}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ty
      | _ -> assert false
    end
  | Variant_boxed cstrs_and_sorts ->
    let depth = depth + 1 in
    let for_one_uniform_value_constructor fields ~field_to_type ~depth
          ~num_nodes_visited =
      let num_nodes_visited, shape =
        List.fold_left_map
          (fun num_nodes_visited field ->
             let ty = field_to_type field in
             let num_nodes_visited = num_nodes_visited + 1 in
             value_kind env ~loc ~visited ~depth ~num_nodes_visited ty)
          num_nodes_visited
          fields
      in
      num_nodes_visited, Lambda.Constructor_uniform shape
    in
    let for_one_constructor (constructor : Types.constructor_declaration)
          ~depth ~num_nodes_visited
          ~(cstr_shape : Types.constructor_representation) =
      let num_nodes_visited = num_nodes_visited + 1 in
      match constructor.cd_args with
      | Cstr_tuple fields ->
        let field_to_type { Types.ca_type } = ca_type in
        let num_nodes_visited, fields =
          match cstr_shape with
          | Constructor_uniform_value ->
              for_one_uniform_value_constructor fields ~field_to_type
                ~depth ~num_nodes_visited
          | Constructor_mixed shape ->
              value_kind_mixed_block env ~loc ~visited ~depth ~num_nodes_visited
                ~shape (List.map (fun f -> Some (field_to_type f)) fields)
        in
        (false, num_nodes_visited), fields
      | Cstr_record labels ->
        let field_to_type (lbl:Types.label_declaration) = lbl.ld_type in
        let is_mutable =
          List.exists
            (fun (lbl:Types.label_declaration) ->
               Types.is_mutable lbl.ld_mutable)
            labels
        in
        let num_nodes_visited, fields =
          match cstr_shape with
          | Constructor_uniform_value ->
              for_one_uniform_value_constructor labels ~field_to_type
                ~depth ~num_nodes_visited
          | Constructor_mixed shape ->
              value_kind_mixed_block env ~loc ~visited ~depth ~num_nodes_visited
                ~shape (List.map (fun f -> Some (field_to_type f)) labels)
        in
        (is_mutable, num_nodes_visited), fields
    in
    let is_constant (cstr: Types.constructor_declaration) =
      match cstr.cd_args with
      | Cstr_tuple [] -> true
      | Cstr_tuple args ->
        List.for_all (fun ca -> Jkind.Sort.Const.all_void ca.ca_sort) args
      | Cstr_record lbls ->
        List.for_all (fun lbl -> Jkind.Sort.Const.all_void lbl.ld_sort) lbls
    in
    let rec mixed_block_shape_is_empty shape =
      Array.for_all mixed_block_element_is_empty shape
    and mixed_block_element_is_empty (element : _ mixed_block_element) =
      match element with
      | Product shape -> mixed_block_shape_is_empty shape
      | _ -> false
    in
    let num_nodes_visited, raw_kind =
    if List.for_all is_constant cstrs then
      (num_nodes_visited, Pintval)
    else
      let _idx, result =
        List.fold_left (fun (idx, result) constructor ->
          idx+1,
          match result with
          | None -> None
          | Some (num_nodes_visited,
                  next_const, consts, next_tag, non_consts) ->
            let cstr_shape, _ = cstrs_and_sorts.(idx) in
            let (is_mutable, num_nodes_visited), fields =
              for_one_constructor constructor ~depth ~num_nodes_visited
                ~cstr_shape
            in
            if is_mutable then None
            else match fields with
            | Constructor_uniform xs when List.compare_length_with xs 0 = 0 ->
              let consts = next_const :: consts in
              Some (num_nodes_visited,
                    next_const + 1, consts, next_tag, non_consts)
            | Constructor_mixed shape when mixed_block_shape_is_empty shape ->
              let consts = next_const :: consts in
              Some (num_nodes_visited,
                    next_const + 1, consts, next_tag, non_consts)
            | Constructor_mixed _ | Constructor_uniform _ ->
              let non_consts =
                (next_tag, fields) :: non_consts
              in
              Some (num_nodes_visited,
                    next_const, consts, next_tag + 1, non_consts))
          (0, Some (num_nodes_visited, 0, [], 0, []))
          cstrs
      in
      begin match result with
      | None -> (num_nodes_visited, Pgenval)
      | Some (num_nodes_visited, _, consts, _, non_consts) ->
        match non_consts with
        | [] -> assert false  (* See [List.for_all is_constant], above *)
        | _::_ ->
          (num_nodes_visited, Pvariant { consts; non_consts })
      end
    in
    num_nodes_visited, non_nullable raw_kind

and value_kind_record env ~loc ~visited ~depth ~num_nodes_visited
      (labels : Types.label_declaration list) rep =
  match rep with
  | (Record_unboxed | (Record_inlined (_, _, Variant_unboxed))) -> begin
      (* CR layouts v1.5: This should only be reachable in the case of a missing
         cmi, according to the comment on scrape_ty.  Reevaluate whether it's
         needed when we deal with missing cmis. *)
      match labels with
      | [{ld_type}] ->
        value_kind env ~loc ~visited ~depth ~num_nodes_visited ld_type
      | [] | _ :: _ :: _ -> assert false
    end
  | Record_inlined (_, _, Variant_with_null) -> assert false
  | Record_inlined (_, _, (Variant_boxed _ | Variant_extensible))
  | Record_boxed _ | Record_float | Record_ufloat | Record_mixed _ -> begin
      let is_mutable =
        List.exists (fun label -> Types.is_mutable label.Types.ld_mutable)
          labels
      in
      if is_mutable then
        num_nodes_visited, non_nullable Pgenval
      else
        let num_nodes_visited, fields =
          match rep with
          | Record_unboxed ->
              (* The outer match guards against this *)
              assert false
          | Record_inlined (_, Constructor_uniform_value, _)
          | Record_boxed _ | Record_float | Record_ufloat ->
              let num_nodes_visited, fields =
                List.fold_left_map
                  (fun num_nodes_visited (label:Types.label_declaration) ->
                    let num_nodes_visited = num_nodes_visited + 1 in
                    let num_nodes_visited, field =
                      (* We're using the `Pboxedfloatval` value kind for unboxed
                        floats inside of records. This is kind of a lie, but
                         that was already happening here due to the float record
                        optimization. *)
                      match rep with
                      | Record_float | Record_ufloat ->
                        num_nodes_visited,
                        non_nullable (Pboxedfloatval Boxed_float64)
                      | Record_inlined _ | Record_boxed _ ->
                          value_kind env ~loc ~visited ~depth ~num_nodes_visited
                            label.ld_type
                      | Record_mixed _ | Record_unboxed ->
                          (* The outer match guards against this *)
                          assert false
                    in
                    num_nodes_visited, field)
                  num_nodes_visited labels
              in
              num_nodes_visited, Constructor_uniform fields
          | Record_inlined (_, Constructor_mixed shape, _)
          | Record_mixed shape ->
            let types = List.map (fun label -> label.Types.ld_type) labels in
            value_kind_mixed_block env ~loc ~visited ~depth ~num_nodes_visited
              ~shape (List.map (fun t -> Some t) types)
        in
        let non_consts =
          match rep with
          | Record_inlined (Ordinary {runtime_tag}, _, _) ->
            [runtime_tag, fields]
          | Record_float | Record_ufloat ->
            [ Obj.double_array_tag, fields ]
          | Record_boxed _ ->
            [0, fields]
          | Record_inlined (Extension _, _, _) ->
            [0, fields]
          | Record_mixed _ ->
            [0, fields]
          | Record_unboxed -> assert false
          | Record_inlined (Null, _, _) -> assert false
        in
        (num_nodes_visited,
         non_nullable (Pvariant { consts = []; non_consts }))
    end

let value_kind env loc ty =
  try
    let (_num_nodes_visited, value_kind) =
      value_kind env ~loc ~visited:Numbers.Int.Set.empty ~depth:0
        ~num_nodes_visited:0 ty
    in
    value_kind
  with
  | Missing_cmi_fallback -> raise (Error (loc, Non_value_layout (ty, None)))

let[@inline always] rec layout_of_const_sort_generic ~value_kind ~error
  : Jkind.Sort.Const.t -> _ = function
  | Base Value -> Lambda.Pvalue (Lazy.force value_kind)
  | Base Float64 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_float Unboxed_float64
  | Base Word when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Unboxed_nativeint
  | Base Bits8 when Language_extension.(is_at_least Layouts Beta) &&
                    Language_extension.(is_at_least Small_numbers Beta) ->
    Lambda.Punboxed_int Unboxed_int8
  | Base Bits16 when Language_extension.(is_at_least Layouts Beta) &&
                     Language_extension.(is_at_least Small_numbers Beta) ->
    Lambda.Punboxed_int Unboxed_int16
  | Base Bits32 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Unboxed_int32
  | Base Bits64 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_int Unboxed_int64
  | Base Float32 when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_float Unboxed_float32
  | Base Vec128 when Language_extension.(is_at_least Layouts Stable) &&
                     Language_extension.(is_at_least SIMD Stable) ->
    Lambda.Punboxed_vector Unboxed_vec128
  | Base Vec256 when Language_extension.(is_at_least Layouts Stable) &&
                     Language_extension.(is_at_least SIMD Stable) ->
    Lambda.Punboxed_vector Unboxed_vec256
  | Base Vec512 when Language_extension.(is_at_least Layouts Stable) &&
                     Language_extension.(is_at_least SIMD Alpha) ->
    Lambda.Punboxed_vector Unboxed_vec512
  | Base Void when Language_extension.(is_at_least Layouts Stable) ->
    Lambda.Punboxed_product []
  | Product consts when Language_extension.(is_at_least Layouts Stable) ->
    (* CR layouts v7.1: assess whether it is important for performance to
       support deep value_kinds here *)
    Lambda.Punboxed_product
      (List.map (layout_of_const_sort_generic
                   ~value_kind:(lazy Lambda.generic_value) ~error)
         consts)
  | ((  Base (Void | Float32 | Float64 | Word | Bits8 | Bits16 | Bits32
             | Bits64 | Vec128 | Vec256 | Vec512)
      | Product _) as const) ->
    error const

let layout env loc sort ty =
  layout_of_const_sort_generic sort
    ~value_kind:(lazy (value_kind env loc ty))
    ~error:(function
      | Base Value -> assert false
      | Base Void as const ->
        raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                   Alpha,
                                                   Some ty)))
      | Base Float32 as const ->
        raise (Error (loc, Small_number_sort_without_extension
                             (Jkind.Sort.of_const const, Some ty)))
      | Base (Vec128 | Vec256 | Vec512) as const ->
        raise (Error (loc, Simd_sort_without_extension
                             (Jkind.Sort.of_const const, Some ty)))
      | (Base (Float64 | Word | Bits8 | Bits16 | Bits32 | Bits64) | Product _)
        as const ->
        raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                   Stable,
                                                   Some ty)))
    )

let layout_of_sort loc sort =
  layout_of_const_sort_generic sort ~value_kind:(lazy Lambda.generic_value)
    ~error:(function
    | Base Value -> assert false
    | Base Void as const ->
      raise (Error (loc, Sort_without_extension (Jkind.Sort.of_const const,
                                                 Alpha,
                                                 None)))
    | Base Float32 as const ->
      raise (Error (loc, Small_number_sort_without_extension
                           (Jkind.Sort.of_const const, None)))
    | Base (Vec128 | Vec256 | Vec512) as const ->
      raise (Error (loc, Simd_sort_without_extension
                           (Jkind.Sort.of_const const, None)))
    | (Base (Float64 | Word | Bits8 | Bits16 | Bits32 | Bits64) | Product _)
      as const ->
      raise (Error (loc, Sort_without_extension
                           (Jkind.Sort.of_const const, Stable, None)))
    )

let layout_of_non_void_sort c =
  layout_of_const_sort_generic
    c
    ~value_kind:(lazy Lambda.generic_value)
    ~error:(fun const ->
      Misc.fatal_errorf "layout_of_const_sort: %a encountered"
        Jkind.Sort.Const.format const)

let function_return_layout env loc sort ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> layout env loc sort rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function2_return_layout env loc sort ty =
  match is_function_type env ty with
  | Some (_lhs, rhs) -> function_return_layout env loc sort rhs
  | None -> Misc.fatal_errorf "function_return_layout called on non-function type"

let function_arg_layout env loc sort ty =
  match is_function_type env ty with
  | Some (arg_type, _) -> layout env loc sort arg_type
  | None -> Misc.fatal_error "function_arg_layout called on non-function type"

(** Whether a forward block is needed for a lazy thunk on a value, i.e.
    if the value can be represented as a float/forward/lazy *)
let lazy_val_requires_forward env loc ty =
  let sort = Jkind.Sort.Const.for_lazy_body in
  let classify_product _ sorts =
    let kind = Jkind.Sort.Const.Product sorts in
    raise (Error (loc, Unsupported_product_in_lazy kind))
  in
  match classify ~classify_product env ty sort with
  | Any | Lazy -> true
  (* CR layouts: Fix this when supporting lazy unboxed values.
     Blocks with forward_tag can get scanned by the gc thus can't
     store unboxed values. Not boxing is also incorrect since the lazy
     type has layout [value] which is different from these unboxed layouts. *)
  | Unboxed_float _ | Unboxed_int _ | Unboxed_vector _ | Void ->
    Misc.fatal_error "Unboxed value encountered inside lazy expression"
  | Float -> Config.flat_float_array
  | Addr | Int -> false
  | Product _ -> assert false (* because [classify_product] raises *)

(** The compilation of the expression [lazy e] depends on the form of e:
    constants, floats and identifiers are optimized.  The optimization must be
    taken into account when determining whether a recursive binding is safe. *)
let classify_lazy_argument : Typedtree.expression ->
                             [`Constant_or_function
                             |`Float_that_cannot_be_shortcut
                             |`Identifier of [`Forward_value|`Other]
                             |`Other] =
  fun e -> match e.exp_desc with
    | Texp_constant
        ( Const_int _ | Const_char _ | Const_string _
        | Const_float32 _ (* There is no float32 array optimization *)
        | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
    | Texp_function _
    | Texp_construct (_, {cstr_arity = 0}, _, _) ->
       `Constant_or_function
    | Texp_constant(Const_float _) ->
       if Config.flat_float_array
       then `Float_that_cannot_be_shortcut
       else `Constant_or_function
    | Texp_ident _ when lazy_val_requires_forward e.exp_env e.exp_loc e.exp_type ->
       `Identifier `Forward_value
    | Texp_ident _ ->
       `Identifier `Other
    | _ ->
       `Other

let value_kind_union (k1 : Lambda.value_kind) (k2 : Lambda.value_kind) =
  if Lambda.equal_value_kind k1 k2 then k1
    (* CR vlaviron: we could be more precise by comparing nullability and
       raw kinds separately *)
  else Lambda.generic_value

let rec layout_union l1 l2 =
  match l1, l2 with
  | Pbottom, l
  | l, Pbottom -> l
  | Pvalue layout1, Pvalue layout2 ->
      Pvalue (value_kind_union layout1 layout2)
  | Punboxed_float f1, Punboxed_float f2 ->
      if Primitive.equal_unboxed_float f1 f2 then l1 else Ptop
  | Punboxed_int bi1, Punboxed_int bi2 ->
      if Primitive.equal_unboxed_integer bi1 bi2 then l1 else Ptop
  | Punboxed_vector vi1, Punboxed_vector vi2 ->
      if Primitive.equal_unboxed_vector vi1 vi2 then l1 else Ptop
  | Punboxed_product layouts1, Punboxed_product layouts2 ->
      if List.compare_lengths layouts1 layouts2 <> 0 then Ptop
      else Punboxed_product (List.map2 layout_union layouts1 layouts2)
  | (Ptop | Pvalue _ | Punboxed_float _ | Punboxed_int _ |
     Punboxed_vector _ | Punboxed_product _),
    _ ->
      Ptop

(* Error report *)
open Format

let report_error ppf = function
  | Non_value_layout (ty, err) ->
      fprintf ppf
        "Non-value detected in [value_kind].@ Please report this error to \
         the Jane Street compilers team.";
      begin match err with
      | None ->
        fprintf ppf "@ Could not find cmi for: %a" Printtyp.type_expr ty
      | Some err ->
        fprintf ppf "@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err
      end
  | Sort_without_extension (sort, maturity, ty) ->
      fprintf ppf "Non-value layout %a detected" Jkind.Sort.format sort;
      begin match ty with
      | None -> ()
      | Some ty -> fprintf ppf " as sort for type@ %a" Printtyp.type_expr ty
      end;
      fprintf ppf
        ",@ but this requires extension %s, which is not enabled.@ \
         If you intended to use this layout, please add this flag to your \
         build file.@ \
         Otherwise, please report this error to the Jane Street compilers team."
        (Language_extension.to_command_line_string Layouts maturity)
  | Small_number_sort_without_extension (sort, ty) ->
      fprintf ppf "Non-value layout %a detected" Jkind.Sort.format sort;
      begin match ty with
      | None -> ()
      | Some ty -> fprintf ppf " as sort for type@ %a" Printtyp.type_expr ty
      end;
      let extension, verb, flags =
        match Language_extension.(is_at_least Layouts Stable),
              Language_extension.(is_enabled Small_numbers) with
        | false, true -> " layouts", "is", "this flag"
        | true, false -> " small_numbers", "is", "this flag"
        | false, false -> "s layouts and small_numbers", "are", "these flags"
        | true, true -> assert false
      in
      fprintf ppf
        ",@ but this requires the extension%s, which %s not enabled.@ \
         If you intended to use this layout, please add %s to your \
         build file.@ \
         Otherwise, please report this error to the Jane Street compilers team."
        extension verb flags
  | Simd_sort_without_extension (sort, ty) ->
      fprintf ppf "Non-value layout %a detected" Jkind.Sort.format sort;
      begin match ty with
      | None -> ()
      | Some ty -> fprintf ppf " as sort for type@ %a" Printtyp.type_expr ty
      end;
      let extension, verb, flags =
        match Language_extension.(is_at_least Layouts Stable),
              Language_extension.(is_at_least SIMD Stable) with
        | false, true -> " layouts", "is", "this flag"
        | true, false -> " simd", "is", "this flag"
        | false, false -> "s layouts and simd", "are", "these flags"
        | true, true -> assert false
      in
      fprintf ppf
        ",@ but this requires the extension%s, which %s not enabled.@ \
         If you intended to use this layout, please add %s to your \
         build file.@ \
         Otherwise, please report this error to the Jane Street compilers team."
        extension verb flags
  | Not_a_sort (ty, err) ->
      fprintf ppf "A representable layout is required here.@ %a"
        (Jkind.Violation.report_with_offender
           ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err
  | Unsupported_product_in_lazy const ->
      fprintf ppf
        "Product layout %a detected in [lazy] in [Typeopt.Layout]@ \
         Please report this error to the Jane Street compilers team."
        Jkind.Sort.Const.format const
  | Unsupported_vector_in_product_array ->
      fprintf ppf
        "Unboxed vector types are not yet supported in arrays of unboxed@ \
         products."
  | Unsupported_void_in_array ->
      fprintf ppf
        "Types whose layout contains [void] are not yet supported in arrays."
  | Mixed_product_array (const, elt_ty) ->
      fprintf ppf
        "An unboxed product array element must be formed from all@ \
         external types (which are ignored by the gc) or all gc-scannable \
         types.@ But this array operation is peformed for an array whose@ \
         element type is %a, which is an unboxed product@ \
         that is not external and contains a type with the non-scannable@ \
         layout %a.@ \
         @[Hint: if the array contents should not be scanned, annotating@ \
         contained abstract types as [mod external] may resolve this error.@]"
        Printtyp.type_expr elt_ty
        Jkind.Sort.Const.format const
  | Product_iarrays_unsupported ->
      fprintf ppf
        "Immutable arrays of unboxed products are not yet supported."
  | Opaque_array_non_value { array_type; elt_kinding_failure }  ->
      begin match elt_kinding_failure with
      | Some (ty, err) ->
        fprintf ppf
        "This array operation cannot tell whether %a is an array type,@ \
         possibly because it is abstract. In this case, the element type@ \
         %a must be a value:@ @\n@[%a@]"
          Printtyp.type_expr array_type
          Printtyp.type_expr ty
          (Jkind.Violation.report_with_offender
            ~offender:(fun ppf -> Printtyp.type_expr ppf ty)) err
      | None ->
        fprintf ppf
          "This array operation expects an array type, but %a does not appear@ \
           to be one.@ (Hint: it is abstract?)"
          Printtyp.type_expr array_type;
      end

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
