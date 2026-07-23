(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Sort = struct
  type base =
    | Void
    | Scannable
    | Untagged_immediate
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128
    | Vec256
    | Vec512

  type univar = { name : string option }

  (* Tracking univar correspondences for Trepr unification *)
  let univar_pairs : (univar * univar) list ref = ref []

  let equal_univar_univar uv1 uv2 =
    uv1 == uv2
    || List.exists
         (fun (p1, p2) -> (p1 == uv1 && p2 == uv2) || (p1 == uv2 && p2 == uv1))
         !univar_pairs

  (* Establish correspondence between sort univars positionally.
     Since Trepr respects order, we just pair them up directly. *)
  let enter_repr pairs f =
    let old_univars = !univar_pairs in
    univar_pairs := pairs @ old_univars;
    Misc.try_finally f ~always:(fun () -> univar_pairs := old_univars)

  (* Special sentinel levels stored in [var.level] when [contents = None]:
     - [level_generic]: a generalized sort variable (genvar), used for layout
       polymorphism and must be quantified. That is, they can only appear under
       [instance_map], etc.)
     - [level_rigid]: a rigid sort variable that cannot be unified.
     - [level_fresh]: a freshly-created unifiable sort variable whose level has
       not yet been set; it will be lowered via [update_level] as soon as it is
       unified with another variable.
     When [contents = Some t], [level] is meaningless. *)
  (* CR-soon zqian: Add the invariant that, when [contents = Some v], we have
    [level >= v.level]. This can improve performance. *)
  let level_generic = Ident.highest_scope

  let level_rigid = Ident.highest_scope - 1

  let level_fresh = Ident.highest_scope - 2

  type t =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar

  and var =
    { mutable contents : t option;
      mutable level : int;  (** See comments on [level_generic] *)
      id : int
    }

  let is_rigidvar var =
    assert (Option.is_none var.contents);
    var.level = level_rigid

  let is_genvar var =
    assert (Option.is_none var.contents);
    var.level = level_generic

  let equal_base b1 b2 =
    match b1, b2 with
    | Void, Void
    | Scannable, Scannable
    | Untagged_immediate, Untagged_immediate
    | Float64, Float64
    | Float32, Float32
    | Word, Word
    | Bits8, Bits8
    | Bits16, Bits16
    | Bits32, Bits32
    | Bits64, Bits64
    | Vec128, Vec128
    | Vec256, Vec256
    | Vec512, Vec512 ->
      true
    | ( ( Void | Scannable | Untagged_immediate | Float64 | Float32 | Word
        | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
        _ ) ->
      false

  let to_string_base = function
    | Scannable -> "value" (* printed as "value" to users *)
    | Void -> "void"
    | Untagged_immediate -> "untagged_immediate"
    | Float64 -> "float64"
    | Float32 -> "float32"
    | Word -> "word"
    | Bits8 -> "bits8"
    | Bits16 -> "bits16"
    | Bits32 -> "bits32"
    | Bits64 -> "bits64"
    | Vec128 -> "vec128"
    | Vec256 -> "vec256"
    | Vec512 -> "vec512"

  (* Global association list mapping poly vars to names for printing *)
  let sort_poly_var_names : (var * string) list ref = ref []

  let to_string_genvar v =
    (* CR-soon zqian: raise if [v] is not found in [sort_poly_var_names],
       i.e. if this is called outside the dynamic extent of
       [print_with_genvars]. *)
    match List.assq_opt v !sort_poly_var_names with
    | Some name -> name
    | None -> "<genvar>"

  let print_with_genvar (v : var) callback =
    let saved = !sort_poly_var_names in
    let is_used s = List.exists (fun (_, name) -> name = s) saved in
    let find_name s =
      let rec loop idx =
        let name = s ^ string_of_int idx in
        if is_used name then loop (idx + 1) else name
      in
      if is_used s then loop 0 else s
    in
    let name = find_name "l" in
    sort_poly_var_names := (v, name) :: saved;
    Misc.try_finally
      (fun () -> callback name)
      ~always:(fun () -> sort_poly_var_names := saved)

  let print_with_genvars vars callback =
    let rec loop vars names_acc =
      match vars with
      | [] -> callback (List.rev names_acc)
      | v :: rest ->
        print_with_genvar v (fun name -> loop rest (name :: names_acc))
    in
    loop vars []

  module Const = struct
    type t =
      | Base of base
      | Product of t list
      | Univar of univar
      | Genvar of var

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | (Base _ | Product _ | Univar _ | Genvar _), _ -> false

    let format ppf c =
      let module Fmt = Format_doc in
      let rec pp_element ~nested ppf = function
        | Base b -> Fmt.fprintf ppf "%s" (to_string_base b)
        | Product cs ->
          let pp_sep ppf () = Fmt.fprintf ppf "@ & " in
          Fmt.pp_nested_list ~nested ~pp_element ~pp_sep ppf cs
        | Univar { name = Some n } -> Fmt.fprintf ppf "%s" n
        | Univar { name = None } -> Fmt.fprintf ppf "_"
        | Genvar v -> Fmt.fprintf ppf "%s" (to_string_genvar v)
      in
      pp_element ~nested:false ppf c

    let rec all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 ) ->
        false
      | Univar _ -> Misc.fatal_error "Sort.Const.all_void: Univar"
      | Genvar _ -> Misc.fatal_error "Sort.Const.all_void: Genvar"
      | Product ts -> List.for_all all_void ts

    let scannable = Base Scannable

    let untagged_immediate = Base Untagged_immediate

    let void = Base Void

    let float64 = Base Float64

    let float32 = Base Float32

    let word = Base Word

    let bits8 = Base Bits8

    let bits16 = Base Bits16

    let bits32 = Base Bits32

    let bits64 = Base Bits64

    let vec128 = Base Vec128

    let vec256 = Base Vec256

    let vec512 = Base Vec512

    module Debug_printers = struct
      let t ppf c =
        let rec pp_element ~nested ppf = function
          | Base b ->
            Format.fprintf ppf "%s"
              (match b with
              | Void -> "Void"
              | Scannable -> "Value"
              | Untagged_immediate -> "Untagged_immediate"
              | Float64 -> "Float64"
              | Float32 -> "Float32"
              | Word -> "Word"
              | Bits8 -> "Bits8"
              | Bits16 -> "Bits16"
              | Bits32 -> "Bits32"
              | Bits64 -> "Bits64"
              | Vec128 -> "Vec128"
              | Vec256 -> "Vec256"
              | Vec512 -> "Vec512")
          | Product cs ->
            let pp_sep ppf () = Format.fprintf ppf "@ , " in
            Format.fprintf ppf "Product [%a]"
              (Misc.pp_nested_list ~nested ~pp_element ~pp_sep)
              cs
          | Univar { name = Some n } -> Format.fprintf ppf "Univar '%s" n
          | Univar { name = None } -> Format.fprintf ppf "Univar '_"
          | Genvar v -> Format.fprintf ppf "Genvar %d" v.id
        in
        pp_element ~nested:false ppf c
    end

    let for_function = scannable

    let for_predef_scannable = scannable

    let for_block_element = scannable

    let for_boxed_record = scannable

    let for_object = scannable

    let for_lazy_body = scannable

    let for_tuple_element = scannable

    let for_variant_arg = scannable

    let for_instance_var = scannable

    let for_class_arg = scannable

    let for_module = scannable

    let for_tuple = scannable

    let for_array_comprehension_element = scannable

    let for_list_element = scannable

    let for_loop_index = scannable

    let for_constructor = scannable

    let for_boxed_variant = scannable

    let for_exception = scannable

    let for_type_extension = scannable

    let for_class = scannable

    let for_effect = scannable

    let for_continuation = scannable

    (* Pre-allocated [Some]-wrappings of the base sort constants, evaluated
       once at module initialization and shared by [some] /
       [some_of_base] to avoid allocating a fresh [Some] block per
       call. Not exposed: callers go through [some]. *)
    let some_scannable = Some scannable

    let some_void = Some void

    let some_untagged_immediate = Some untagged_immediate

    let some_float64 = Some float64

    let some_float32 = Some float32

    let some_word = Some word

    let some_bits8 = Some bits8

    let some_bits16 = Some bits16

    let some_bits32 = Some bits32

    let some_bits64 = Some bits64

    let some_vec128 = Some vec128

    let some_vec256 = Some vec256

    let some_vec512 = Some vec512

    let[@inline] some_of_base = function
      | Scannable -> some_scannable
      | Void -> some_void
      | Untagged_immediate -> some_untagged_immediate
      | Float64 -> some_float64
      | Float32 -> some_float32
      | Word -> some_word
      | Bits8 -> some_bits8
      | Bits16 -> some_bits16
      | Bits32 -> some_bits32
      | Bits64 -> some_bits64
      | Vec128 -> some_vec128
      | Vec256 -> some_vec256
      | Vec512 -> some_vec512

    let[@inline] some : t -> t option = function
      | Base b -> some_of_base b
      | (Product _ | Univar _ | Genvar _) as t -> Some t
  end

  module Var = struct
    type id = int

    let get_id { id; _ } = id

    let is_cmi_var { id; _ } = id < 0

    (* Map var ids to smaller numbers for more consistent printing. *)
    let next_id = ref 1

    let names : (int, int) Hashtbl.t = Hashtbl.create 16

    let get_print_number id =
      match Hashtbl.find_opt names id with
      | Some n -> n
      | None ->
        let counter = !next_id in
        incr next_id;
        Hashtbl.add names id counter;
        counter

    let name { id; _ } =
      "'_representable_layout_" ^ Int.to_string (get_print_number id)
  end

  (*** debug printing **)
  module Debug_printers = struct
    open Format

    let base ppf b =
      fprintf ppf "%s"
        (match b with
        | Void -> "Void"
        | Scannable -> "Value"
        | Untagged_immediate -> "Untagged_immediate"
        | Float64 -> "Float64"
        | Float32 -> "Float32"
        | Word -> "Word"
        | Bits8 -> "Bits8"
        | Bits16 -> "Bits16"
        | Bits32 -> "Bits32"
        | Bits64 -> "Bits64"
        | Vec128 -> "Vec128"
        | Vec256 -> "Vec256"
        | Vec512 -> "Vec512")

    let rec t ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Base b -> base ppf b
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list ~pp_sep:(fun ppf () -> pp_print_text ppf "; ") t)
          ts
      | Univar { name = Some n } -> fprintf ppf "Univar '%s" n
      | Univar { name = None } -> fprintf ppf "Univar '_"

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ id = %d@ @]}" opt_t v.contents v.id
  end

  (* To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type sort_change =
    | Ccontents of t option
    | Clevel of int

  type change = var * sort_change

  let change_log : (change -> unit) ref = ref (fun _ -> ())

  let set_change_log cl = change_log := cl

  let log_change change = !change_log change

  let undo_change (v, ch) =
    match ch with
    | Ccontents t_op -> v.contents <- t_op
    | Clevel level -> v.level <- level

  let rec update_level level = function
    | Var v -> update_level_var level v
    | Base _ | Univar _ -> ()
    | Product ts -> List.iter (update_level level) ts

  and update_level_var level u =
    match u.contents with
    | Some t -> update_level level t
    | None ->
      let new_level = min level u.level in
      if u.level <> new_level
      then (
        log_change (u, Clevel u.level);
        u.level <- new_level)

  let[@inline] set_without_level : var -> t option -> unit =
   fun v t_op ->
    log_change (v, Ccontents v.contents);
    v.contents <- t_op

  let[@inline] set : var -> t option -> unit =
   fun v t_op ->
    assert (Option.is_none v.contents);
    (* [t_op] is always [Some _]. Takes [option] only for performance. *)
    let t = Option.get t_op in
    (* [v.level] is meaningful and should affect all variables in [t]. *)
    update_level v.level t;
    (* [v.contents] is set, which renders [v.level] meaningless, so we don't
       need to update that. *)
    set_without_level v t_op

  let[@inline] set_to_compress : var -> t option -> unit =
   fun v t_op ->
    assert (Option.is_some v.contents);
    (* [v.contents] is [Some _], hence [v.level] safe to ignore *)
    set_without_level v t_op

  module Static = struct
    (* Statically allocated values of various consts and sorts to save
       allocations in in the core hot path functions. [T] is also included in
       the outer module to provide the core sorts. *)

    module T = struct
      let void = Base Void

      let scannable = Base Scannable

      let untagged_immediate = Base Untagged_immediate

      let float64 = Base Float64

      let float32 = Base Float32

      let word = Base Word

      let bits8 = Base Bits8

      let bits16 = Base Bits16

      let bits32 = Base Bits32

      let bits64 = Base Bits64

      let vec128 = Base Vec128

      let vec256 = Base Vec256

      let vec512 = Base Vec512

      let of_base = function
        | Void -> void
        | Scannable -> scannable
        | Untagged_immediate -> untagged_immediate
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits8 -> bits8
        | Bits16 -> bits16
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
        | Vec256 -> vec256
        | Vec512 -> vec512

      let rec of_const : Const.t -> t = function
        | Base b -> of_base b
        | Product cs -> Product (List.map of_const cs)
        | Univar uv -> Univar uv
        | Genvar v -> Var v
    end

    module T_option = struct
      let scannable = Some T.scannable

      let void = Some T.void

      let untagged_immediate = Some T.untagged_immediate

      let float64 = Some T.float64

      let float32 = Some T.float32

      let word = Some T.word

      let bits8 = Some T.bits8

      let bits16 = Some T.bits16

      let bits32 = Some T.bits32

      let bits64 = Some T.bits64

      let vec128 = Some T.vec128

      let vec256 = Some T.vec256

      let vec512 = Some T.vec512

      let of_base = function
        | Void -> void
        | Scannable -> scannable
        | Untagged_immediate -> untagged_immediate
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits8 -> bits8
        | Bits16 -> bits16
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
        | Vec256 -> vec256
        | Vec512 -> vec512

      let rec of_const : Const.t -> t option = function
        | Base b -> of_base b
        | Product cs ->
          Option.map
            (fun x -> Product x)
            (Misc.Stdlib.List.map_option of_const cs)
        | Univar uv -> Some (Univar uv)
        | Genvar v -> Some (Var v)
    end

    module Const = struct
      open Const

      let scannable = Base Scannable

      let void = Base Void

      let untagged_immediate = Base Untagged_immediate

      let float64 = Base Float64

      let float32 = Base Float32

      let word = Base Word

      let bits8 = Base Bits8

      let bits16 = Base Bits16

      let bits32 = Base Bits32

      let bits64 = Base Bits64

      let vec128 = Base Vec128

      let vec256 = Base Vec256

      let vec512 = Base Vec512

      let of_base : base -> Const.t = function
        | Scannable -> scannable
        | Void -> void
        | Untagged_immediate -> untagged_immediate
        | Float64 -> float64
        | Float32 -> float32
        | Word -> word
        | Bits8 -> bits8
        | Bits16 -> bits16
        | Bits32 -> bits32
        | Bits64 -> bits64
        | Vec128 -> vec128
        | Vec256 -> vec256
        | Vec512 -> vec512
    end
  end

  let of_var v = Var v

  let last_var_id = ref 0

  let last_var_cmi_id = ref 0

  let reset_cmi_sort_id () = last_var_cmi_id := 0

  let new_var_unsafe ~level =
    incr last_var_id;
    { contents = None; level; id = !last_var_id }

  let new_var ~level =
    (* Guard against accidentally creating a genvar or rigidvar via this path:
       those require special handling (instance_map registration for genvars;
       refusal to unify for rigidvars). [level_fresh] is intentionally
       not guarded here — it behaves like any other unifiable variable and its
       level is simply lowered by [update_level] upon unification. *)
    if level >= level_rigid
    then Misc.fatal_error "Jkind_types.new_var: level >= level_rigid";
    new_var_unsafe ~level

  let new_genvar () = new_var_unsafe ~level:level_generic

  let new_genvar_for_cmi () =
    decr last_var_cmi_id;
    { contents = None; level = level_generic; id = !last_var_cmi_id }

  let new_rigidvar () = new_var_unsafe ~level:level_rigid

  let instance_map : (var * var) list ref = ref []

  let instance_with ~level vars f =
    let new_vars =
      List.map
        (fun v ->
          assert (is_genvar v);
          (* ensure the variable is not a CMI serialised variable *)
          assert (v.id > 0);
          let v' = new_var_unsafe ~level in
          v, v')
        vars
    in
    let old_map = !instance_map in
    instance_map := new_vars @ old_map;
    Misc.try_finally
      (fun () ->
        let result = f () in
        List.map snd new_vars, result)
      ~always:(fun () -> instance_map := old_map)

  let rec instance_var v =
    match v.contents with
    | None when is_genvar v ->
      begin match List.assq_opt v !instance_map with
      | Some v' -> Var v'
      | None ->
        (* If the caller didn't set up layout instantiation, conservatively
           return a rigid variable (which is not equal to anything) *)
        (* CR-someday zqian: explicitly distinguish among three cases:
        - instantiating layouts properly
        - knowingly instantiating to rigidvar conservatively
        - unknown context, in which case we should crash *)
        Var (new_rigidvar ())
      end
    | None -> Var v
    | Some t -> instance t

  and instance : t -> t = function
    | Var v -> instance_var v
    | (Base _ | Univar _) as s -> s
    | Product ts -> Product (List.map instance ts)

  let rec get : t -> t = function
    | (Base _ | Univar _) as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Var r as t -> (
      match r.contents with
      | None -> t
      | Some s ->
        let result = get s in
        if result != s then set_to_compress r (Some result);
        (* path compression *)
        result)

  let rec get_representable : t -> t option = function
    | (Base _ | Univar _) as t -> Some t
    | Product ts ->
      begin match get_representable_product ts with
      | None -> None
      | Some ts' -> Some (Product ts')
      end
    | Var v -> get_representable_var v

  and get_representable_product : t list -> t list option =
   fun ts ->
    List.fold_right
      (fun t acc ->
        match acc, get_representable t with
        | None, _ | _, None -> None
        | Some ts, Some t -> Some (t :: ts))
      ts (Some [])

  and get_representable_var : var -> t option =
   fun v ->
    match v.contents with
    | None ->
      begin if is_rigidvar v then Some (Var v) else None
      end
    | Some t -> get_representable t

  let rec subst s t =
    match t with
    | Var v ->
      begin match v.contents with
      | None ->
        begin match List.assq_opt v s with Some t -> t | None -> t
        end
      | Some t -> subst s t
      end
    | Base _ | Univar _ -> t
    | Product ts -> Product (List.map (subst s) ts)

  (* Sort generalization context for let poly_ *)
  let in_sort_generalization_context : var list ref option ref = ref None

  (* Generalize sort variables when in sort generalization context.
     This is called from Ctype.generalize when processing let poly_ bindings.
     For each free sort variable, the level is set to Ident.highest_scope,
     making it a generic sort variable (genvar), and the var is accumulated. *)
  let rec generalize_rec ~current_level ~vars_ref sort =
    match sort with
    | Var v ->
      assert (Option.is_none v.contents);
      if v.level > current_level && v.level <> Ident.highest_scope
      then begin
        v.level <- Ident.highest_scope;
        vars_ref := v :: !vars_ref
      end
    | Product sorts -> List.iter (generalize_rec ~current_level ~vars_ref) sorts
    | Base _ | Univar _ -> ()

  let generalize ~current_level sort =
    match !in_sort_generalization_context with
    | None -> () (* Not in generalization context *)
    | Some vars_ref -> generalize_rec ~current_level ~vars_ref (get sort)

  (* Wrapper to run a function in sort generalization context. Returns the
     result of [f] and the vars generalized during [f]. *)
  let generalize_with f =
    let vars_ref = ref [] in
    let old_context = !in_sort_generalization_context in
    in_sort_generalization_context := Some vars_ref;
    let result =
      Misc.try_finally f ~always:(fun () ->
          in_sort_generalization_context := old_context)
    in
    result, List.rev !vars_ref

  let rec default_to_scannable_and_get : t -> Const.t = function
    | Base b -> Static.Const.of_base b
    | Product ts -> Product (List.map default_to_scannable_and_get ts)
    | Univar uv -> Univar uv
    | Var r -> var_default_to_scannable_and_get r

  and var_default_to_scannable_and_get r : Const.t =
    match r.contents with
    | None when is_genvar r -> Genvar r
    | None when is_rigidvar r ->
      Misc.fatal_error
        "Jkind_types.var_default_to_scannable_and_get: cannot default rigid \
         variables"
    | None ->
      set r Static.T_option.scannable;
      Static.Const.scannable
    | Some s ->
      let result = default_to_scannable_and_get s in
      set_to_compress r (Static.T_option.of_const result);
      (* path compression *)
      result

  (* Like [default_to_scannable_and_get], but returns a [Some] wrapping. Reuses
     pre-allocated [Some] boxes when the result is one of the known base
     constants, to avoid an allocation per call site. *)
  let default_to_scannable_and_get_some s =
    Const.some (default_to_scannable_and_get s)

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_scannable_and_get s

  let is_scannable_or_var s =
    match get s with Base Scannable | Var _ -> true | _ -> false

  (***********************)
  (* equality *)

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  let swap_equate_result = function
    | Equal_mutated_first -> Equal_mutated_second
    | Equal_mutated_second -> Equal_mutated_first
    | (Unequal | Equal_no_mutation | Equal_mutated_both) as r -> r

  let[@inline] sorts_of_product s =
    (* In the equate functions, it's useful to pass around lists of sorts inside
       the product constructor they came from to avoid re-allocating it if we
       end up wanting to store it in a variable. We could probably eliminate the
       use of this by collapsing a bunch of the functions below into each other,
       but that would be much less readable. *)
    match s with
    | Product sorts -> sorts
    | Var _ | Base _ | Univar _ ->
      Misc.fatal_error "Jkind_types.sorts_of_product"

  let rec equate_sort_sort s1 s2 =
    match s1 with
    | Base b1 -> swap_equate_result (equate_sort_base s2 b1)
    | Var v1 -> equate_var_sort v1 s2
    | Product _ -> swap_equate_result (equate_sort_product s2 s1)
    | Univar uv1 -> swap_equate_result (equate_sort_univar s2 uv1)

  and equate_sort_base s1 b2 =
    match s1 with
    | Base b1 -> if equal_base b1 b2 then Equal_no_mutation else Unequal
    | Var v1 -> equate_var_base v1 b2
    | Product _ | Univar _ -> Unequal

  and equate_sort_univar s1 uv2 =
    match s1 with
    | Univar uv1 ->
      if equal_univar_univar uv1 uv2 then Equal_no_mutation else Unequal
    | Base _ | Product _ -> Unequal
    | Var v1 -> equate_var_univar v1 uv2

  and equate_var_univar v1 uv2 =
    match v1.contents with
    | Some s1 -> equate_sort_univar s1 uv2
    | None when is_rigidvar v1 -> Unequal
    | None ->
      set v1 (Some (Univar uv2));
      Equal_mutated_first

  and equate_var_base v1 b2 =
    match v1.contents with
    | Some s1 -> equate_sort_base s1 b2
    | None when is_rigidvar v1 -> Unequal
    | None ->
      set v1 (Static.T_option.of_base b2);
      Equal_mutated_first

  and equate_var_sort v1 s2 =
    match s2 with
    | Base b2 -> equate_var_base v1 b2
    | Var v2 -> equate_var_var v1 v2
    | Product _ -> equate_var_product v1 s2
    | Univar uv2 -> equate_var_univar v1 uv2

  and equate_var_var v1 v2 =
    if v1.id = v2.id (* equal id means physical equality *)
    then Equal_no_mutation
    else
      match v1.contents, v2.contents with
      | Some s1, _ -> swap_equate_result (equate_var_sort v2 s1)
      | _, Some s2 -> equate_var_sort v1 s2
      | None, None when not @@ is_rigidvar v1 ->
        set v1 (Some (of_var v2));
        Equal_mutated_first
      | None, None when not @@ is_rigidvar v2 ->
        set v2 (Some (of_var v1));
        Equal_mutated_second
      | None, None -> Unequal

  and equate_var_product v1 s2 =
    match v1.contents with
    | Some s1 -> equate_sort_product s1 s2
    | None when is_rigidvar v1 -> Unequal
    | None ->
      set v1 (Some s2);
      Equal_mutated_first

  and equate_sort_product s1 s2 =
    match s1 with
    | Base _ | Univar _ -> Unequal
    | Product sorts1 ->
      let sorts2 = sorts_of_product s2 in
      equate_sorts sorts1 sorts2
    | Var v1 -> equate_var_product v1 s2

  and equate_sorts sorts1 sorts2 =
    let rec go sorts1 sorts2 acc =
      match sorts1, sorts2 with
      | [], [] -> acc
      | sort1 :: sorts1, sort2 :: sorts2 -> (
        match equate_sort_sort sort1 sort2, acc with
        | Unequal, _ -> Unequal
        | _, Unequal -> assert false
        | Equal_no_mutation, acc | acc, Equal_no_mutation ->
          go sorts1 sorts2 acc
        | Equal_mutated_both, _ | _, Equal_mutated_both ->
          go sorts1 sorts2 Equal_mutated_both
        | Equal_mutated_first, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_first
        | Equal_mutated_second, Equal_mutated_second ->
          go sorts1 sorts2 Equal_mutated_second
        | Equal_mutated_first, Equal_mutated_second
        | Equal_mutated_second, Equal_mutated_first ->
          go sorts1 sorts2 Equal_mutated_both)
      | _, _ -> assert false
    in
    if List.compare_lengths sorts1 sorts2 = 0
    then go sorts1 sorts2 Equal_no_mutation
    else Unequal

  let equate_tracking_mutation = equate_sort_sort

  (* Don't expose whether or not mutation happened; we just need that for
     [Jkind] *)
  let equate s1 s2 =
    match equate_tracking_mutation s1 s2 with
    | Unequal -> false
    | Equal_mutated_first | Equal_mutated_second | Equal_no_mutation
    | Equal_mutated_both ->
      true

  let decompose_into_product t n =
    let ts = List.init n (fun _ -> of_var (new_var ~level:level_fresh)) in
    if equate t (Product ts) then Some ts else None

  (*** pretty printing ***)

  let format ppf t =
    let module Fmt = Format_doc in
    let rec pp_element ~nested ppf t =
      match get t with
      | Base b -> Fmt.fprintf ppf "%s" (to_string_base b)
      | Var v -> Fmt.fprintf ppf "%s" (Var.name v)
      | Product ts ->
        let pp_sep ppf () = Fmt.fprintf ppf " & " in
        Fmt.pp_nested_list ~nested ~pp_element ~pp_sep ppf ts
      | Univar { name = Some n } -> Fmt.fprintf ppf "%s" n
      | Univar { name = None } -> Fmt.fprintf ppf "_"
    in
    pp_element ~nested:false ppf t

  include Static.T

  module Flat = struct
    type t =
      | Var of Var.id
      | Genvar of var
      | Univar of univar
      | Base of base
  end
end

module Scannable_axes = struct
  open Jkind_axis

  type t =
    { nullability : Nullability.t;
      separability : Separability.t
    }

  let max = { nullability = Nullability.max; separability = Separability.max }

  let value_axes = { nullability = Non_null; separability = Separable }

  let equal { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    Nullability.equal n1 n2 && Separability.equal s1 s2

  let less_or_equal { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    Misc.Le_result.combine
      (Nullability.less_or_equal n1 n2)
      (Separability.less_or_equal s1 s2)

  let meet { nullability = n1; separability = s1 }
      { nullability = n2; separability = s2 } =
    { nullability = Nullability.meet n1 n2;
      separability = Separability.meet s1 s2
    }
end

(* Defined as [Addressability0] so that scopes under [open Jkind_axis] (which
   shadows [Addressability] with the module this one extends) can still refer
   to it. *)
module Addressability0 = struct
  include Jkind_axis.Addressability

  (* The intrinsic addressability of each base sort, as a reading: whether
     its types store all of their information in the data portion of a block
     when boxed. Kinds boxed as tagged immediates or as float blocks are not
     addressable ([Id]: only the identity action keeps them so). *)
  let of_base : Sort.base -> t = function
    | Scannable | Word | Bits64 | Vec128 | Vec256 | Vec512 -> Addressable
    | Void | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16 | Bits32 ->
      Id

  (* The intrinsic addressability of a sort, insofar as it is determined: the
     addressability of an unfilled sort variable is not yet known. *)
  let of_sort s =
    let rec go : Sort.t -> t = function
      | Base b -> of_base b
      | Product sorts -> combine_product (List.map go sorts)
      | Var _ | Univar _ -> Id_or_addressable
    in
    (* [Sort.get] is deep, so a returned [Var] is guaranteed unfilled. *)
    go (Sort.get s)
end

module Addressability = Addressability0

module Layout = struct
  open Jkind_axis
  module Addressability = Addressability0

  (* Addressability slots track applications of the [addressable] kind
     operator.

     [Any] and [Product] nodes (and [Types.Kconstr], and the [Const]
     counterparts below) carry a two-state [Addressability.Action.t]: either
     the operator was applied to the node ([Addressable]) or it wasn't
     ([Id]). An [Id] product root means the product is unmarked; its
     addressability is derived from its components at read time (a product of
     addressable kinds is addressable). Marking a whole product (e.g.
     [(bits8 & bits16) addressable]) is distinct from marking its components.
     [Id] on [Any] is the top kind [any].

     [Sort] nodes (and the [Const.Base], [Const.Univar], and [Const.Genvar]
     snapshots) carry a full [Addressability.t], whose third value
     [Id_or_addressable] denotes the JOIN of the layout and its marked form -
     over a product sort, the top of the whole fiber of kinds at that sort,
     the marks being unknown deeply through the components
     ([Addressability.decomposed_component] regenerates the join on each
     fabricated component).
     The join is introduced only for flexible bounds - fresh sort variables
     ([of_new_sort_var]), transfers of an [any] bound's action onto a
     variable, and the components fabricated when decomposing or flattening
     a product whose root slot is itself the join
     ([Addressability.decomposed_component]; exact roots get exactly-plain
     components) - which must admit both [L] and [L addressable] for
     whatever [L] the variable resolves to. The join persists after its
     variable resolves: sorts do not carry addressability, so filling the
     variable with [bits8] leaves the kind the join of [bits8] and
     [bits8 addressable]. Readers collapse the join only when the resolved
     sort is intrinsically addressable, where the two branches denote the
     same kind (see [Jkind.Layout.addressability]).

     Construction invariants:
     - A [Sort]/[Const.Base] node of an intrinsically addressable base
       ([Addressability.of_base]) normalizes its slot to [Addressable]
       (e.g. [bits64 addressable] IS [bits64]); see [Const.Static.of_base].
     - On an unaddressable base, [Id] and [Addressable] mean exactly the
       plain and the marked kind.
     - On an unfilled *flexible* variable the slot is [Id_or_addressable]
       or, once constrained by [any addressable], [Addressable]. The latter
       is a COMMITMENT, in the same family as sort defaulting: the variable's
       kind becomes exactly the marked kind of whatever sort it resolves to.
       At a base sort this loses nothing (there is exactly one addressable
       kind per unaddressable base), so the commitment is complete there. At
       a product sort it is deliberately incomplete: only the whole-marked
       product satisfies it, and a component-marked product - a different,
       incomparable addressable kind - is rejected (see
       [Addressability.decomposed_component] and
       testsuite/tests/typing-layouts-addressable/instantiate.ml). This
       keeps every question the checker asks about an EXACT kind; asking
       "is this some addressable kind at this sort?" would require a fourth
       slot value distinguishing the constraint from the mark, which can
       recover the completeness at products if it is ever needed. An [Id]
       slot on an unfilled variable arises only from *rigid* stand-ins
       (lpoly layout variables; see [Typetexp]) and the instance copies used
       to match them, and means exactly the plain kind: [x] and
       [x addressable] are incomparable while [x] is unknown, and readers
       collapse either to the marked kind once [x] resolves intrinsically
       addressable. Meets treat the rigid-unknown mismatch as "not
       definitely disjoint" ([Jkind.Layout.has_intersection]). *)
  type 'sort t =
    | Sort of 'sort * Scannable_axes.t * Addressability.t
    | Product of 'sort t list * Addressability.Action.t
    | Any of Scannable_axes.t * Addressability.Action.t

  module Const = struct
    type t =
      | Any of Scannable_axes.t * Addressability.Action.t
      | Base of Sort.base * Scannable_axes.t * Addressability.t
      | Product of t list * Addressability.Action.t
      (* [Univar] and [Genvar] carry their scannable axes and addressability
         so that a layout variable's bound survives a cmi round trip
         losslessly ([Subst.Prepare_for_saving] rebuilds every saved jkind
         through this type): in particular, for a rigid [x], the exact
         distinction between [x] and [x addressable] must be preserved. The
         slots are carried passively (root scannable-axis operations still
         skip these constructors, as before). *)
      | Univar of Sort.univar * Scannable_axes.t * Addressability.t
      | Genvar of Sort.var * Scannable_axes.t * Addressability.t

    let max = Any (Scannable_axes.max, Addressability.Action.Id)

    (* The addressability reading of a constant layout. A join slot on a base
       collapses only when the base is intrinsically addressable, where its
       branches coincide; it is otherwise PRESERVED - the join is
       information (e.g. a flexible [bits8] bound still admits
       [bits8 addressable]), and must survive [equal] (in particular through
       the builtin memoization used when saving cmis). Only printing elides
       the join ([Jkind.Addressability.to_string_list_diff]). An unmarked
       product derives its reading from its components. *)
    let rec addressability : t -> Addressability.t = function
      | Base (b, _, a) -> (
        match a with
        | Id_or_addressable -> (
          match Addressability.of_base b with
          | Addressable -> Addressable
          | Id | Id_or_addressable -> Id_or_addressable)
        | Addressable | Id -> a)
      | Any (_, a) -> Addressability.of_action_on_undetermined a
      | Product (ts, a) -> (
        match (a : Addressability.Action.t) with
        | Addressable -> Addressability.Addressable
        | Id -> Addressability.combine_product (List.map addressability ts))
      | Univar (_, _, a) | Genvar (_, _, a) ->
        (* The carrier is never resolved, so the slot is the reading:
           [Id] on a rigid variable means exactly the plain kind. *)
        a

    let rec equal c1 c2 =
      match c1, c2 with
      | Base (Scannable, sa1, _), Base (Scannable, sa2, _) ->
        (* [Scannable] is intrinsically addressable, so the addressability
           slots necessarily agree. *)
        Scannable_axes.equal sa1 sa2
      | (Base (b1, _, _) as c1), (Base (b2, _, _) as c2) ->
        Sort.equal_base b1 b2
        && Addressability.equal (addressability c1) (addressability c2)
      | Any (sa1, a1), Any (sa2, a2) ->
        Scannable_axes.equal sa1 sa2 && Addressability.Action.equal a1 a2
      | (Product (cs1, _) as c1), (Product (cs2, _) as c2) ->
        List.equal equal cs1 cs2
        (* Comparing the roots distinguishes a marked product of
           unaddressable kinds from the unmarked product; marking a product
           of addressable kinds does nothing. *)
        && Addressability.equal (addressability c1) (addressability c2)
      | Univar (uv1, sa1, a1), Univar (uv2, sa2, a2) ->
        Sort.equal_univar_univar uv1 uv2
        && Scannable_axes.equal sa1 sa2
        && Addressability.equal a1 a2
      | Genvar (v1, sa1, a1), Genvar (v2, sa2, a2) ->
        v1.id = v2.id
        && Scannable_axes.equal sa1 sa2
        && Addressability.equal a1 a2
      | (Base _ | Any _ | Product _ | Univar _ | Genvar _), _ -> false

    let rec get_sort : t -> Sort.Const.t option = function
      | Any _ -> None
      | Base (b, _, _) -> Sort.Const.some (Base b)
      | Product (ts, _) ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)
      | Univar (uv, _, _) -> Some (Sort.Const.Univar uv)
      | Genvar (v, _, _) -> Some (Sort.Const.Genvar v)

    let is_scannable_or_any = function
      | Any _ | Base (Scannable, _, _) -> true
      | Base
          ( ( Void | Untagged_immediate | Float64 | Float32 | Word | Bits8
            | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
            _,
            _ ) ->
        false
      | Product _ -> false
      | Univar _ -> false
      | Genvar _ -> false

    let get_root_scannable_axes t =
      match t with
      | Any (sa, _) -> Some sa
      | Base (_, sa, _) -> if is_scannable_or_any t then Some sa else None
      | Product _ -> None
      | Univar _ -> None
      | Genvar _ -> None

    let set_root_scannable_axes t sa =
      match t with
      | Any (_, a) -> Any (sa, a)
      | Base (b, _, a) -> if is_scannable_or_any t then Base (b, sa, a) else t
      | Product _ -> t
      | Univar _ -> t
      | Genvar _ -> t

    let meet_root_scannable_axes t sa =
      match get_root_scannable_axes t with
      | None -> t
      | Some sa' -> set_root_scannable_axes t (Scannable_axes.meet sa sa')

    (* Apply the [addressable] kind operator: an override of the root slot to
       [Addressable], not a meet. This does nothing to an already-addressable
       kind. *)
    let set_root_addressable t =
      match t with
      | Any (sa, _) -> Any (sa, Addressability.Action.Addressable)
      | Base (b, sa, _) -> Base (b, sa, Addressability.Addressable)
      | Product (ts, _) -> Product (ts, Addressability.Action.Addressable)
      | Univar (uv, sa, _) -> Univar (uv, sa, Addressability.Addressable)
      | Genvar (v, sa, _) -> Genvar (v, sa, Addressability.Addressable)

    module Static = struct
      let scannable_non_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer },
            Addressable )

      let scannable_non_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer64 },
            Addressable )

      let scannable_non_null_non_float =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_float },
            Addressable )

      let scannable_non_null_separable =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Separable },
            Addressable )

      let scannable_non_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Maybe_separable },
            Addressable )

      let scannable_maybe_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer },
            Addressable )

      let scannable_maybe_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer64 },
            Addressable )

      let scannable_maybe_null_non_float =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_float },
            Addressable )

      let scannable_maybe_null_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Separable },
            Addressable )

      let scannable_maybe_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Maybe_separable },
            Addressable )

      (* For all non-[Scannable] layouts, the scannable axes are ignored. We
         have to pick something, though, so we pick [Scannable_axes.max].
         The addressability slot of these constants is [Id] on intrinsically
         unaddressable bases (the plain, unmarked kind) and [Addressable] on
         intrinsically addressable ones ([Addressability.of_base]). *)

      let void = Base (Sort.Void, Scannable_axes.max, Id)

      let float64 = Base (Sort.Float64, Scannable_axes.max, Id)

      let float32 = Base (Sort.Float32, Scannable_axes.max, Id)

      let word = Base (Sort.Word, Scannable_axes.max, Addressable)

      let untagged_immediate =
        Base (Sort.Untagged_immediate, Scannable_axes.max, Id)

      let bits8 = Base (Sort.Bits8, Scannable_axes.max, Id)

      let bits16 = Base (Sort.Bits16, Scannable_axes.max, Id)

      let bits32 = Base (Sort.Bits32, Scannable_axes.max, Id)

      let bits64 = Base (Sort.Bits64, Scannable_axes.max, Addressable)

      let vec128 = Base (Sort.Vec128, Scannable_axes.max, Addressable)

      let vec256 = Base (Sort.Vec256, Scannable_axes.max, Addressable)

      let vec512 = Base (Sort.Vec512, Scannable_axes.max, Addressable)

      let of_base (b : Sort.base) (sa : Scannable_axes.t) (a : Addressability.t)
          =
        (* The addressability slot is normalized to [Addressable] on
           intrinsically addressable bases: applying [addressable] to such a
           kind does nothing (e.g. [bits64 addressable] IS [bits64]), and
           both branches of a join on them denote the same kind. *)
        match b, a with
        | Scannable, _ -> (
          match sa with
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_pointer
            } ->
            scannable_non_null_non_pointer
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_pointer64
            } ->
            scannable_non_null_non_pointer64
          | { nullability = Nullability.Non_null;
              separability = Separability.Non_float
            } ->
            scannable_non_null_non_float
          | { nullability = Nullability.Non_null;
              separability = Separability.Separable
            } ->
            scannable_non_null_separable
          | { nullability = Nullability.Non_null;
              separability = Separability.Maybe_separable
            } ->
            scannable_non_null_maybe_separable
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_pointer
            } ->
            scannable_maybe_null_non_pointer
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_pointer64
            } ->
            scannable_maybe_null_non_pointer64
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Non_float
            } ->
            scannable_maybe_null_non_float
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Separable
            } ->
            scannable_maybe_null_separable
          | { nullability = Nullability.Maybe_null;
              separability = Separability.Maybe_separable
            } ->
            scannable_maybe_null_maybe_separable)
        | Word, _ -> word
        | Bits64, _ -> bits64
        | Vec128, _ -> vec128
        | Vec256, _ -> vec256
        | Vec512, _ -> vec512
        | Void, Id -> void
        | Untagged_immediate, Id -> untagged_immediate
        | Float64, Id -> float64
        | Float32, Id -> float32
        | Bits8, Id -> bits8
        | Bits16, Id -> bits16
        | Bits32, Id -> bits32
        | ( ( Void | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
            | Bits32 ),
            ((Addressable | Id_or_addressable) as a) ) ->
          Base (b, Scannable_axes.max, a)
    end

    let of_sort s sa a =
      let rec of_sort (s : Sort.t) sa a =
        match s with
        | Var v when Sort.is_genvar v -> Some (Genvar (v, sa, a))
        | Var _ -> None
        | Base b -> Some (Static.of_base b sa a)
        | Product sorts ->
          let component_slot = Addressability.decomposed_component a in
          Option.map
            (fun x -> Product (x, Addressability.forget_join a))
            (* [Sort.get] is deep, so no need to repeat it here *)
            (* The sort product doesn't constrain the components' scannable
               axes, so they get [Scannable_axes.max]. The addressability
               slot stays on the product root as an action: it marks the
               product as a whole, not its components. A join slot (a
               flexible variable's bound later filled with a product sort)
               becomes an unmarked root deriving from join components, which
               reads the same; an exact slot has exactly-plain components
               ([Addressability.decomposed_component]). *)
            (Misc.Stdlib.List.map_option
               (fun s -> of_sort s Scannable_axes.max component_slot)
               sorts)
        | Univar uv -> Some (Univar (uv, sa, a))
      in
      of_sort (Sort.get s) sa a

    let of_univar uv sa a = Univar (uv, sa, a)

    let of_flat_sort (s : Sort.Flat.t) sa a =
      match s with
      | Var _ -> None
      | Genvar v -> Some (Genvar (v, sa, a))
      | Univar uv -> Some (of_univar uv sa a)
      | Base b -> Some (Static.of_base b sa a)
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any (sa, a) -> Any (sa, a)
    | Base (b, sa, a) -> Sort (Sort.of_base b, sa, a)
    | Product (cs, a) -> Product (List.map of_const cs, a)
    | Univar (uv, sa, a) -> Sort (Sort.Univar uv, sa, a)
    | Genvar (v, sa, a) -> Sort (Sort.Var v, sa, a)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> Product (lays, Addressability.Action.Id)

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any (sa, a) -> Some (Any (sa, a))
    | Sort (s, sa, a) -> of_sort s sa a
    | Product (layouts, a) ->
      Option.map
        (fun x -> Const.Product (x, a))
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let of_new_sort_var ~level sa =
    let sort = Sort.(of_var (new_var ~level)) in
    Sort (sort, sa, Addressability.Id_or_addressable), sort
end
