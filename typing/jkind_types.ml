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
    | Mask

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

  (* Same as [Btype.generic_level]. Like for types, generic variables are
     treated as flexible, but have special treatment in e.g. [instance]. *)
  let generic_level = Ident.highest_scope

  (* Same as [Ctype.subject_level]. Rigid, so if [v.level = subject_level],
     then [v.contents] is always [None]. *)
  let subject_level = generic_level - 1

  type t =
    | Var of var
    | Base of base
    | Product of t list
    | Univar of univar
    | Addressable of t

  and var =
    { mutable contents : t option;
      mutable level : int;
      id : int
    }

  let is_genvar var =
    assert (Option.is_none var.contents);
    var.level = generic_level

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
    | Vec512, Vec512
    | Mask, Mask ->
      true
    | ( ( Void | Scannable | Untagged_immediate | Float64 | Float32 | Word
        | Bits8 | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask ),
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
    | Mask -> "mask"

  let base_is_addressable = function
    | Scannable | Word | Bits64 | Vec128 | Vec256 | Vec512 | Mask -> true
    | Void | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16 | Bits32 ->
      false

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
      | Addressable of t

    let base b = Base b

    let product cs = Product cs

    let univar uv = Univar uv

    let genvar v = Genvar v

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | Addressable c1, Addressable c2 ->
        (* Relies on constants not having redundant addressability wrappers. *)
        equal c1 c2
      | (Base _ | Product _ | Univar _ | Genvar _ | Addressable _), _ -> false

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
        | Addressable c ->
          Fmt.fprintf ppf "%a addressable" (pp_element ~nested:true) c
      in
      pp_element ~nested:false ppf c

    let rec all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 | Mask ) ->
        false
      | Univar _ -> Misc.fatal_error "Sort.Const.all_void: Univar"
      | Genvar _ -> Misc.fatal_error "Sort.Const.all_void: Genvar"
      | Product ts -> List.for_all all_void ts
      | Addressable t ->
        (* CR box: This may have to be updated once addressability affects boxed
           representations *)
        all_void t

    let rec is_surely_addressable = function
      | Base b -> base_is_addressable b
      | Product cs -> List.for_all is_surely_addressable cs
      | Univar _ | Genvar _ -> false
      | Addressable _ -> true

    (* Maintains invariant of no redundant [Addressable] constructors in
       constants *)
    let addressable c = if is_surely_addressable c then c else Addressable c

    let rec maybe_all_void = function
      | Base Void -> true
      | Base
          ( Scannable | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16
          | Bits32 | Bits64 | Word | Vec128 | Vec256 | Vec512 | Mask ) ->
        false
      | Univar _ | Genvar _ -> true
      | Product ts -> List.for_all maybe_all_void ts
      | Addressable t -> maybe_all_void t

    let rec is_concrete = function
      | Base _ -> true
      | Product ts -> List.for_all is_concrete ts
      | Univar _ | Genvar _ -> false
      | Addressable t -> is_concrete t

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

    let mask = Base Mask

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
              | Vec512 -> "Vec512"
              | Mask -> "Mask")
          | Product cs ->
            let pp_sep ppf () = Format.fprintf ppf "@ , " in
            Format.fprintf ppf "Product [%a]"
              (Misc.pp_nested_list ~nested ~pp_element ~pp_sep)
              cs
          | Univar { name = Some n } -> Format.fprintf ppf "Univar '%s" n
          | Univar { name = None } -> Format.fprintf ppf "Univar '_"
          | Genvar v -> Format.fprintf ppf "Genvar %d" v.id
          | Addressable c ->
            Format.fprintf ppf "Addressable (%a)" (pp_element ~nested:false) c
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

    let some_mask = Some mask

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
      | Mask -> some_mask

    let[@inline] some : t -> t option = function
      | Base b -> some_of_base b
      | (Product _ | Univar _ | Genvar _ | Addressable _) as t -> Some t
  end

  module Var = struct
    type id = int

    let get_id { id; _ } = id

    let is_cmi_var { id; _ } = id < 0

    let is_root { contents; _ } = Option.is_none contents

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
        | Vec512 -> "Vec512"
        | Mask -> "Mask")

    let rec t ppf = function
      | Var v -> fprintf ppf "Var %a" var v
      | Base b -> base ppf b
      | Product ts ->
        fprintf ppf "Product [ %a ]"
          (pp_print_list ~pp_sep:(fun ppf () -> pp_print_text ppf "; ") t)
          ts
      | Univar { name = Some n } -> fprintf ppf "Univar '%s" n
      | Univar { name = None } -> fprintf ppf "Univar '_"
      | Addressable s -> fprintf ppf "Addressable (%a)" t s

    and opt_t ppf = function
      | Some s -> fprintf ppf "Some %a" t s
      | None -> fprintf ppf "None"

    and var ppf v =
      fprintf ppf "{@[@ contents = %a;@ level = %d;@ id = %d@ @]}" opt_t
        v.contents v.level v.id
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
    | Var v -> (
      match v.contents with
      | Some t -> update_level level t
      | None when level < v.level ->
        log_change (v, Clevel v.level);
        v.level <- level
      | None -> ())
    | Base _ | Univar _ -> ()
    | Product ts -> List.iter (update_level level) ts
    | Addressable t -> update_level level t

  let[@inline] update_contents (v : var) (contents : t option) =
    if v.contents != contents
    then (
      log_change (v, Ccontents v.contents);
      v.contents <- contents)

  let[@inline] equate_var (v : var) (t : t) =
    assert (Option.is_none v.contents);
    (* Variables at [subject_level] are rigid. *)
    if v.level != subject_level
    then (
      update_level v.level t;
      update_contents v (Some t);
      true)
    else false

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

      let mask = Base Mask

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
        | Mask -> mask

      let rec of_const : Const.t -> t = function
        | Base b -> of_base b
        | Product cs -> Product (List.map of_const cs)
        | Univar uv -> Univar uv
        | Genvar v -> Var v
        | Addressable c -> Addressable (of_const c)
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

      let mask = Base Mask

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
        | Mask -> mask
    end
  end

  let of_var v = Var v

  let last_var_id = ref 0

  let last_var_cmi_id = ref 0

  let reset_cmi_sort_id () = last_var_cmi_id := 0

  let new_var ~level =
    assert (level >= 0 && level <= generic_level);
    incr last_var_id;
    { contents = None; level; id = !last_var_id }

  let new_genvar () = new_var ~level:generic_level

  let new_genvar_for_cmi () =
    decr last_var_cmi_id;
    { contents = None; level = generic_level; id = !last_var_cmi_id }

  let instance_map : (var * var) list ref = ref []

  let instance_with ~level vars f =
    let new_vars =
      List.map
        (fun v ->
          assert (is_genvar v);
          (* ensure the variable is not a CMI serialised variable *)
          assert (v.id > 0);
          let v' = new_var ~level in
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
      | None -> Misc.fatal_error "Jkind_types.instance_var: free genvar"
      end
    | None -> Var v
    | Some t -> instance t

  and instance : t -> t = function
    | Var v -> instance_var v
    | (Base _ | Univar _) as s -> s
    | Product ts -> Product (List.map instance ts)
    | Addressable s -> Addressable (instance s)

  let rec get : t -> t = function
    | (Base _ | Univar _) as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Addressable s as t ->
      let s' = get s in
      if s' == s then t else Addressable s'
    | Var r as t -> (
      match r.contents with
      | None -> t
      | Some s ->
        let result = get s in
        (* path compression *)
        if result != s then update_contents r (Some result);
        result)

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
    | Addressable t -> Addressable (subst s t)

  (** List of variables generalized so far during a call to [generalize_with],
      [None] otherwise. *)
  let generalized : var list ref option ref = ref None

  (** All free sort variables above the [current_level] are generalized: their
      level is set to [generic_level]. *)
  let generalize ~current_level sort =
    match !generalized with
    | None -> () (* Not in generalization context *)
    | Some vars_ref ->
      let rec loop sort =
        match sort with
        | Var v ->
          assert (Option.is_none v.contents);
          if v.level > current_level && v.level <> generic_level
          then begin
            v.level <- generic_level;
            vars_ref := v :: !vars_ref
          end
        | Product sorts -> List.iter loop sorts
        | Addressable sort -> loop sort
        | Base _ | Univar _ -> ()
      in
      loop (get sort)

  (** Calls [f] with sort variable generalization enabled, returning its result
      and sort variables generalized during the call. *)
  let generalize_with f =
    let vars_ref = ref [] in
    match !generalized with
    | None ->
      generalized := Some vars_ref;
      let result = Misc.try_finally f ~always:(fun () -> generalized := None) in
      result, List.rev !vars_ref
    | Some _ ->
      Misc.fatal_error "Jkind_types.generalize_with: nested generalize"

  let rec to_const_opt : t -> Const.t option = function
    | Base b -> Some (Static.Const.of_base b)
    | Product ts ->
      Misc.Stdlib.List.map_option to_const_opt ts
      |> Option.map (fun cs : Const.t -> Const.Product cs)
    | Univar uv -> Some (Univar uv)
    | Var r -> (
      match r.contents with None -> None | Some s -> to_const_opt s)
    | Addressable s -> Option.map Const.addressable (to_const_opt s)

  let is_scannable_or_var s =
    let rec go = function
      | Base Scannable | Var _ -> true
      | Addressable s -> go s
      | Base _ | Product _ | Univar _ -> false
    in
    go (get s)

  (***********************)
  (* equality *)

  let rec constrain_addressable ~allow_mutation : t -> bool = function
    | Addressable _ -> true
    | Base b -> base_is_addressable b
    | Product ts -> List.for_all (constrain_addressable ~allow_mutation) ts
    | Univar _ -> false
    | Var v -> (
      match v.contents with
      | Some s -> constrain_addressable ~allow_mutation s
      | None when not allow_mutation -> false
      | None -> equate_var v (Addressable (Var (new_var ~level:generic_level))))

  let is_surely_addressable = constrain_addressable ~allow_mutation:false

  let rec strip_head_addressable : t -> t = function
    | Addressable s -> strip_head_addressable s
    | Var { contents = Some s; _ } as t ->
      let s' = strip_head_addressable s in
      if s' == s then t else s'
    | (Var _ | Base _ | Product _ | Univar _) as t -> t

  let rec equate ~allow_mutation s1 s2 =
    match s1, s2 with
    | Var v1, Var v2 when v1.id = v2.id -> true
    | Var { contents = Some s1 }, _ -> equate ~allow_mutation s1 s2
    | _, Var { contents = Some s2 } -> equate ~allow_mutation s1 s2
    | Var v1, Var v2 when v1.level < v2.level -> equate ~allow_mutation s2 s1
    | Var ({ contents = None } as v1), _ -> allow_mutation && equate_var v1 s2
    | _, Var ({ contents = None } as v2) -> allow_mutation && equate_var v2 s1
    | Addressable _, _ | _, Addressable _ ->
      (* We reduce the problem to [s1 addressable = s2 addressable], since if
         one side is addressable, then the other is too. At this point we
         proceed by proving [s1 = s2], which is incomplete:

         Consider [s1 = 'var addressable] and [s2 = bits8 addressable].
         We could unify ['var = bits8] or ['var = bits8 addressable], but
         neither is more general. *)
      constrain_addressable ~allow_mutation s1
      && constrain_addressable ~allow_mutation s2
      && equate ~allow_mutation
           (strip_head_addressable s1)
           (strip_head_addressable s2)
    | Base b1, Base b2 -> equal_base b1 b2
    | Product sorts1, Product sorts2 -> (
      try List.for_all2 (equate ~allow_mutation) sorts1 sorts2
      with Invalid_argument _ -> false)
    | Univar uv1, Univar uv2 -> equal_univar_univar uv1 uv2
    | _, (Base _ | Product _ | Univar _) -> false

  let decompose_into_product t n =
    let ts = List.init n (fun _ -> of_var (new_var ~level:generic_level)) in
    if equate ~allow_mutation:true t (Product ts) then Some ts else None

  (*** defaulting ***)

  let rec default_to_scannable_and_get (s : t) : Const.t =
    match s with
    | Base b -> Static.Const.of_base b
    | Product ts -> Product (List.map default_to_scannable_and_get ts)
    | Univar uv -> Univar uv
    | Var { contents = Some s } -> default_to_scannable_and_get s
    | Var ({ contents = None } as v) ->
      if is_genvar v
      then Genvar v
      else if equate ~allow_mutation:true s Static.T.scannable
      then Static.Const.scannable
      else
        Misc.fatal_error
          "Jkind_types.default_to_scannable_and_get: cannot default rigid \
           variables"
    | Addressable s -> Const.addressable (default_to_scannable_and_get s)

  let get_concrete_defaulting_to_scannable s =
    let const = default_to_scannable_and_get s in
    if Const.is_concrete const then Const.some const else None

  (* CR layouts v12: Default to void instead. *)
  let default_for_transl_and_get s = default_to_scannable_and_get s

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
      | Addressable s when is_surely_addressable s -> pp_element ~nested ppf s
      | Addressable s ->
        Fmt.fprintf ppf "%a addressable" (pp_element ~nested:true) s
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

module Kind_operator = struct
  type t =
    | Id
    | Addressable

  let equal t1 t2 =
    match t1, t2 with
    | Id, Id | Addressable, Addressable -> true
    | (Id | Addressable), _ -> false

  let compose t1 t2 =
    match t1, t2 with
    | Id, t | t, Id -> t
    | Addressable, Addressable -> Addressable
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

module Layout = struct
  open Jkind_axis

  type 'sort t =
    | Sort of 'sort * Scannable_axes.t
    | Product of 'sort t list
    | Any of Scannable_axes.t
    | Addressable of 'sort t

  module Const = struct
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base * Scannable_axes.t
      | Product of t list
      | Univar of Sort.univar
      | Genvar of Sort.var
      | Addressable of t

    let any sa = Any sa

    let product cs = Product cs

    let univar uv = Univar uv

    let genvar v = Genvar v

    let max = Any Scannable_axes.max

    let rec equal c1 c2 =
      match c1, c2 with
      | Base (Scannable, sa1), Base (Scannable, sa2) ->
        Scannable_axes.equal sa1 sa2
      | Base (b1, _), Base (b2, _) -> Sort.equal_base b1 b2
      | Any sa1, Any sa2 -> Scannable_axes.equal sa1 sa2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> Sort.equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | Addressable c1, Addressable c2 ->
        (* Relies on invariant that constants don't have redundant [Addressable] *)
        equal c1 c2
      | (Base _ | Any _ | Product _ | Univar _ | Genvar _ | Addressable _), _ ->
        false

    let rec get_sort : t -> Sort.Const.t option = function
      | Any _ -> None
      | Base (b, _) -> Sort.Const.some (Base b)
      | Product ts ->
        Option.map
          (fun x -> Sort.Const.Product x)
          (Misc.Stdlib.List.map_option get_sort ts)
      | Univar uv -> Some (Sort.Const.Univar uv)
      | Genvar v -> Some (Sort.Const.Genvar v)
      | Addressable t -> Option.map Sort.Const.addressable (get_sort t)

    let rec is_scannable_or_any = function
      | Any _ | Base (Scannable, _) -> true
      | Base
          ( ( Void | Untagged_immediate | Float64 | Float32 | Word | Bits8
            | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask ),
            _ ) ->
        false
      | Product _ -> false
      | Univar _ -> false
      | Genvar _ -> false
      | Addressable t -> is_scannable_or_any t

    let rec is_surely_addressable = function
      | Base (b, _) -> Sort.base_is_addressable b
      | Product cs -> List.for_all is_surely_addressable cs
      | Any _ | Univar _ | Genvar _ -> false
      | Addressable _ -> true

    let addressable c = if is_surely_addressable c then c else Addressable c

    let apply_operator c : Kind_operator.t -> t = function
      | Id -> c
      | Addressable -> addressable c

    let rec get_root_scannable_axes t =
      match t with
      | Any sa -> Some sa
      | Base (_, sa) -> if is_scannable_or_any t then Some sa else None
      | Product _ -> None
      | Univar _ -> None
      | Genvar _ -> None
      | Addressable t -> get_root_scannable_axes t

    let rec set_root_scannable_axes t sa =
      match t with
      | Any _ -> Any sa
      | Base (b, _) -> if is_scannable_or_any t then Base (b, sa) else t
      | Product _ -> t
      | Univar _ -> t
      | Genvar _ -> t
      | Addressable t' -> Addressable (set_root_scannable_axes t' sa)

    let meet_root_scannable_axes t sa =
      match get_root_scannable_axes t with
      | None -> t
      | Some sa' -> set_root_scannable_axes t (Scannable_axes.meet sa sa')

    module Static = struct
      let scannable_non_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer } )

      let scannable_non_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Non_pointer64 } )

      let scannable_non_null_non_float =
        Base
          (Sort.Scannable, { nullability = Non_null; separability = Non_float })

      let scannable_non_null_separable =
        Base
          (Sort.Scannable, { nullability = Non_null; separability = Separable })

      let scannable_non_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Non_null; separability = Maybe_separable } )

      let scannable_maybe_null_non_pointer =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer } )

      let scannable_maybe_null_non_pointer64 =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_pointer64 } )

      let scannable_maybe_null_non_float =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Non_float } )

      let scannable_maybe_null_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Separable } )

      let scannable_maybe_null_maybe_separable =
        Base
          ( Sort.Scannable,
            { nullability = Maybe_null; separability = Maybe_separable } )

      (* For all non-[Scannable] layouts, the scannable axes are ignored. We
         have to pick something, though, so we pick [Scannable_axes.max]. *)

      let void = Base (Sort.Void, Scannable_axes.max)

      let float64 = Base (Sort.Float64, Scannable_axes.max)

      let float32 = Base (Sort.Float32, Scannable_axes.max)

      let word = Base (Sort.Word, Scannable_axes.max)

      let untagged_immediate = Base (Sort.Untagged_immediate, Scannable_axes.max)

      let bits8 = Base (Sort.Bits8, Scannable_axes.max)

      let bits16 = Base (Sort.Bits16, Scannable_axes.max)

      let bits32 = Base (Sort.Bits32, Scannable_axes.max)

      let bits64 = Base (Sort.Bits64, Scannable_axes.max)

      let vec128 = Base (Sort.Vec128, Scannable_axes.max)

      let vec256 = Base (Sort.Vec256, Scannable_axes.max)

      let vec512 = Base (Sort.Vec512, Scannable_axes.max)

      let mask = Base (Sort.Mask, Scannable_axes.max)

      let of_base (b : Sort.base) (sa : Scannable_axes.t) =
        match b, sa with
        | Scannable, sa -> (
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
        | Void, _ -> void
        | Untagged_immediate, _ -> untagged_immediate
        | Float64, _ -> float64
        | Float32, _ -> float32
        | Word, _ -> word
        | Bits8, _ -> bits8
        | Bits16, _ -> bits16
        | Bits32, _ -> bits32
        | Bits64, _ -> bits64
        | Vec128, _ -> vec128
        | Vec256, _ -> vec256
        | Vec512, _ -> vec512
        | Mask, _ -> mask
    end

    let of_sort s sa =
      let rec of_sort (s : Sort.t) sa =
        match s with
        | Var v when Sort.is_genvar v -> Some (Genvar v)
        | Var _ -> None
        | Base b -> Some (Static.of_base b sa)
        | Product sorts ->
          Option.map
            (fun x -> Product x)
            (* [Sort.get] is deep, so no need to repeat it here *)
            (* In all cases where sort products are turned into layout products,
               [Scannable_axes.max] is used. The sort product doesn't store
               enough information to make any other choice. *)
            (Misc.Stdlib.List.map_option
               (fun s -> of_sort s Scannable_axes.max)
               sorts)
        | Univar uv -> Some (Univar uv)
        | Addressable s -> Option.map addressable (of_sort s sa)
      in
      of_sort (Sort.get s) sa

    let of_univar uv = Univar uv

    let of_flat_sort (s : Sort.Flat.t) sa =
      match s with
      | Var _ -> None
      | Genvar v -> Some (Genvar v)
      | Univar uv -> Some (of_univar uv)
      | Base b -> Some (Static.of_base b sa)
  end

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any sa -> Any sa
    | Base (b, sa) -> Sort (Sort.of_base b, sa)
    | Product cs -> Product (List.map of_const cs)
    | Univar uv -> Sort (Sort.Univar uv, Scannable_axes.max)
    | Genvar v -> Sort (Sort.Var v, Scannable_axes.max)
    | Addressable c -> Addressable (of_const c)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> Product lays

  let apply_operator t : Kind_operator.t -> _ t = function
    | Id -> t
    | Addressable -> Addressable t

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any sa -> Some (Any sa)
    | Sort (s, sa) -> of_sort s sa
    | Product layouts ->
      Option.map
        (fun x -> Const.Product x)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)
    | Addressable t -> Option.map Const.addressable (get_const of_sort t)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let of_new_sort_var ~level sa =
    let sort = Sort.(of_var (new_var ~level)) in
    Sort (sort, sa), sort
end
