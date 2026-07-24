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
    | Addressable of t
        (** [Addressable s] is the sort [s addressable]. See the comment on this
            constructor in jkind_types.mli. *)

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

  (* An addressable sort is one whose types store all of their information
     in the data portion of a block when boxed, so that interior pointers
     into the box can address them. *)
  let base_is_addressable = function
    | Scannable | Word | Bits64 | Vec128 | Vec256 | Vec512 -> true
    | Void | Untagged_immediate | Float64 | Float32 | Bits8 | Bits16 | Bits32 ->
      false

  (* Whether [s] is known to be addressable. [false] means "not known to be
     addressable": the sort may be definitely unaddressable (e.g.
     [Base Bits8]) or not yet determined (an unfilled variable, or a
     univar). *)
  let rec is_known_addressable = function
    | Base b -> base_is_addressable b
    | Product ts -> List.for_all is_known_addressable ts
    | Addressable _ -> true
    | Univar _ -> false
    | Var v -> (
      match v.contents with None -> false | Some s -> is_known_addressable s)

  (* The [addressable] operator. Absorbs (returning the argument unchanged,
     without allocating) when the argument is already addressable, since
     [s addressable = s] for addressable [s]. This is the only way
     [Addressable] should be built. *)
  let addressable s = if is_known_addressable s then s else Addressable s

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
      | Addressable of t
          (** See the comment on [Sort.t]'s [Addressable] constructor in
              jkind_types.mli. *)

    let rec equal c1 c2 =
      match c1, c2 with
      | Base b1, Base b2 -> equal_base b1 b2
      | Product cs1, Product cs2 -> List.equal equal cs1 cs2
      | Univar uv1, Univar uv2 -> equal_univar_univar uv1 uv2
      | Genvar v1, Genvar v2 -> v1.id = v2.id
      | Addressable c1, Addressable c2 -> equal c1 c2
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
          pp_element ~nested:true ppf c;
          Fmt.fprintf ppf " addressable"
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
      (* The [addressable] operator does not change the unboxed
         representation (it only affects how the sort will be boxed), so
         [void addressable] still occupies no space unboxed. Nothing can
         construct a value of an all-void addressable sort today; revisit
         when the [box] operator lands. *)
      | Addressable s -> all_void s

    let rec is_known_addressable = function
      | Base b -> base_is_addressable b
      | Product ts -> List.for_all is_known_addressable ts
      | Addressable _ -> true
      | Univar _ | Genvar _ -> false

    (* See the comment on [Sort.addressable]. *)
    let addressable c = if is_known_addressable c then c else Addressable c

    let rec erase_addressable = function
      | Base _ as c -> c
      | Product ts as c ->
        let ts' = Misc.Stdlib.List.map_sharing erase_addressable ts in
        if ts == ts' then c else Product ts'
      | Univar _ as c -> c
      | Genvar _ as c -> c
      | Addressable c -> erase_addressable c

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
          | Addressable c ->
            Format.fprintf ppf "Addressable (%a)" (pp_element ~nested:true) c
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
      | (Product _ | Univar _ | Genvar _ | Addressable _) as t -> Some t
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
      | Addressable s -> fprintf ppf "Addressable (%a)" t s

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
    | Addressable s -> update_level level s

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
        | Addressable c -> Addressable (of_const c)
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
        | Addressable c -> Option.map (fun s -> Addressable s) (of_const c)
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
    | Addressable s -> addressable (instance s)

  let rec get : t -> t = function
    | (Base _ | Univar _) as t -> t
    | Product ts as t ->
      let ts' = List.map get ts in
      if List.for_all2 ( == ) ts ts' then t else Product ts'
    | Addressable s as t ->
      (* Re-normalize: a variable inside [s] may have been instantiated to
         an addressable sort since the wrapper was built, in which case the
         operator application is the identity and the wrapper is dropped. *)
      let s' = get s in
      if is_known_addressable s'
      then s'
      else if s' == s
      then t
      else Addressable s'
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
    | Addressable s -> Option.map addressable (get_representable s)
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
    | Addressable inner -> addressable (subst s inner)

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
    | Addressable s -> generalize_rec ~current_level ~vars_ref s
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
    (* Defaulting variables inside [s] (to [scannable], which is
       addressable) can make the operator application the identity, so
       re-normalize via [Const.addressable]. *)
    | Addressable s -> Const.addressable (default_to_scannable_and_get s)
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

  let rec is_scannable_or_var s =
    match get s with
    | Base Scannable | Var _ -> true
    (* [Addressable (Var v)] can still become [Scannable] (which is
       addressable, absorbing the wrapper), so it must keep behaving like a
       variable here. *)
    | Addressable s -> is_scannable_or_var s
    | _ -> false

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
    | Var _ | Base _ | Univar _ | Addressable _ ->
      Misc.fatal_error "Jkind_types.sorts_of_product"

  let rec equate_sort_sort s1 s2 =
    match s1 with
    | Base b1 -> swap_equate_result (equate_sort_base s2 b1)
    | Var v1 -> equate_var_sort v1 s2
    | Product _ -> swap_equate_result (equate_sort_product s2 s1)
    | Univar uv1 -> swap_equate_result (equate_sort_univar s2 uv1)
    | Addressable _ -> swap_equate_result (equate_sort_addressable s2 s1)

  and equate_sort_base s1 b2 =
    match s1 with
    | Base b1 -> if equal_base b1 b2 then Equal_no_mutation else Unequal
    | Var v1 -> equate_var_base v1 b2
    | Product _ | Univar _ -> Unequal
    | Addressable inner1 ->
      (* [inner1 addressable = b2] requires [b2] to be addressable (a sort
         made addressable is addressable, so it is distinct from every
         unaddressable sort), in which case absorption gives
         [inner1 addressable = b2] iff [inner1 = b2]. *)
      if base_is_addressable b2 then equate_sort_base inner1 b2 else Unequal

  and equate_sort_univar s1 uv2 =
    match s1 with
    | Univar uv1 ->
      if equal_univar_univar uv1 uv2 then Equal_no_mutation else Unequal
    | Base _ | Product _ -> Unequal
    (* A univar's addressability is unknown, so it cannot be shown equal to
       a sort made addressable. *)
    | Addressable _ -> Unequal
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
    | Addressable _ -> equate_var_addressable v1 s2

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

  and equate_sort_addressable s1 s2 =
    (* [s2] is headed by [Addressable]. Filling a variable under a wrapper
       can make the payload addressable and hence the wrapper redundant —
       including the payload becoming another wrapped sort, so that [s1] or
       [s2] reads as a nested [Addressable (Addressable _)] (which equals
       its payload). Comparing wrapped sorts by peeling one layer from each
       side would then be wrong: [A (A bits8)] and [A bits8] are equal, but
       their payloads [A bits8] and [bits8] are not. So first re-normalize
       both sides with [get], which prunes filled variables and removes
       redundant wrappers. *)
    let s1 = get s1 in
    let s2 = get s2 in
    match s1, s2 with
    | Addressable inner1, Addressable inner2 ->
      (* [get] guarantees neither payload is known to be addressable; in
         particular neither is itself wrapped. Comparing the payloads is
         then sound, because [addressable] is a function: [inner1 = inner2]
         implies [s1 = s2]. Incomplete: [s1] and [s2] can also be equal
         with [inner1 <> inner2], if instantiating variables makes both
         payloads addressable (so that both applications are the identity).
         We accept this; inference involving addressability is incomplete
         until the layers of the kind system are collapsed. *)
      equate_sort_sort inner1 inner2
    | Var v1, Addressable _ ->
      (* [get] pruned filled variables, so [v1] is unfilled. *)
      if is_rigidvar v1
      then Unequal
      else (
        set v1 (Some s2);
        Equal_mutated_first)
    | ((Base _ | Product _) as s1), Addressable inner2 ->
      (* [s1 = s2] requires [s1] to be addressable ([s2] is), in which case
         [s2] absorbs to [inner2] and [s1 = s2] iff [s1 = inner2]. See
         [equate_sort_product] for why an unknown-addressability product is
         conservatively [Unequal]. *)
      if is_known_addressable s1 then equate_sort_sort s1 inner2 else Unequal
    | Univar _, Addressable _ -> Unequal
    | _, (Var _ | Base _ | Product _ | Univar _) ->
      (* [s2]'s wrapper was redundant and [get] removed it. *)
      equate_sort_sort s1 s2

  and equate_var_addressable v1 s2 = equate_sort_addressable (of_var v1) s2

  and equate_sort_product s1 s2 =
    match s1 with
    | Base _ | Univar _ -> Unequal
    | Product sorts1 ->
      let sorts2 = sorts_of_product s2 in
      equate_sorts sorts1 sorts2
    | Var v1 -> equate_var_product v1 s2
    | Addressable inner1 ->
      (* As in [equate_sort_base]: [inner1 addressable] can only equal an
         addressable product, in which case absorption applies — either
         because [s2] is known to be addressable, or because [inner1] is
         (instantiation made [s1]'s wrapper redundant, so [s1 = inner1]).
         When neither is known (there are unfilled variable components) we
         conservatively answer [Unequal]: we have no way to record "all
         components are addressable", and [addressable] does not distribute
         through products. *)
      if is_known_addressable inner1 || is_known_addressable s2
      then equate_sort_product inner1 s2
      else Unequal

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

  (* Note that [decompose_into_product] of a sort made addressable is
     [None]: [addressable] does not distribute through products, so such a
     sort is not equal to any product of fresh variables. *)

  let decompose_into_addressable_product t n =
    let ts = List.init n (fun _ -> of_var (new_var ~level:level_fresh)) in
    if equate t (addressable (Product ts)) then Some ts else None

  (* [constrain_addressable t] requires [t] to be an addressable sort,
     returning [false] if it cannot be one. The image of the [addressable]
     operator is exactly the set of addressable sorts (every addressable
     sort is a fixed point), so equating [t] with a fresh variable made
     addressable expresses exactly that constraint. *)
  let constrain_addressable t =
    equate t (addressable (of_var (new_var ~level:level_fresh)))

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
      | Addressable s ->
        pp_element ~nested:true ppf s;
        Fmt.fprintf ppf " addressable"
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
  (* An operator application pending on an abstract kind constructor
     ([Types.jkind_base.Kconstr]). Since [jkind_base] is not recursive, the
     [addressable] operator applied to a [Kconstr] is recorded here and
     applied to the expansion whenever the constructor is expanded. [Id] is
     "no operator". *)
  type t =
    | Id
    | Addressable

  let equal t1 t2 =
    match t1, t2 with
    | Id, Id | Addressable, Addressable -> true
    | (Id | Addressable), _ -> false
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
        (** The [addressable] operator, applied to a layout. See the comment on
            this constructor in jkind_types.mli. *)

  module Const = struct
    type t =
      | Any of Scannable_axes.t
      | Base of Sort.base * Scannable_axes.t
      | Product of t list
      | Univar of Sort.univar
      | Genvar of Sort.var
      | Addressable of t
          (** See the comment on [Layout.t]'s [Addressable] constructor in
              jkind_types.mli. *)

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
      | Addressable c1, Addressable c2 -> equal c1 c2
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
      | Addressable c -> Option.map Sort.Const.addressable (get_sort c)

    let rec is_scannable_or_any = function
      | Any _ | Base (Scannable, _) -> true
      | Base
          ( ( Void | Untagged_immediate | Float64 | Float32 | Word | Bits8
            | Bits16 | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 ),
            _ ) ->
        false
      | Product _ -> false
      | Univar _ -> false
      | Genvar _ -> false
      (* The operator does not change whether a layout is scannable, so
         recurse; note [Addressable (Any _)] does have meaningful scannable
         axes. *)
      | Addressable c -> is_scannable_or_any c

    let rec is_known_addressable = function
      | Base (b, _) -> Sort.base_is_addressable b
      | Product ts -> List.for_all is_known_addressable ts
      | Addressable _ -> true
      | Any _ -> false
      | Univar _ | Genvar _ -> false

    (* See the comment on [Sort.addressable]. *)
    let addressable t = if is_known_addressable t then t else Addressable t

    let rec get_root_scannable_axes t =
      match t with
      | Any sa -> Some sa
      | Base (_, sa) -> if is_scannable_or_any t then Some sa else None
      | Product _ -> None
      | Univar _ -> None
      | Genvar _ -> None
      | Addressable c -> get_root_scannable_axes c

    let rec set_root_scannable_axes t sa =
      match t with
      | Any _ -> Any sa
      | Base (b, _) -> if is_scannable_or_any t then Base (b, sa) else t
      | Product _ -> t
      | Univar _ -> t
      | Genvar _ -> t
      (* Scannable axes do not affect addressability, so rewrapping
         preserves the [Addressable] construction invariant. *)
      | Addressable c -> Addressable (set_root_scannable_axes c sa)

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

  (* Whether the layout is known to be addressable. As for sorts, [false]
     means "not known to be addressable". Note that [Any _] is [false]:
     [any] has unaddressable subkinds. *)
  let rec is_known_addressable : Sort.t t -> bool = function
    | Sort (s, _) -> Sort.is_known_addressable s
    | Product ts -> List.for_all is_known_addressable ts
    | Any _ -> false
    | Addressable _ -> true

  (* The [addressable] operator on layouts. Addressability of a [Sort] node
     is pushed into the sort itself, so that the constraint travels with the
     sort during unification; hence [Addressable] never wraps a [Sort] node
     (see the invariant in jkind_types.mli). *)
  let addressable (t : Sort.t t) : Sort.t t =
    if is_known_addressable t
    then t
    else
      match t with
      | Sort (s, sa) -> Sort (Sort.addressable s, sa)
      | (Any _ | Product _) as t -> Addressable t
      | Addressable _ -> t (* unreachable: known to be addressable *)

  (* The smart destructor paired with [addressable]: filling sort variables
     inside a wrapped [Product] payload can make the payload addressable and
     hence the wrapper redundant ([l addressable = l] once [l] is
     addressable). Comparisons ([Jkind.Layout.sub], [equate_or_equal],
     [intersection]) apply this first, so their [Addressable] arms only ever
     see wrappers whose payload is not known to be addressable. *)
  let rec strip_redundant_addressable (t : Sort.t t) : Sort.t t =
    match t with
    | Addressable l when is_known_addressable l -> strip_redundant_addressable l
    | Addressable _ | Sort _ | Product _ | Any _ -> t

  let rec of_const (const : Const.t) : _ t =
    match const with
    | Any sa -> Any sa
    | Base (b, sa) -> Sort (Sort.of_base b, sa)
    | Product cs -> Product (List.map of_const cs)
    | Univar uv -> Sort (Sort.Univar uv, Scannable_axes.max)
    | Genvar v -> Sort (Sort.Var v, Scannable_axes.max)
    (* Re-canonicalize: the wrapper moves into the sort when the layout is a
       [Sort] node. *)
    | Addressable c -> addressable (of_const c)

  let product = function
    | [] -> Misc.fatal_error "Layout.product: empty product"
    | [lay] -> lay
    | lays -> Product lays

  let rec get_const of_sort : _ t -> Const.t option = function
    | Any sa -> Some (Any sa)
    | Sort (s, sa) -> of_sort s sa
    | Product layouts ->
      Option.map
        (fun x -> Const.Product x)
        (Misc.Stdlib.List.map_option (get_const of_sort) layouts)
    | Addressable l -> Option.map Const.addressable (get_const of_sort l)

  let get_flat_const t = get_const Const.of_flat_sort t

  let get_const t = get_const Const.of_sort t

  let of_new_sort_var ~level sa =
    let sort = Sort.(of_var (new_var ~level)) in
    Sort (sort, sa), sort
end
