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

module Fmt = Format_doc

module type Axis_ops = sig
  include Mode_intf.Lattice

  val to_string : t -> string

  val less_or_equal : t -> t -> Misc.Le_result.t

  val equal : t -> t -> bool
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal

  include Mode.Lattices.Total (struct
    type nonrec t = t

    let min = External

    let max = Internal

    let ord = function External -> 0 | External64 -> 1 | Internal -> 2
  end)

  let less_or_equal s1 s2 : Misc.Le_result.t =
    if equal s1 s2 then Equal else if le s1 s2 then Less else Not_le

  let to_string = function
    | External -> "external_"
    | External64 -> "external64"
    | Internal -> "internal"

  let print ppf t = Fmt.fprintf ppf "%s" (to_string t)

  let upper_bound_if_is_always_gc_ignorable () =
    (* We check that we're compiling to (64-bit) native code before counting
        External64 types as gc_ignorable, because bytecode is intended to be
        platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64 then External64 else External
end

module Nullability = struct
  type t =
    | Non_null
    | Maybe_null

  include Mode.Lattices.Total (struct
    type nonrec t = t

    let min = Non_null

    let max = Maybe_null

    let ord = function Non_null -> 0 | Maybe_null -> 1
  end)

  let less_or_equal s1 s2 : Misc.Le_result.t =
    if equal s1 s2 then Equal else if le s1 s2 then Less else Not_le

  let to_string = function Non_null -> "non_null" | Maybe_null -> "maybe_null"

  let print ppf t = Fmt.fprintf ppf "%s" (to_string t)
end

module Separability = struct
  type t =
    | Non_pointer
    | Non_pointer64
    | Non_float
    | Separable
    | Maybe_separable

  include Mode.Lattices.Total (struct
    type nonrec t = t

    let min = Non_pointer

    let max = Maybe_separable

    let ord = function
      | Non_pointer -> 0
      | Non_pointer64 -> 1
      | Non_float -> 2
      | Separable -> 3
      | Maybe_separable -> 4
  end)

  let less_or_equal s1 s2 : Misc.Le_result.t =
    if equal s1 s2 then Equal else if le s1 s2 then Less else Not_le

  let to_string = function
    | Non_pointer -> "non_pointer"
    | Non_pointer64 -> "non_pointer64"
    | Non_float -> "non_float"
    | Separable -> "separable"
    | Maybe_separable -> "maybe_separable"

  let print ppf t = Fmt.fprintf ppf "%s" (to_string t)

  let upper_bound_if_is_always_gc_ignorable () =
    (* We check that we're compiling to (64-bit) native code before counting
        Non_pointer64 types as gc_ignorable, because bytecode is intended to be
        platform independent. *)
    if !Clflags.native_code && Sys.word_size = 64
    then Non_pointer64
    else Non_pointer
end

module Addressability = struct
  (* See the .mli for an overview of actions vs readings and where each type
     is stored. *)
  module Action = struct
    type t =
      | Id
      | Addressable

    let equal a1 a2 =
      match a1, a2 with
      | Id, Id | Addressable, Addressable -> true
      | (Id | Addressable), _ -> false

    (* The operator is idempotent, so composition is commutative and
       coincides with the join of the two actions. *)
    let compose a1 a2 =
      match a1, a2 with
      | Addressable, _ | _, Addressable -> Addressable
      | Id, Id -> Id

    let to_string = function Id -> "id" | Addressable -> "addressable"

    let print ppf t = Fmt.fprintf ppf "%s" (to_string t)
  end

  type t =
    | Exact of Action.t
    | Requires_addressable
    | Join

  type slot = t

  let equal a1 a2 =
    match a1, a2 with
    | Exact a1, Exact a2 -> Action.equal a1 a2
    | Requires_addressable, Requires_addressable -> true
    | Join, Join -> true
    | (Exact _ | Requires_addressable | Join), _ -> false

  let of_action_on_undetermined : Action.t -> t = function
    | Id -> Join
    | Addressable -> Requires_addressable

  (* The root action and component slot a [Sort] node's slot flattens to
     when its (resolved) product sort becomes a [Product] node, whose root
     carries only an action. Exact slots flatten exactly: the whole-kind
     form becomes the root action with exactly-plain components (the
     whole-product mark does not distribute). A [Join] becomes an unmarked
     root with join components, which reads the same.
     [Requires_addressable] has no exact flattening (neither the
     whole-marked nor the component-marked satisfier determines the
     components), so it COMMITS to the whole-marked form at this boundary -
     sound (the bound only shrinks) but incomplete, exactly as the
     pre-[Requires_addressable] design was everywhere. CR rtjoa: give
     [Product] roots a third state to preserve the requirement across
     flattening and cmi round trips. *)
  let flatten_slot : t -> Action.t * t = function
    | Exact a -> a, Exact Id
    | Requires_addressable -> Addressable, Exact Id
    | Join -> Id, Join

  (* The slot fabricated for the components of a decomposed sort-backed
     product whose root slot is the argument (the root itself stays on the
     [Sort] node, unlike [flatten_slot]): exact roots have exactly-plain
     components; a required-addressable root does not constrain the
     components (the whole-marked satisfier has plain components, the
     component-marked one has marked components); a join leaves them
     unconstrained too. *)
  let decomposed_component : t -> t = function
    | Exact _ -> Exact Id
    | Requires_addressable | Join -> Join

  let to_string = function
    | Exact a -> Action.to_string a
    | Requires_addressable -> "requires_addressable"
    | Join -> "join"

  let print ppf t = Fmt.fprintf ppf "%s" (to_string t)

  module Mark = struct
    type t =
      | Marked
      | Unmarked
      | Requires
      | Flexible

    (* The raw embedding: the mark a slot denotes verbatim. Readers apply
       the sort's collapse before falling back to this. *)
    let of_slot : slot -> t = function
      | Exact Addressable -> Marked
      | Exact Id -> Unmarked
      | Requires_addressable -> Requires
      | Join -> Flexible

    (* Store a computed mark back as a slot, for the meets' write-back.
       Sound only because meets return marks that describe the written
       node exactly (a meet of exact marks, the requirement, or the
       join). *)
    let to_slot : t -> slot = function
      | Marked -> Exact Addressable
      | Unmarked -> Exact Id
      | Requires -> Requires_addressable
      | Flexible -> Join

    let equal m1 m2 =
      match m1, m2 with
      | Marked, Marked
      | Unmarked, Unmarked
      | Requires, Requires
      | Flexible, Flexible ->
        true
      | (Marked | Unmarked | Requires | Flexible), _ -> false

    (* The order: [Marked <= Requires <= Flexible] and
       [Unmarked <= Flexible], with [Unmarked] incomparable to the other
       two ([Marked] because the operator is a modifier, not a narrowing;
       [Requires] because a not-known-addressable kind does not satisfy the
       requirement). *)
    let less_or_equal m1 m2 : Misc.Le_result.t =
      match m1, m2 with
      | Marked, Marked
      | Unmarked, Unmarked
      | Requires, Requires
      | Flexible, Flexible ->
        Equal
      | Marked, Requires | (Marked | Unmarked | Requires), Flexible -> Less
      | Marked, Unmarked
      | Unmarked, (Marked | Requires)
      | Requires, (Marked | Unmarked)
      | Flexible, (Marked | Unmarked | Requires) ->
        Not_le

    let le m1 m2 = Misc.Le_result.is_le (less_or_equal m1 m2)

    (* The greatest lower bound where one exists ([Marked] and [Unmarked],
       and [Requires] and [Unmarked], have none). *)
    let meet m1 m2 =
      match m1, m2 with
      | Flexible, m | m, Flexible -> Some m
      | Marked, (Marked | Requires) | Requires, Marked -> Some Marked
      | Requires, Requires -> Some Requires
      | Unmarked, Unmarked -> Some Unmarked
      | (Marked | Requires), Unmarked | Unmarked, (Marked | Requires) -> None

    let combine_product ts =
      (* Combines component mark readings into an unmarked product's mark
         reading; see the .mli. Note that the implicit order here ([Marked]
         absorbed by [Flexible] absorbed by [Unmarked]) is not the subkind
         order, under which [Marked] and [Unmarked] are incomparable. A
         [Requires] component counts as [Marked]: whatever it resolves to
         is addressable, so whole-marking the product is a no-op either
         way. *)
      List.fold_left
        (fun acc t ->
          match acc, t with
          | Unmarked, _ | _, Unmarked -> Unmarked
          | Flexible, _ | _, Flexible -> Flexible
          | (Marked | Requires), (Marked | Requires) -> Marked)
        Marked ts

    (* Whether kinds with these marks are all addressable - equivalently,
       [combine_product] of them is [Marked], so a whole-product mark over
       them would be a no-op. *)
    let all_marked ts =
      List.for_all
        (fun t ->
          match t with
          | Marked | Requires -> true
          | Unmarked | Flexible -> false)
        ts

    let of_action_on_undetermined : Action.t -> t = function
      | Id -> Flexible
      | Addressable -> Requires

    (* Only [Marked] and [Requires] are ever printed in user-facing output
       (both as the word [addressable]). *)
    let to_string = function
      | Marked -> "addressable"
      | Unmarked -> "unmarked"
      | Requires -> "addressable"
      | Flexible -> "flexible"
  end

  module Verdict = struct
    type t =
      | Known_unaddressable
      | Known_addressable
      | Undetermined

    let consistent v1 v2 =
      match v1, v2 with
      | Known_unaddressable, Known_addressable
      | Known_addressable, Known_unaddressable ->
        false
      | ( (Known_unaddressable | Known_addressable | Undetermined),
          (Known_unaddressable | Known_addressable | Undetermined) ) ->
        true

    let combine_product ts =
      List.fold_left
        (fun acc t ->
          match acc, t with
          | Known_unaddressable, _ | _, Known_unaddressable ->
            Known_unaddressable
          | Undetermined, _ | _, Undetermined -> Undetermined
          | Known_addressable, Known_addressable -> Known_addressable)
        Known_addressable ts

    let of_action_on_undetermined : Action.t -> t = function
      | Id -> Undetermined
      | Addressable -> Known_addressable
  end
end

module Axis = struct
  module Nonmodal = struct
    type 'a t = Externality : Externality.t t
  end

  type 'a t =
    | Modal : 'a Mode.Crossing.Axis.t -> 'a t
    | Nonmodal : 'a Nonmodal.t -> 'a t

  type packed = Pack : 'a t -> packed [@@unboxed]

  let all =
    [ Pack (Modal (Comonadic Areality));
      Pack (Modal (Monadic Uniqueness));
      Pack (Modal (Comonadic Linearity));
      Pack (Modal (Monadic Contention));
      Pack (Modal (Comonadic Portability));
      Pack (Modal (Comonadic Forkable));
      Pack (Modal (Comonadic Yielding));
      Pack (Modal (Comonadic Statefulness));
      Pack (Modal (Monadic Visibility));
      Pack (Modal (Monadic Staticity));
      (* CR-soon zqian: call [Mode.Crossing.Axis.all] for modal axes *)
      Pack (Nonmodal Externality) ]

  let name (type a) : a t -> string = function
    | Modal ax ->
      let (P ax) =
        P ax |> Mode.Crossing.Axis.to_modality |> Mode.Modality.Axis.to_value
      in
      Fmt.asprintf "%a" Mode.Value.Axis.print ax
    | Nonmodal Externality -> "externality"
end

module Per_axis = struct
  open Axis

  module Nonmodal = struct
    open Axis.Nonmodal

    let min : type a. a t -> a = function Externality -> Externality.min

    let max : type a. a t -> a = function Externality -> Externality.max

    let le : type a. a t -> a -> a -> bool =
     fun ax a b -> match ax with Externality -> Externality.le a b

    let equal : type a. a t -> a -> a -> bool =
     fun ax a b -> match ax with Externality -> Externality.equal a b

    let meet : type a. a t -> a -> a -> a =
     fun ax a b -> match ax with Externality -> Externality.meet a b

    let join : type a. a t -> a -> a -> a =
     fun ax a b -> match ax with Externality -> Externality.join a b

    let print : type a. a t -> Fmt.formatter -> a -> unit = function
      | Externality -> Externality.print

    let compare_obj : type a b. a t -> b t -> int =
     fun a b -> match a, b with Externality, Externality -> 0

    let equal_obj : type a b. a t -> b t -> (a, b) Misc.is_eq =
     fun a b -> match a, b with Externality, Externality -> Misc.Is_eq
  end

  let min : type a. a t -> a = function[@inline available]
    | Modal ax -> (Mode.Crossing.Per_axis.min [@inlined hint]) ax
    | Nonmodal ax -> (Nonmodal.min [@inlined hint]) ax

  let max : type a. a t -> a = function[@inline available]
    | Modal ax -> (Mode.Crossing.Per_axis.max [@inlined hint]) ax
    | Nonmodal ax -> (Nonmodal.max [@inlined hint]) ax

  let le : type a. a t -> a -> a -> bool =
   fun[@inline available] ax a b ->
    match ax with
    | Modal ax -> (Mode.Crossing.Per_axis.le [@inlined hint]) ax a b
    | Nonmodal ax -> (Nonmodal.le [@inlined hint]) ax a b

  let equal : type a. a t -> a -> a -> bool =
   fun[@inline available] ax a b ->
    match ax with
    | Modal ax -> (Mode.Crossing.Per_axis.equal [@inlined hint]) ax a b
    | Nonmodal ax -> (Nonmodal.equal [@inlined hint]) ax a b

  let meet : type a. a t -> a -> a -> a =
   fun[@inline available] ax a b ->
    match ax with
    | Modal ax -> (Mode.Crossing.Per_axis.meet [@inlined hint]) ax a b
    | Nonmodal ax -> (Nonmodal.meet [@inlined hint]) ax a b

  let join : type a. a t -> a -> a -> a =
   fun[@inline available] ax a b ->
    match ax with
    | Modal ax -> (Mode.Crossing.Per_axis.join [@inlined hint]) ax a b
    | Nonmodal ax -> (Nonmodal.join [@inlined hint]) ax a b

  let print : type a. a t -> Fmt.formatter -> a -> unit = function
    | Modal ax -> Mode.Crossing.Per_axis.print ax
    | Nonmodal ax -> Nonmodal.print ax

  let compare_obj : type a b. a t -> b t -> int =
   fun a b ->
    match a, b with
    | Modal ax0, Modal ax1 -> Mode.Crossing.Per_axis.compare_obj ax0 ax1
    | Modal _, _ -> -1
    | _, Modal _ -> 1
    | Nonmodal ax0, Nonmodal ax1 -> Nonmodal.compare_obj ax0 ax1

  let equal_obj : type a b. a t -> b t -> (a, b) Misc.is_eq =
   fun a b ->
    match a, b with
    | Modal ax0, Modal ax1 -> Mode.Crossing.Per_axis.equal_obj ax0 ax1
    | Modal _, _ -> Misc.Is_not_eq
    | _, Modal _ -> Misc.Is_not_eq
    | Nonmodal ax0, Nonmodal ax1 -> Nonmodal.equal_obj ax0 ax1

  let print_obj : type a. Fmt.formatter -> a t -> unit =
   fun ppf ax -> Fmt.pp_print_string ppf (name ax)
end

module Axis_set = struct
  (* This could be [bool Axis_collection.t], but instead we represent it as a bitfield for
     performance (this matters, since these are hammered on quite a bit during with-bound
     normalization) *)

  type t = int

  let[@inline] axis_index (type a) : a Axis.t -> _ = function
    | Modal (Comonadic Areality) -> 0
    | Modal (Monadic Uniqueness) -> 1
    | Modal (Comonadic Linearity) -> 2
    | Modal (Monadic Contention) -> 3
    | Modal (Comonadic Portability) -> 4
    | Modal (Comonadic Forkable) -> 5
    | Modal (Comonadic Yielding) -> 6
    | Modal (Comonadic Statefulness) -> 7
    | Modal (Monadic Visibility) -> 8
    | Modal (Monadic Staticity) -> 9
    (* CR-soon zqian: call [Mode.Crossing.Axis.index] for modal axes *)
    | Nonmodal Externality -> 10

  let[@inline] axis_mask ax = 1 lsl axis_index ax

  let[@inline] set ~axis ~to_ t =
    match to_ with
    | true -> t lor axis_mask axis
    | false -> t land lnot (axis_mask axis)

  let empty = 0

  let[@inline] add t axis = set ~axis ~to_:true t

  let[@inline] create ~f =
    (* PERF: this is manually unrolled because flambda2 doesn't unroll for us, and this
       function is quite hot *)
    let[@inline] set_axis axis t =
      if f ~axis:(Axis.Pack axis) then t lor axis_mask axis else t
    in
    0
    |> set_axis (Modal (Comonadic Areality))
    |> set_axis (Modal (Monadic Uniqueness))
    |> set_axis (Modal (Comonadic Linearity))
    |> set_axis (Modal (Monadic Contention))
    |> set_axis (Modal (Comonadic Portability))
    |> set_axis (Modal (Comonadic Forkable))
    |> set_axis (Modal (Comonadic Yielding))
    |> set_axis (Modal (Comonadic Statefulness))
    |> set_axis (Modal (Monadic Visibility))
    |> set_axis (Modal (Monadic Staticity))
    |> set_axis (Nonmodal Externality)

  let all = create ~f:(fun ~axis:_ -> true)

  let equal = Int.equal

  let all_modal_axes =
    create ~f:(fun ~axis ->
        match axis with Pack (Modal _) -> true | Pack (Nonmodal _) -> false)

  let[@inline] singleton axis = add empty axis

  let[@inline] remove t axis = set ~axis ~to_:false t

  let[@inline] mem t axis = not (Int.equal (t land axis_mask axis) 0)

  let[@inline] union t1 t2 = t1 lor t2

  let[@inline] intersection t1 t2 = t1 land t2

  let[@inline] diff t1 t2 = t1 land lnot t2

  let[@inline] is_subset t1 t2 = Int.equal (t1 land t2) t1

  let[@inline] is_empty t = Int.equal t 0

  let[@inline] complement t = diff all t

  let all_nonmodal_axes = complement all_modal_axes

  let[@inline] to_seq t =
    Axis.all |> List.to_seq |> Seq.filter (fun (Axis.Pack axis) -> mem t axis)

  let[@inline] to_list t = List.of_seq (to_seq t)

  let print ppf t =
    Format.fprintf ppf "@[{%t}@]" (fun ppf ->
        Format.pp_print_seq
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
          (fun ppf (Axis.Pack axis) -> Format.fprintf ppf "%s" (Axis.name axis))
          ppf (to_seq t))
end
