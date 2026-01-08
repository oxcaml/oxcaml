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

module type Axis_ops = sig
  include Mode_intf.Lattice

  val less_or_equal : t -> t -> Misc.Le_result.t

  val equal : t -> t -> bool
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal

  let max = Internal

  let min = External

  let equal e1 e2 =
    match e1, e2 with
    | External, External -> true
    | External64, External64 -> true
    | Internal, Internal -> true
    | (External | External64 | Internal), _ -> false

  let less_or_equal t1 t2 : Misc.Le_result.t =
    match t1, t2 with
    | External, External -> Equal
    | External, (External64 | Internal) -> Less
    | External64, External -> Not_le
    | External64, External64 -> Equal
    | External64, Internal -> Less
    | Internal, (External | External64) -> Not_le
    | Internal, Internal -> Equal

  let le t1 t2 = Misc.Le_result.is_le (less_or_equal t1 t2)

  let meet t1 t2 =
    match t1, t2 with
    | External, (External | External64 | Internal)
    | (External64 | Internal), External ->
      External
    | External64, (External64 | Internal) | Internal, External64 -> External64
    | Internal, Internal -> Internal

  let join t1 t2 =
    match t1, t2 with
    | Internal, (Internal | External64 | External)
    | (External64 | External), Internal ->
      Internal
    | External64, (External64 | External) | External, External64 -> External64
    | External, External -> External

  let print ppf = function
    | External -> Format.fprintf ppf "external_"
    | External64 -> Format.fprintf ppf "external64"
    | Internal -> Format.fprintf ppf "internal"

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

  let max = Maybe_null

  let min = Non_null

  let equal n1 n2 =
    match n1, n2 with
    | Non_null, Non_null -> true
    | Maybe_null, Maybe_null -> true
    | (Non_null | Maybe_null), _ -> false

  let less_or_equal n1 n2 : Misc.Le_result.t =
    match n1, n2 with
    | Non_null, Non_null -> Equal
    | Non_null, Maybe_null -> Less
    | Maybe_null, Non_null -> Not_le
    | Maybe_null, Maybe_null -> Equal

  let le n1 n2 = Misc.Le_result.is_le (less_or_equal n1 n2)

  let meet n1 n2 =
    match n1, n2 with
    | Non_null, (Non_null | Maybe_null) | Maybe_null, Non_null -> Non_null
    | Maybe_null, Maybe_null -> Maybe_null

  let join n1 n2 =
    match n1, n2 with
    | Maybe_null, (Maybe_null | Non_null) | Non_null, Maybe_null -> Maybe_null
    | Non_null, Non_null -> Non_null

  let print ppf = function
    | Non_null -> Format.fprintf ppf "non_null"
    | Maybe_null -> Format.fprintf ppf "maybe_null"
end

module Separability = struct
  type t =
    | Non_float
    | Separable
    | Maybe_separable

  let max = Maybe_separable

  let min = Non_float

  let equal s1 s2 =
    match s1, s2 with
    | Non_float, Non_float -> true
    | Separable, Separable -> true
    | Maybe_separable, Maybe_separable -> true
    | (Non_float | Separable | Maybe_separable), _ -> false

  let less_or_equal s1 s2 : Misc.Le_result.t =
    match s1, s2 with
    | Non_float, Non_float -> Equal
    | Non_float, (Separable | Maybe_separable) -> Less
    | Separable, Non_float -> Not_le
    | Separable, Separable -> Equal
    | Separable, Maybe_separable -> Less
    | Maybe_separable, (Non_float | Separable) -> Not_le
    | Maybe_separable, Maybe_separable -> Equal

  let le s1 s2 = Misc.Le_result.is_le (less_or_equal s1 s2)

  let meet s1 s2 =
    match s1, s2 with
    | Non_float, (Non_float | Separable | Maybe_separable)
    | (Separable | Maybe_separable), Non_float ->
      Non_float
    | Separable, (Separable | Maybe_separable) | Maybe_separable, Separable ->
      Separable
    | Maybe_separable, Maybe_separable -> Maybe_separable

  let join s1 s2 =
    match s1, s2 with
    | Maybe_separable, (Maybe_separable | Separable | Non_float)
    | (Separable | Non_float), Maybe_separable ->
      Maybe_separable
    | Separable, (Separable | Non_float) | Non_float, Separable -> Separable
    | Non_float, Non_float -> Non_float

  let print ppf = function
    | Non_float -> Format.fprintf ppf "non_float"
    | Separable -> Format.fprintf ppf "separable"
    | Maybe_separable -> Format.fprintf ppf "maybe_separable"
end

module Axis = struct
  module Nonmodal = struct
    type 'a t =
      | Externality : Externality.t t
      | Nullability : Nullability.t t
      | Separability : Separability.t t
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
      Pack (Nonmodal Externality);
      Pack (Nonmodal Nullability);
      Pack (Nonmodal Separability) ]

  let name (type a) : a t -> string = function
    | Modal ax ->
      let (P ax) =
        P ax |> Mode.Crossing.Axis.to_modality |> Mode.Modality.Axis.to_value
      in
      Format.asprintf "%a" Mode.Value.Axis.print ax
    | Nonmodal Externality -> "externality"
    | Nonmodal Nullability -> "nullability"
    | Nonmodal Separability -> "separability"
end

module Per_axis = struct
  open Axis

  module Nonmodal = struct
    open Axis.Nonmodal

    let min : type a. a t -> a = function
      | Externality -> Externality.min
      | Nullability -> Nullability.min
      | Separability -> Separability.min

    let max : type a. a t -> a = function
      | Externality -> Externality.max
      | Nullability -> Nullability.max
      | Separability -> Separability.max

    let le : type a. a t -> a -> a -> bool =
     fun ax a b ->
      match ax with
      | Externality -> Externality.le a b
      | Nullability -> Nullability.le a b
      | Separability -> Separability.le a b

    let meet : type a. a t -> a -> a -> a =
     fun ax a b ->
      match ax with
      | Externality -> Externality.meet a b
      | Nullability -> Nullability.meet a b
      | Separability -> Separability.meet a b

    let join : type a. a t -> a -> a -> a =
     fun ax a b ->
      match ax with
      | Externality -> Externality.join a b
      | Nullability -> Nullability.join a b
      | Separability -> Separability.join a b

    let print : type a. a t -> Format.formatter -> a -> unit = function
      | Externality -> Externality.print
      | Nullability -> Nullability.print
      | Separability -> Separability.print

    let eq_obj : type a b. a t -> b t -> (a, b) Misc.eq option =
     fun a b ->
      match a, b with
      | Externality, Externality -> Some Refl
      | Nullability, Nullability -> Some Refl
      | Separability, Separability -> Some Refl
      | _ -> None
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

  let print : type a. a t -> Format.formatter -> a -> unit = function
    | Modal ax -> Mode.Crossing.Per_axis.print ax
    | Nonmodal ax -> Nonmodal.print ax

  let eq_obj : type a b. a t -> b t -> (a, b) Misc.eq option =
   fun a b ->
    match a, b with
    | Modal ax0, Modal ax1 -> Mode.Crossing.Per_axis.eq_obj ax0 ax1
    | Nonmodal ax0, Nonmodal ax1 -> Nonmodal.eq_obj ax0 ax1
    | _ -> None

  let print_obj : type a. Format.formatter -> a t -> unit =
   fun ppf ax -> Format.pp_print_string ppf (name ax)
end

module Axis_set = struct
  (* This could be [bool Axis_collection.t], but instead we represent it as a bitfield for
     performance (this matters, since these are hammered on quite a bit during with-bound
     normalization) *)

  type t = int

  let[@inline] axis_index (type a) : a Axis.t -> _ = function
    | Modal (Comonadic Areality) -> 0
    | Modal (Comonadic Linearity) -> 1
    | Modal (Monadic Uniqueness) -> 2
    | Modal (Comonadic Portability) -> 3
    | Modal (Monadic Contention) -> 4
    | Modal (Comonadic Forkable) -> 5
    | Modal (Comonadic Yielding) -> 6
    | Modal (Comonadic Statefulness) -> 7
    | Modal (Monadic Visibility) -> 8
    | Modal (Monadic Staticity) -> 9
    (* CR-soon zqian: call [Mode.Crossing.Axis.index] for modal axes *)
    | Nonmodal Externality -> 10
    | Nonmodal Nullability -> 11
    | Nonmodal Separability -> 12

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
    |> set_axis (Modal (Comonadic Linearity))
    |> set_axis (Modal (Monadic Uniqueness))
    |> set_axis (Modal (Comonadic Portability))
    |> set_axis (Modal (Monadic Contention))
    |> set_axis (Modal (Comonadic Forkable))
    |> set_axis (Modal (Comonadic Yielding))
    |> set_axis (Modal (Comonadic Statefulness))
    |> set_axis (Modal (Monadic Visibility))
    |> set_axis (Modal (Monadic Staticity))
    |> set_axis (Nonmodal Externality)
    |> set_axis (Nonmodal Nullability)
    |> set_axis (Nonmodal Separability)

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
