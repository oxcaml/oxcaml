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
  include Mode.Externality.Const

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

module Axis = struct
  include Mode.Crossing.Axis

  type packed = Pack : 'a t -> packed [@@unboxed]

  let[@inline] index (type a) : a t -> _ = function
    | Comonadic Areality -> 0
    | Monadic Uniqueness -> 1
    | Comonadic Linearity -> 2
    | Monadic Contention -> 3
    | Comonadic Portability -> 4
    | Comonadic Forkable -> 5
    | Comonadic Yielding -> 6
    | Comonadic Statefulness -> 7
    | Monadic Visibility -> 8
    | Monadic Staticity -> 9
    | Comonadic Externality -> 10

  let all =
    [ Pack (Comonadic Areality);
      Pack (Monadic Uniqueness);
      Pack (Comonadic Linearity);
      Pack (Monadic Contention);
      Pack (Comonadic Portability);
      Pack (Comonadic Forkable);
      Pack (Comonadic Yielding);
      Pack (Comonadic Statefulness);
      Pack (Monadic Visibility);
      Pack (Monadic Staticity);
      Pack (Comonadic Externality) ]

  let equal (Pack a) (Pack b) = Mode.Crossing.Per_axis.compare_obj a b = 0

  let name ax =
    let (Mode.Value.Axis.P ax) =
      to_modality (P ax) |> Mode.Modality.Axis.to_value
    in
    Fmt.asprintf "%a" Mode.Value.Axis.print ax
end

module Per_axis = Mode.Crossing.Per_axis

module Axis_set = struct
  (* This could be [bool Axis_collection.t], but instead we represent it as a bitfield for
     performance (this matters, since these are hammered on quite a bit during with-bound
     normalization) *)

  type t = int

  let[@inline] axis_mask ax = 1 lsl Axis.index ax

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
    |> set_axis (Comonadic Areality)
    |> set_axis (Monadic Uniqueness)
    |> set_axis (Comonadic Linearity)
    |> set_axis (Monadic Contention)
    |> set_axis (Comonadic Portability)
    |> set_axis (Comonadic Forkable)
    |> set_axis (Comonadic Yielding)
    |> set_axis (Comonadic Statefulness)
    |> set_axis (Monadic Visibility)
    |> set_axis (Monadic Staticity)
    |> set_axis (Comonadic Externality)

  let all = create ~f:(fun ~axis:_ -> true)

  let equal = Int.equal

  let[@inline] singleton axis = add empty axis

  let[@inline] remove t axis = set ~axis ~to_:false t

  let[@inline] mem t axis = not (Int.equal (t land axis_mask axis) 0)

  let[@inline] union t1 t2 = t1 lor t2

  let[@inline] intersection t1 t2 = t1 land t2

  let[@inline] diff t1 t2 = t1 land lnot t2

  let[@inline] is_subset t1 t2 = Int.equal (t1 land t2) t1

  let[@inline] is_empty t = Int.equal t 0

  let[@inline] complement t = diff all t

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
