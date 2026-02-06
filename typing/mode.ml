(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Zesen Qian, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance
open Solver_intf
open Solver
open Mode_intf
module Hint = Mode_hint
module Fmt = Format_doc

module Hint_for_solver (* : Solver_intf.Hint *) = struct
  module Pinpoint = struct
    type t = Hint.pinpoint

    let unknown : t = Location.none, Unknown
  end

  module Morph = struct
    type 'd t = 'd Hint.morph

    let unknown : _ t = Unknown

    let id : _ t = Skip

    let left_adjoint : type l.
        Hint.pinpoint ->
        (l * allowed) t ->
        Hint.pinpoint * (allowed * disallowed) t =
     fun pp t ->
      match t with
      | Skip -> pp, Skip
      | Is_closed_by (Monadic, co) -> co.closure, Close_over (Monadic, co)
      | Is_closed_by (Comonadic, co) -> co.closure, Close_over (Comonadic, co)
      | Captured_by_partial_application ->
        (Location.none, Expression), Adj_captured_by_partial_application
      | Crossing -> pp, Crossing
      | Unknown -> (Location.none, Unknown), Unknown
      | Allocation_r loc -> pp, Allocation_l loc
      | Contains_r (Comonadic, { containing; contained }) ->
        contained, Is_contained_by (Comonadic, { containing; container = pp })
      | Contains_l (Monadic, { containing; contained }) ->
        contained, Is_contained_by (Monadic, { containing; container = pp })
      | Is_contained_by (Comonadic, { containing; container }) ->
        container, Contains_l (Comonadic, { containing; contained = pp })
      | Is_contained_by (Monadic, { containing; container }) ->
        container, Contains_r (Monadic, { containing; contained = pp })

    let right_adjoint : type r.
        Hint.pinpoint ->
        (allowed * r) t ->
        Hint.pinpoint * (disallowed * allowed) t =
     fun pp t ->
      match t with
      | Skip -> pp, Skip
      | Close_over (Monadic, co) -> co.closed, Is_closed_by (Monadic, co)
      | Close_over (Comonadic, co) -> co.closed, Is_closed_by (Comonadic, co)
      | Adj_captured_by_partial_application ->
        (Location.none, Expression), Captured_by_partial_application
      | Crossing -> pp, Crossing
      | Unknown -> (Location.none, Unknown), Unknown
      | Allocation_l loc -> pp, Allocation_r loc
      | Contains_l (Comonadic, { containing; contained }) ->
        contained, Is_contained_by (Comonadic, { containing; container = pp })
      | Contains_r (Monadic, { containing; contained }) ->
        contained, Is_contained_by (Monadic, { containing; container = pp })
      | Is_contained_by (Comonadic, { containing; container }) ->
        container, Contains_r (Comonadic, { containing; contained = pp })
      | Is_contained_by (Monadic, { containing; container }) ->
        container, Contains_l (Monadic, { containing; contained = pp })

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

      let allow_left : type l r. (allowed * r) t -> (l * r) t =
       fun (type l r) (h : (allowed * r) t) : (l * r) t ->
        match h with
        | Skip -> Skip
        | Unknown -> Unknown
        | Close_over (Monadic, x) -> Close_over (Monadic, x)
        | Close_over (Comonadic, x) -> Close_over (Comonadic, x)
        | Adj_captured_by_partial_application ->
          Adj_captured_by_partial_application
        | Crossing -> Crossing
        | Allocation_l loc -> Allocation_l loc
        | Contains_l (Comonadic, x) -> Contains_l (Comonadic, x)
        | Contains_r (Monadic, x) -> Contains_r (Monadic, x)
        | Is_contained_by (Comonadic, x) -> Is_contained_by (Comonadic, x)
        | Is_contained_by (Monadic, x) -> Is_contained_by (Monadic, x)

      let allow_right : type l r. (l * allowed) t -> (l * r) t =
       fun (type l r) (h : (l * allowed) t) : (l * r) t ->
        match h with
        | Skip -> Skip
        | Unknown -> Unknown
        | Is_closed_by (Monadic, x) -> Is_closed_by (Monadic, x)
        | Is_closed_by (Comonadic, x) -> Is_closed_by (Comonadic, x)
        | Captured_by_partial_application -> Captured_by_partial_application
        | Crossing -> Crossing
        | Allocation_r loc -> Allocation_r loc
        | Contains_r (Comonadic, x) -> Contains_r (Comonadic, x)
        | Contains_l (Monadic, x) -> Contains_l (Monadic, x)
        | Is_contained_by (Comonadic, x) -> Is_contained_by (Comonadic, x)
        | Is_contained_by (Monadic, x) -> Is_contained_by (Monadic, x)

      let disallow_left : type l r. (l * r) t -> (disallowed * r) t =
       fun (type l r) (h : (l * r) t) : (disallowed * r) t ->
        match h with
        | Skip -> Skip
        | Unknown -> Unknown
        | Close_over (Monadic, x) -> Close_over (Monadic, x)
        | Close_over (Comonadic, x) -> Close_over (Comonadic, x)
        | Is_closed_by (Monadic, x) -> Is_closed_by (Monadic, x)
        | Is_closed_by (Comonadic, x) -> Is_closed_by (Comonadic, x)
        | Captured_by_partial_application -> Captured_by_partial_application
        | Adj_captured_by_partial_application ->
          Adj_captured_by_partial_application
        | Crossing -> Crossing
        | Allocation_r loc -> Allocation_r loc
        | Allocation_l loc -> Allocation_l loc
        | Contains_r (Comonadic, x) -> Contains_r (Comonadic, x)
        | Contains_l (Monadic, x) -> Contains_l (Monadic, x)
        | Is_contained_by (Comonadic, x) -> Is_contained_by (Comonadic, x)
        | Is_contained_by (Monadic, x) -> Is_contained_by (Monadic, x)
        | Contains_r (Monadic, x) -> Contains_r (Monadic, x)
        | Contains_l (Comonadic, x) -> Contains_l (Comonadic, x)

      let disallow_right : type l r. (l * r) t -> (l * disallowed) t =
       fun (type l r) (h : (l * r) t) : (l * disallowed) t ->
        match h with
        | Skip -> Skip
        | Unknown -> Unknown
        | Close_over (Monadic, x) -> Close_over (Monadic, x)
        | Close_over (Comonadic, x) -> Close_over (Comonadic, x)
        | Is_closed_by (Monadic, x) -> Is_closed_by (Monadic, x)
        | Is_closed_by (Comonadic, x) -> Is_closed_by (Comonadic, x)
        | Captured_by_partial_application -> Captured_by_partial_application
        | Adj_captured_by_partial_application ->
          Adj_captured_by_partial_application
        | Crossing -> Crossing
        | Allocation_l loc -> Allocation_l loc
        | Allocation_r loc -> Allocation_r loc
        | Contains_l (Comonadic, x) -> Contains_l (Comonadic, x)
        | Contains_r (Monadic, x) -> Contains_r (Monadic, x)
        | Is_contained_by (Comonadic, x) -> Is_contained_by (Comonadic, x)
        | Is_contained_by (Monadic, x) -> Is_contained_by (Monadic, x)
        | Contains_l (Monadic, x) -> Contains_l (Monadic, x)
        | Contains_r (Comonadic, x) -> Contains_r (Comonadic, x)
    end)
  end

  module Const = struct
    type 'd t = 'd Hint.const

    let unknown : _ t = Unknown

    let max : _ t = Unknown

    let min : _ t = Unknown

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t constraint 'd = 'l * 'r

      let allow_left : type l r. (allowed * r) t -> (l * r) t =
       fun (type l r) (h : (allowed * r) t) : (l * r) t ->
        match h with
        | Unknown -> Unknown
        | Legacy x -> Legacy x
        | Stack_expression -> Stack_expression
        | Mutable_read m -> Mutable_read m
        | Mutable_write m -> Mutable_write m
        | Lazy_forced -> Lazy_forced
        | Borrowed (loc, Comonadic) -> Borrowed (loc, Comonadic)
        | Borrowed (loc, Monadic) -> Borrowed (loc, Monadic)
        | Contained_by c -> Contained_by c

      let allow_right : type l r. (l * allowed) t -> (l * r) t =
       fun (type l r) (h : (l * allowed) t) : (l * r) t ->
        match h with
        | Unknown -> Unknown
        | Legacy x -> Legacy x
        | Lazy_allocated_on_heap -> Lazy_allocated_on_heap
        | Tailcall_function -> Tailcall_function
        | Tailcall_argument -> Tailcall_argument
        | Function_return -> Function_return
        | Module_allocated_on_heap -> Module_allocated_on_heap
        | Is_used_in pp -> Is_used_in pp
        | Always_dynamic x -> Always_dynamic x
        | Branching -> Branching
        | Borrowed (loc, Monadic) -> Borrowed (loc, Monadic)
        | Borrowed (loc, Comonadic) -> Borrowed (loc, Comonadic)
        | Escape_region x -> Escape_region x
        | Contained_by c -> Contained_by c

      let disallow_left : type l r. (l * r) t -> (disallowed * r) t =
       fun (type l r) (h : (l * r) t) : (disallowed * r) t ->
        match h with
        | Unknown -> Unknown
        | Lazy_allocated_on_heap -> Lazy_allocated_on_heap
        | Legacy x -> Legacy x
        | Tailcall_function -> Tailcall_function
        | Tailcall_argument -> Tailcall_argument
        | Mutable_read m -> Mutable_read m
        | Mutable_write m -> Mutable_write m
        | Lazy_forced -> Lazy_forced
        | Function_return -> Function_return
        | Stack_expression -> Stack_expression
        | Module_allocated_on_heap -> Module_allocated_on_heap
        | Is_used_in pp -> Is_used_in pp
        | Always_dynamic x -> Always_dynamic x
        | Branching -> Branching
        | Borrowed (loc, Monadic) -> Borrowed (loc, Monadic)
        | Borrowed (loc, Comonadic) -> Borrowed (loc, Comonadic)
        | Escape_region x -> Escape_region x
        | Contained_by c -> Contained_by c

      let disallow_right : type l r. (l * r) t -> (l * disallowed) t =
       fun (type l r) (h : (l * r) t) : (l * disallowed) t ->
        match h with
        | Unknown -> Unknown
        | Lazy_allocated_on_heap -> Lazy_allocated_on_heap
        | Legacy x -> Legacy x
        | Tailcall_function -> Tailcall_function
        | Tailcall_argument -> Tailcall_argument
        | Mutable_read m -> Mutable_read m
        | Mutable_write m -> Mutable_write m
        | Lazy_forced -> Lazy_forced
        | Function_return -> Function_return
        | Stack_expression -> Stack_expression
        | Module_allocated_on_heap -> Module_allocated_on_heap
        | Is_used_in pp -> Is_used_in pp
        | Always_dynamic x -> Always_dynamic x
        | Branching -> Branching
        | Borrowed (loc, Monadic) -> Borrowed (loc, Monadic)
        | Borrowed (loc, Comonadic) -> Borrowed (loc, Comonadic)
        | Escape_region x -> Escape_region x
        | Contained_by c -> Contained_by c
    end)
  end
end

type nonrec allowed = allowed

type nonrec disallowed = disallowed

type nonrec equate_step = equate_step

module type Heyting = sig
  (** Extend the [Lattice] interface with operations of Heyting algebras *)

  include Lattice

  (** [imply c] is the right adjoint of [meet c]; That is, for any [a] and [b],
      [meet c a <= b] iff [a <= imply c b] *)
  val imply : t -> t -> t
end

module type CoHeyting = sig
  (** Extend the [Lattice] interface with operations of co-Heyting algebras *)

  include Lattice

  (** [subtract _ c] is the left adjoint of [join c]. That is, for any [a] and
      [b], [subtract a c <= b] iff [a <= join c b] *)
  val subtract : t -> t -> t
end

(* Even though our lattices are all bi-heyting algebras, that knowledge is
   internal to this module. Externally they are seen as normal lattices. *)
module Lattices = struct
  module type Total = sig
    (** A lattice is total order, if for any [a] [b], [a <= b] or [b <= a].

        If it's also finite, then the ordering can be represented as a monotone
        injection [ord] into [int], where [a <= b] iff [ord a <= ord b]. *)

    type t

    val ord : t -> int

    val min : t

    val max : t
  end

  module Total (L : Total) = struct
    let min = L.min

    let max = L.max

    let le a b = L.ord a <= L.ord b

    let equal a b = L.ord a = L.ord b

    let join a b = if L.ord a > L.ord b then a else b

    let meet a b = if L.ord a < L.ord b then a else b

    (* A total lattice has a co-heyting structure.
       Prove the [subtract] below is the left adjoint of [join].
        - If [subtract a c <= b], by the definition of [subtract] below,
          that could mean one of two things:
          - Took the branch [a <= c], and [min <= b]. In this case, we have [a <= c <= join c b].
          - Took the other branch, and [a <= b]. In this case, we have [a <= b <= join c b].

        - In the other direction: Given [a <= join c b], compare [c] and [b]:
          - if [c <= b], then [a <= join c b = b], and:
            - either [a <= c], then [subtract a c = min <= b]
            - or the other branch, then [subtract a c = a <= b]
          - if [b <= c], then [a <= join c b = c], then [subtract a c = min <= b]
    *)
    let subtract a c = if le a c then L.min else a

    (* A total lattice has a heyting structure. The proof for [imply] is dual
       and omitted. *)
    let imply c b = if le c b then L.max else b
  end
  [@@inline]

  type locality =
    | Global
    | Local

  type regionality =
    | Global
    | Regional
    | Local

  type 'a areality =
    | Locality : locality areality
    | Regionality : regionality areality

  let equal_areality
      : type a b. a areality -> b areality -> (a, b) equality =
    fun a b ->
      match a, b with
      | Locality, Locality -> Equal
      | Regionality, Regionality -> Equal
      | (Locality | Regionality), _ -> Not_equal

  let compare_areality
      : type a b. a areality -> b areality -> (a, b) comparison =
    fun a b ->
      match a, b with
      | Locality, Locality -> Equal
      | Locality, _ -> Less_than
      | _, Locality -> Greater_than
      | Regionality, Regionality -> Equal

  module type Areality = sig
    include Const

    include Heyting with type t := t

    val areality : t areality
  end

  module Locality = struct
    type t = locality =
      | Global
      | Local

    include Total (struct
      type nonrec t = t

      let min = Global

      let max = Local

      let ord = function Global -> 0 | Local -> 1
    end)

    let legacy = Global

    let print ppf = function
      | Global -> Fmt.fprintf ppf "global"
      | Local -> Fmt.fprintf ppf "local"

    let areality = Locality
  end

  module Regionality = struct
    type t = regionality =
      | Global
      | Regional
      | Local

    include Total (struct
      type nonrec t = t

      let min = Global

      let max = Local

      let ord = function Global -> 0 | Regional -> 1 | Local -> 2
    end)

    let legacy = Global

    let print ppf = function
      | Global -> Fmt.fprintf ppf "global"
      | Regional -> Fmt.fprintf ppf "regional"
      | Local -> Fmt.fprintf ppf "local"

    let areality = Regionality
  end

  module Uniqueness = struct
    type t =
      | Unique
      | Aliased

    include Total (struct
      type nonrec t = t

      let min = Unique

      let max = Aliased

      let ord = function Unique -> 0 | Aliased -> 1
    end)

    let legacy = Aliased

    let print ppf = function
      | Aliased -> Fmt.fprintf ppf "aliased"
      | Unique -> Fmt.fprintf ppf "unique"
  end

  module Linearity = struct
    type t =
      | Many
      | Once

    include Total (struct
      type nonrec t = t

      let min = Many

      let max = Once

      let ord = function Many -> 0 | Once -> 1
    end)

    let legacy = Many

    let print ppf = function
      | Once -> Fmt.fprintf ppf "once"
      | Many -> Fmt.fprintf ppf "many"
  end

  module Portability = struct
    type t =
      | Portable
      | Shareable
      | Nonportable

    include Total (struct
      type nonrec t = t

      let min = Portable

      let max = Nonportable

      let ord = function Portable -> 0 | Shareable -> 1 | Nonportable -> 2
    end)

    let legacy = Nonportable

    let print ppf = function
      | Portable -> Fmt.fprintf ppf "portable"
      | Shareable -> Fmt.fprintf ppf "shareable"
      | Nonportable -> Fmt.fprintf ppf "nonportable"
  end

  module Contention = struct
    type t =
      | Uncontended
      | Shared
      | Contended

    include Total (struct
      type nonrec t = t

      let min = Uncontended

      let max = Contended

      let ord = function Uncontended -> 0 | Shared -> 1 | Contended -> 2
    end)

    let legacy = Uncontended

    let print ppf = function
      | Contended -> Fmt.fprintf ppf "contended"
      | Shared -> Fmt.fprintf ppf "shared"
      | Uncontended -> Fmt.fprintf ppf "uncontended"
  end

  module Forkable = struct
    type t =
      | Forkable
      | Unforkable

    include Total (struct
      type nonrec t = t

      let min = Forkable

      let max = Unforkable

      let ord = function Forkable -> 0 | Unforkable -> 1
    end)

    let legacy = Forkable

    let print ppf = function
      | Unforkable -> Fmt.fprintf ppf "unforkable"
      | Forkable -> Fmt.fprintf ppf "forkable"
  end

  module Yielding = struct
    type t =
      | Unyielding
      | Yielding

    include Total (struct
      type nonrec t = t

      let min = Unyielding

      let max = Yielding

      let ord = function Unyielding -> 0 | Yielding -> 1
    end)

    let legacy = Unyielding

    let print ppf = function
      | Yielding -> Fmt.fprintf ppf "yielding"
      | Unyielding -> Fmt.fprintf ppf "unyielding"
  end

  module Statefulness = struct
    type t =
      | Stateless
      | Observing
      | Stateful

    include Total (struct
      type nonrec t = t

      let min = Stateless

      let max = Stateful

      let ord = function Stateless -> 0 | Observing -> 1 | Stateful -> 2
    end)

    let legacy = Stateful

    let print ppf = function
      | Stateless -> Fmt.fprintf ppf "stateless"
      | Observing -> Fmt.fprintf ppf "observing"
      | Stateful -> Fmt.fprintf ppf "stateful"
  end

  module Visibility = struct
    type t =
      | Read_write
      | Read
      | Immutable

    include Total (struct
      type nonrec t = t

      let min = Read_write

      let max = Immutable

      let ord = function Read_write -> 0 | Read -> 1 | Immutable -> 2
    end)

    let legacy = Read_write

    let print ppf = function
      | Immutable -> Fmt.fprintf ppf "immutable"
      | Read -> Fmt.fprintf ppf "read"
      | Read_write -> Fmt.fprintf ppf "read_write"
  end

  module Staticity = struct
    type t =
      | Static
      | Dynamic

    include Total (struct
      type nonrec t = t

      let min = Static

      let max = Dynamic

      let ord = function Static -> 0 | Dynamic -> 1
    end)

    let legacy = Dynamic

    let print ppf = function
      | Dynamic -> Fmt.fprintf ppf "dynamic"
      | Static -> Fmt.fprintf ppf "static"
  end

  type monadic =
    { uniqueness : Uniqueness.t;
      contention : Contention.t;
      visibility : Visibility.t;
      staticity : Staticity.t
    }

  module Monadic = struct
    type t = monadic

    let min =
      let uniqueness = Uniqueness.min in
      let contention = Contention.min in
      let visibility = Visibility.min in
      let staticity = Staticity.min in
      { uniqueness; contention; visibility; staticity }

    let max =
      let uniqueness = Uniqueness.max in
      let contention = Contention.max in
      let visibility = Visibility.max in
      let staticity = Staticity.max in
      { uniqueness; contention; visibility; staticity }

    let legacy =
      let uniqueness = Uniqueness.legacy in
      let contention = Contention.legacy in
      let visibility = Visibility.legacy in
      let staticity = Staticity.legacy in
      { uniqueness; contention; visibility; staticity }

    let le m1 m2 =
      let { uniqueness = uniqueness1;
            contention = contention1;
            visibility = visibility1;
            staticity = staticity1
          } =
        m1
      in
      let { uniqueness = uniqueness2;
            contention = contention2;
            visibility = visibility2;
            staticity = staticity2
          } =
        m2
      in
      Uniqueness.le uniqueness1 uniqueness2
      && Contention.le contention1 contention2
      && Visibility.le visibility1 visibility2
      && Staticity.le staticity1 staticity2

    let equal m1 m2 =
      let { uniqueness = uniqueness1;
            contention = contention1;
            visibility = visibility1;
            staticity = staticity1
          } =
        m1
      in
      let { uniqueness = uniqueness2;
            contention = contention2;
            visibility = visibility2;
            staticity = staticity2
          } =
        m2
      in
      Uniqueness.equal uniqueness1 uniqueness2
      && Contention.equal contention1 contention2
      && Visibility.equal visibility1 visibility2
      && Staticity.equal staticity1 staticity2

    let join m1 m2 =
      let uniqueness = Uniqueness.join m1.uniqueness m2.uniqueness in
      let contention = Contention.join m1.contention m2.contention in
      let visibility = Visibility.join m1.visibility m2.visibility in
      let staticity = Staticity.join m1.staticity m2.staticity in
      { uniqueness; contention; visibility; staticity }

    let meet m1 m2 =
      let uniqueness = Uniqueness.meet m1.uniqueness m2.uniqueness in
      let contention = Contention.meet m1.contention m2.contention in
      let visibility = Visibility.meet m1.visibility m2.visibility in
      let staticity = Staticity.meet m1.staticity m2.staticity in
      { uniqueness; contention; visibility; staticity }

    let subtract m1 m2 =
      let uniqueness = Uniqueness.subtract m1.uniqueness m2.uniqueness in
      let contention = Contention.subtract m1.contention m2.contention in
      let visibility = Visibility.subtract m1.visibility m2.visibility in
      let staticity = Staticity.subtract m1.staticity m2.staticity in
      { uniqueness; contention; visibility; staticity }

    let print ppf m =
      Fmt.fprintf ppf "%a,%a,%a,%a" Uniqueness.print m.uniqueness
        Contention.print m.contention Visibility.print m.visibility
        Staticity.print m.staticity
  end

  type 'areality comonadic_with =
    { areality : 'areality;
      linearity : Linearity.t;
      portability : Portability.t;
      forkable : Forkable.t;
      yielding : Yielding.t;
      statefulness : Statefulness.t
    }

  module Comonadic_with (Areality : Areality) = struct
    type t = Areality.t comonadic_with

    let min =
      let areality = Areality.min in
      let linearity = Linearity.min in
      let portability = Portability.min in
      let forkable = Forkable.min in
      let yielding = Yielding.min in
      let statefulness = Statefulness.min in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let max =
      let areality = Areality.max in
      let linearity = Linearity.max in
      let portability = Portability.max in
      let forkable = Forkable.max in
      let yielding = Yielding.max in
      let statefulness = Statefulness.max in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let legacy =
      let areality = Areality.legacy in
      let linearity = Linearity.legacy in
      let portability = Portability.legacy in
      let forkable = Forkable.legacy in
      let yielding = Yielding.legacy in
      let statefulness = Statefulness.legacy in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let le m1 m2 =
      let { areality = areality1;
            linearity = linearity1;
            portability = portability1;
            forkable = forkable1;
            yielding = yielding1;
            statefulness = statefulness1
          } =
        m1
      in
      let { areality = areality2;
            linearity = linearity2;
            portability = portability2;
            forkable = forkable2;
            yielding = yielding2;
            statefulness = statefulness2
          } =
        m2
      in
      Areality.le areality1 areality2
      && Linearity.le linearity1 linearity2
      && Portability.le portability1 portability2
      && Forkable.le forkable1 forkable2
      && Yielding.le yielding1 yielding2
      && Statefulness.le statefulness1 statefulness2

    let equal m1 m2 =
      let { areality = areality1;
            linearity = linearity1;
            portability = portability1;
            forkable = forkable1;
            yielding = yielding1;
            statefulness = statefulness1
          } =
        m1
      in
      let { areality = areality2;
            linearity = linearity2;
            portability = portability2;
            forkable = forkable2;
            yielding = yielding2;
            statefulness = statefulness2
          } =
        m2
      in
      Areality.equal areality1 areality2
      && Linearity.equal linearity1 linearity2
      && Portability.equal portability1 portability2
      && Forkable.equal forkable1 forkable2
      && Yielding.equal yielding1 yielding2
      && Statefulness.equal statefulness1 statefulness2

    let join m1 m2 =
      let areality = Areality.join m1.areality m2.areality in
      let linearity = Linearity.join m1.linearity m2.linearity in
      let portability = Portability.join m1.portability m2.portability in
      let forkable = Forkable.join m1.forkable m2.forkable in
      let yielding = Yielding.join m1.yielding m2.yielding in
      let statefulness = Statefulness.join m1.statefulness m2.statefulness in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let meet m1 m2 =
      let areality = Areality.meet m1.areality m2.areality in
      let linearity = Linearity.meet m1.linearity m2.linearity in
      let portability = Portability.meet m1.portability m2.portability in
      let forkable = Forkable.meet m1.forkable m2.forkable in
      let yielding = Yielding.meet m1.yielding m2.yielding in
      let statefulness = Statefulness.meet m1.statefulness m2.statefulness in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let imply m1 m2 =
      let areality = Areality.imply m1.areality m2.areality in
      let linearity = Linearity.imply m1.linearity m2.linearity in
      let portability = Portability.imply m1.portability m2.portability in
      let forkable = Forkable.imply m1.forkable m2.forkable in
      let yielding = Yielding.imply m1.yielding m2.yielding in
      let statefulness = Statefulness.imply m1.statefulness m2.statefulness in
      { areality; linearity; portability; forkable; yielding; statefulness }

    let print ppf m =
      Fmt.fprintf ppf "%a,%a,%a,%a,%a,%a" Areality.print m.areality
        Linearity.print m.linearity Portability.print m.portability
        Forkable.print m.forkable Yielding.print m.yielding Statefulness.print
        m.statefulness
  end
  [@@inline]

  module Opposite (L : CoHeyting) : Heyting with type t = L.t = struct
    type t = L.t

    let min = L.max

    let max = L.min

    let[@inline] le a b = L.le b a

    let equal = L.equal

    let join = L.meet

    let meet = L.join

    let print = L.print

    let imply a b = L.subtract b a
  end
  [@@inline]

  (* Notes on flipping

     Our lattices are split into two opposite fragments: monadic and comonadic.
     Moreover:
     - Morphisms between lattices in the same fragment are always monotone.
     - Morphisms between lattices from opposite fragments are always antitone.

     [Solver_mono] only supports monotone morphisms. Due to this limitation,
     here, we flip all lattices in the monadic fragment, which makes morphisms
     between opposite fragments monotone. We submit this category of lattices
     (original comonadic lattices + flipped monadic lattices) to [Solver_mono].

     The resulted interface given by [Solver_mono] therefore has the monadic
     lattices flipped. We build on top of that and provide an interface to the
     downstream code where monadic lattices are flipped back to its original
     ordering. See [module Monadic_gen] and [module Monadic].
  *)
  module Uniqueness_op = Opposite (Uniqueness)
  module Contention_op = Opposite (Contention)
  module Visibility_op = Opposite (Visibility)
  module Staticity_op = Opposite (Staticity)
  module Monadic_op = Opposite (Monadic)
  module Comonadic_with_locality = Comonadic_with (Locality)
  module Comonadic_with_regionality = Comonadic_with (Regionality)

  type 'a obj =
    | Locality : Locality.t obj
    | Regionality : Regionality.t obj
    | Uniqueness_op : Uniqueness_op.t obj
    | Linearity : Linearity.t obj
    | Portability : Portability.t obj
    | Forkable : Forkable.t obj
    | Yielding : Yielding.t obj
    | Statefulness : Statefulness.t obj
    | Contention_op : Contention_op.t obj
    | Visibility_op : Visibility_op.t obj
    | Staticity_op : Staticity_op.t obj
    | Monadic_op : Monadic_op.t obj
    | Comonadic_with_regionality : Comonadic_with_regionality.t obj
    | Comonadic_with_locality : Comonadic_with_locality.t obj

  let is_opposite : type a. a obj -> bool = function
    | Locality -> false
    | Regionality -> false
    | Uniqueness_op -> true
    | Linearity -> false
    | Portability -> false
    | Forkable -> false
    | Yielding -> false
    | Statefulness -> false
    | Contention_op -> true
    | Visibility_op -> true
    | Staticity_op -> true
    | Monadic_op -> true
    | Comonadic_with_locality -> false
    | Comonadic_with_regionality -> false

  let print_obj : type a. _ -> a obj -> unit =
   fun ppf -> function
    | Locality -> Fmt.fprintf ppf "Locality"
    | Regionality -> Fmt.fprintf ppf "Regionality"
    | Uniqueness_op -> Fmt.fprintf ppf "Uniqueness_op"
    | Linearity -> Fmt.fprintf ppf "Linearity"
    | Portability -> Fmt.fprintf ppf "Portability"
    | Forkable -> Fmt.fprintf ppf "Forkable"
    | Yielding -> Fmt.fprintf ppf "Yielding"
    | Statefulness -> Fmt.fprintf ppf "Statefulness"
    | Contention_op -> Fmt.fprintf ppf "Contention_op"
    | Visibility_op -> Fmt.fprintf ppf "Visibility_op"
    | Staticity_op -> Fmt.fprintf ppf "Staticity_op"
    | Monadic_op -> Fmt.fprintf ppf "Monadic_op"
    | Comonadic_with_locality -> Fmt.fprintf ppf "Comonadic_with_locality"
    | Comonadic_with_regionality -> Fmt.fprintf ppf "Comonadic_with_regionality"

  let min : type a. a obj -> a = function
    | Locality -> Locality.min
    | Regionality -> Regionality.min
    | Uniqueness_op -> Uniqueness_op.min
    | Contention_op -> Contention_op.min
    | Visibility_op -> Visibility_op.min
    | Forkable -> Forkable.min
    | Yielding -> Yielding.min
    | Statefulness -> Statefulness.min
    | Linearity -> Linearity.min
    | Portability -> Portability.min
    | Staticity_op -> Staticity_op.min
    | Monadic_op -> Monadic_op.min
    | Comonadic_with_locality -> Comonadic_with_locality.min
    | Comonadic_with_regionality -> Comonadic_with_regionality.min

  let max : type a. a obj -> a = function
    | Locality -> Locality.max
    | Regionality -> Regionality.max
    | Uniqueness_op -> Uniqueness_op.max
    | Contention_op -> Contention_op.max
    | Visibility_op -> Visibility_op.max
    | Linearity -> Linearity.max
    | Portability -> Portability.max
    | Forkable -> Forkable.max
    | Yielding -> Yielding.max
    | Statefulness -> Statefulness.max
    | Staticity_op -> Staticity_op.max
    | Monadic_op -> Monadic_op.max
    | Comonadic_with_locality -> Comonadic_with_locality.max
    | Comonadic_with_regionality -> Comonadic_with_regionality.max

  let le : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Locality -> Locality.le a b
    | Regionality -> Regionality.le a b
    | Uniqueness_op -> Uniqueness_op.le a b
    | Contention_op -> Contention_op.le a b
    | Visibility_op -> Visibility_op.le a b
    | Linearity -> Linearity.le a b
    | Portability -> Portability.le a b
    | Forkable -> Forkable.le a b
    | Yielding -> Yielding.le a b
    | Statefulness -> Statefulness.le a b
    | Staticity_op -> Staticity_op.le a b
    | Monadic_op -> Monadic_op.le a b
    | Comonadic_with_locality -> Comonadic_with_locality.le a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.le a b

  let equal : type a. a obj -> a -> a -> bool =
   fun obj a b ->
    match obj with
    | Locality -> Locality.equal a b
    | Regionality -> Regionality.equal a b
    | Uniqueness_op -> Uniqueness_op.equal a b
    | Contention_op -> Contention_op.equal a b
    | Visibility_op -> Visibility_op.equal a b
    | Linearity -> Linearity.equal a b
    | Portability -> Portability.equal a b
    | Forkable -> Forkable.equal a b
    | Yielding -> Yielding.equal a b
    | Statefulness -> Statefulness.equal a b
    | Staticity_op -> Staticity_op.equal a b
    | Monadic_op -> Monadic_op.equal a b
    | Comonadic_with_locality -> Comonadic_with_locality.equal a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.equal a b

  let join : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.join a b
    | Regionality -> Regionality.join a b
    | Uniqueness_op -> Uniqueness_op.join a b
    | Contention_op -> Contention_op.join a b
    | Visibility_op -> Visibility_op.join a b
    | Linearity -> Linearity.join a b
    | Portability -> Portability.join a b
    | Forkable -> Forkable.join a b
    | Yielding -> Yielding.join a b
    | Statefulness -> Statefulness.join a b
    | Staticity_op -> Staticity_op.join a b
    | Monadic_op -> Monadic_op.join a b
    | Comonadic_with_locality -> Comonadic_with_locality.join a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.join a b

  let meet : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.meet a b
    | Regionality -> Regionality.meet a b
    | Uniqueness_op -> Uniqueness_op.meet a b
    | Contention_op -> Contention_op.meet a b
    | Visibility_op -> Visibility_op.meet a b
    | Linearity -> Linearity.meet a b
    | Portability -> Portability.meet a b
    | Forkable -> Forkable.meet a b
    | Yielding -> Yielding.meet a b
    | Statefulness -> Statefulness.meet a b
    | Staticity_op -> Staticity_op.meet a b
    | Monadic_op -> Monadic_op.meet a b
    | Comonadic_with_locality -> Comonadic_with_locality.meet a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.meet a b

  let imply : type a. a obj -> a -> a -> a =
   fun obj a b ->
    match obj with
    | Locality -> Locality.imply a b
    | Regionality -> Regionality.imply a b
    | Uniqueness_op -> Uniqueness_op.imply a b
    | Contention_op -> Contention_op.imply a b
    | Visibility_op -> Visibility_op.imply a b
    | Linearity -> Linearity.imply a b
    | Portability -> Portability.imply a b
    | Forkable -> Forkable.imply a b
    | Yielding -> Yielding.imply a b
    | Statefulness -> Statefulness.imply a b
    | Staticity_op -> Staticity_op.imply a b
    | Comonadic_with_locality -> Comonadic_with_locality.imply a b
    | Comonadic_with_regionality -> Comonadic_with_regionality.imply a b
    | Monadic_op -> Monadic_op.imply a b

  (* not hotpath, Ok to curry *)
  let print : type a. a obj -> _ -> a -> unit = function
    | Locality -> Locality.print
    | Regionality -> Regionality.print
    | Uniqueness_op -> Uniqueness_op.print
    | Contention_op -> Contention_op.print
    | Visibility_op -> Visibility_op.print
    | Linearity -> Linearity.print
    | Portability -> Portability.print
    | Forkable -> Forkable.print
    | Yielding -> Yielding.print
    | Statefulness -> Statefulness.print
    | Staticity_op -> Staticity_op.print
    | Monadic_op -> Monadic_op.print
    | Comonadic_with_locality -> Comonadic_with_locality.print
    | Comonadic_with_regionality -> Comonadic_with_regionality.print

  let equal_obj : type a b. a obj -> b obj -> (a, b) equality =
    fun a b ->
    match a, b with
    | Locality, Locality -> Equal
    | Regionality, Regionality -> Equal
    | Uniqueness_op, Uniqueness_op -> Equal
    | Contention_op, Contention_op -> Equal
    | Visibility_op, Visibility_op -> Equal
    | Linearity, Linearity -> Equal
    | Portability, Portability -> Equal
    | Forkable, Forkable -> Equal
    | Yielding, Yielding -> Equal
    | Statefulness, Statefulness -> Equal
    | Staticity_op, Staticity_op -> Equal
    | Monadic_op, Monadic_op -> Equal
    | Comonadic_with_locality, Comonadic_with_locality -> Equal
    | Comonadic_with_regionality, Comonadic_with_regionality -> Equal
    | ( ( Locality | Regionality | Uniqueness_op | Contention_op
          | Visibility_op | Linearity | Portability | Forkable | Yielding
          | Statefulness | Staticity_op | Monadic_op
          | Comonadic_with_locality | Comonadic_with_regionality ),
        _ ) ->
        Not_equal


  let compare_obj : type a b. a obj -> b obj -> (a, b) comparison =
    fun a b ->
    match a, b with
    | Locality, Locality -> Equal
    | Locality, _ -> Less_than
    | _, Locality -> Greater_than
    | Regionality, Regionality -> Equal
    | Regionality, _ -> Less_than
    | _, Regionality -> Greater_than
    | Uniqueness_op, Uniqueness_op -> Equal
    | Uniqueness_op, _ -> Less_than
    | _, Uniqueness_op -> Greater_than
    | Linearity, Linearity -> Equal
    | Linearity, _ -> Less_than
    | _, Linearity -> Greater_than
    | Portability, Portability -> Equal
    | Portability, _ -> Less_than
    | _, Portability -> Greater_than
    | Forkable, Forkable -> Equal
    | Forkable, _ -> Less_than
    | _, Forkable -> Greater_than
    | Yielding, Yielding -> Equal
    | Yielding, _ -> Less_than
    | _, Yielding -> Greater_than
    | Statefulness, Statefulness -> Equal
    | Statefulness, _ -> Less_than
    | _, Statefulness -> Greater_than
    | Contention_op, Contention_op -> Equal
    | Contention_op, _ -> Less_than
    | _, Contention_op -> Greater_than
    | Visibility_op, Visibility_op -> Equal
    | Visibility_op, _ -> Less_than
    | _, Visibility_op -> Greater_than
    | Staticity_op, Staticity_op -> Equal
    | Staticity_op, _ -> Less_than
    | _, Staticity_op -> Greater_than
    | Monadic_op, Monadic_op -> Equal
    | Monadic_op, _ -> Less_than
    | _, Monadic_op -> Greater_than
    | Comonadic_with_regionality, Comonadic_with_regionality -> Equal
    | Comonadic_with_regionality, _ -> Less_than
    | _, Comonadic_with_regionality -> Greater_than
    | Comonadic_with_locality, Comonadic_with_locality -> Equal

end

module Lattices_mono = struct
  include Lattices

  module Axis = struct
    type ('t, 'r) t =
      | Areality : ('a comonadic_with, 'a) t
      | Forkable : ('areality comonadic_with, Forkable.t) t
      | Yielding : ('areality comonadic_with, Yielding.t) t
      | Linearity : ('areality comonadic_with, Linearity.t) t
      | Statefulness : ('areality comonadic_with, Statefulness.t) t
      | Portability : ('areality comonadic_with, Portability.t) t
      | Uniqueness : (Monadic_op.t, Uniqueness_op.t) t
      | Visibility : (Monadic_op.t, Visibility_op.t) t
      | Contention : (Monadic_op.t, Contention_op.t) t
      | Staticity : (Monadic_op.t, Staticity_op.t) t

    let print : type p r. _ -> (p, r) t -> unit =
     fun ppf -> function
      | Areality -> Fmt.fprintf ppf "locality"
      | Linearity -> Fmt.fprintf ppf "linearity"
      | Portability -> Fmt.fprintf ppf "portability"
      | Uniqueness -> Fmt.fprintf ppf "uniqueness"
      | Contention -> Fmt.fprintf ppf "contention"
      | Forkable -> Fmt.fprintf ppf "forkable"
      | Yielding -> Fmt.fprintf ppf "yielding"
      | Statefulness -> Fmt.fprintf ppf "statefulness"
      | Visibility -> Fmt.fprintf ppf "visibility"
      | Staticity -> Fmt.fprintf ppf "staticity"

    let equal : type p r1 r2. (p, r1) t -> (p, r2) t -> (r1, r2) equality =
     fun ax1 ax2 ->
      match ax1, ax2 with
      | Areality, Areality -> Equal
      | Linearity, Linearity -> Equal
      | Portability, Portability -> Equal
      | Uniqueness, Uniqueness -> Equal
      | Contention, Contention -> Equal
      | Forkable, Forkable -> Equal
      | Yielding, Yielding -> Equal
      | Statefulness, Statefulness -> Equal
      | Visibility, Visibility -> Equal
      | Staticity, Staticity -> Equal
      | ( ( Areality | Linearity | Uniqueness | Portability | Contention
          | Forkable | Yielding | Statefulness | Visibility | Staticity ),
          _ ) ->
        Not_equal

    let compare :
        type p r1 r2. (p, r1) t -> (p, r2) t -> (r1, r2) comparison =
     fun ax1 ax2 ->
      match ax1, ax2 with
      | Areality, Areality -> Equal
      | Areality, _ -> Less_than
      | _, Areality -> Greater_than
      | Forkable, Forkable -> Equal
      | Forkable, _ -> Less_than
      | _, Forkable -> Greater_than
      | Yielding, Yielding -> Equal
      | Yielding, _ -> Less_than
      | _, Yielding -> Greater_than
      | Linearity, Linearity -> Equal
      | Linearity, _ -> Less_than
      | _, Linearity -> Greater_than
      | Statefulness, Statefulness -> Equal
      | Statefulness, _ -> Less_than
      | _, Statefulness -> Greater_than
      | Portability, Portability -> Equal
      | Portability, _ -> .
      | _, Portability -> .
      | Uniqueness, Uniqueness -> Equal
      | Uniqueness, _ -> Less_than
      | _, Uniqueness -> Greater_than
      | Visibility, Visibility -> Equal
      | Visibility, _ -> Less_than
      | _, Visibility -> Greater_than
      | Contention, Contention -> Equal
      | Contention, _ -> Less_than
      | _, Contention -> Greater_than
      | Staticity, Staticity -> Equal
      | Staticity, _ -> .
      | _, Staticity -> .

    let proj : type p r. (p, r) t -> p -> r =
     fun ax t ->
      match ax with
      | Areality -> t.areality
      | Linearity -> t.linearity
      | Portability -> t.portability
      | Forkable -> t.forkable
      | Yielding -> t.yielding
      | Statefulness -> t.statefulness
      | Uniqueness -> t.uniqueness
      | Contention -> t.contention
      | Visibility -> t.visibility
      | Staticity -> t.staticity

    let set : type p r. (p, r) t -> r -> p -> p =
     fun ax r t ->
      match ax with
      | Areality -> { t with areality = r }
      | Linearity -> { t with linearity = r }
      | Portability -> { t with portability = r }
      | Forkable -> { t with forkable = r }
      | Yielding -> { t with yielding = r }
      | Statefulness -> { t with statefulness = r }
      | Uniqueness -> { t with uniqueness = r }
      | Contention -> { t with contention = r }
      | Visibility -> { t with visibility = r }
      | Staticity -> { t with staticity = r }
  end

  type ('a, 'b, 'd) locality_morph =
    (* Following is a chain of adjunctions (this can be extended one
       further, but we never need the missing operation) *)
    | Local_to_regional :
        (Locality.t,
         Regionality.t, 'l * disallowed) locality_morph
        (** Maps local to regional, global to global *)
    | Regional_to_local :
        (Regionality.t,
         Locality.t, 'l * 'r) locality_morph
        (** Maps regional to local, identity otherwise *)
    | Locality_as_regionality :
        (Locality.t,
         Regionality.t, 'l * 'r) locality_morph
        (** Inject locality into regionality  *)
    | Regional_to_global :
        (Regionality.t,
         Locality.t, disallowed * 'r) locality_morph
        (** Maps regional to global, identity otherwise *)
    (* Versions of the above morphisms operating on regionality. *)
    | Local_to_regional_regionality :
        (Regionality.t,
         Regionality.t, 'l * disallowed) locality_morph
        (** Maps regional to local, identity otherwise. *)
    | Regional_to_local_regionality :
        (Regionality.t,
         Regionality.t, 'l * 'r) locality_morph
        (** Maps regional to local, identity otherwise. *)
    | Regional_to_global_regionality :
        (Regionality.t,
         Regionality.t, disallowed * 'r) locality_morph
        (** Maps regional to global, identity otherwise. *)

  type ('a, 'b, 'd) core_morph =
    | Locality_restricted
      : ('a, 'b, 'l * 'r) locality_morph
        -> ('a, 'b, 'l * 'r) core_morph
    | Locality_full
      : ('a, 'b, 'l * 'r) locality_morph
        -> ('a comonadic_with, 'b comonadic_with, 'l * 'r) core_morph
    | Uniqueness_op_to_linearity
      : (Uniqueness_op.t, Linearity.t, 'l * 'r) core_morph
    | Linearity_to_uniqueness_op
      : (Linearity.t, Uniqueness_op.t, 'l * 'r) core_morph
    | Contention_op_to_portability
      : (Contention_op.t, Portability.t, 'l * 'r) core_morph
    | Portability_to_contention_op
      : (Portability.t, Contention_op.t, 'l * 'r) core_morph
    | Visibility_op_to_statefulness
      : (Visibility_op.t, Statefulness.t, 'l * 'r) core_morph
    | Statefulness_to_visibility_op
      : (Statefulness.t, Visibility_op.t, 'l * 'r) core_morph
    | Monadic_to_comonadic_min
      : (Monadic_op.t, 'a comonadic_with, 'l * disallowed) core_morph
      (** Dualize the monadic fragment to the comonadic fragment.
          The areality is set to min. *)
    | Comonadic_to_monadic_min
      : 'a areality
        -> ('a comonadic_with, Monadic_op.t, 'l * disallowed) core_morph
      (** Dualize the comonadic fragment to the monadic fragment.  The
          areality axis is ignored, the staticity axis is set to min. *)
    | Monadic_to_comonadic_max
      : (Monadic_op.t, 'a comonadic_with, disallowed * 'r) core_morph
      (** Dualize the monadic fragment to the comonadic fragment.
          The areality is set to max. *)
    | Comonadic_to_monadic_max
      : 'a areality
        -> ('a comonadic_with, Monadic_op.t, disallowed * 'r) core_morph
      (** Dualize the comonadic fragment to the monadic fragment.
          The areality axis is ignored.  *)

  type ('a, 'b, 'd) simple_morph =
    | Id : ('a, 'a, 'd) simple_morph  (** identity morphism *)
    | Core : ('a, 'b, 'd) core_morph -> ('a, 'b, 'd) simple_morph
    | Meet_const : 'a -> ('a, 'a, 'l * disallowed) simple_morph
        (** Meet the input with the parameter *)
    | Imply_const : 'a -> ('a, 'a, disallowed * 'r) simple_morph
        (** The right adjoint of [Meet_const] *)
    | Core_and_meet_const :
        'b * ('a, 'b, 'l * disallowed) core_morph
        -> ('a, 'b, 'l * disallowed) simple_morph
        (** Composition of [Core] and [Meet_const]. We only need to include one
            order of composition because currently all our core left morphisms
            preserve binary meets. *)
    | Imply_const_and_core :
        ('a, 'b, disallowed * 'r) core_morph * 'a
        -> ('a, 'b, disallowed * 'r) simple_morph
        (** Composition of [Core] and [Imply_const]. We only need to include one
            order of composition because currently all our core right morphisms
            commute with implication. *)
    | Compose : ('b, 'c, neither) simple_morph * ('a, 'b, neither) simple_morph
                -> ('a, 'c, neither) simple_morph
        (** Compoistion of two morphisms. We don't allow compositions to
            appear on either side to ensure that there are a finite
            number of morphisms we can encounter in practice. *)

  type ('a, 'b, 'd) morph =
    | Simple : ('a, 'b, 'd) simple_morph -> ('a, 'b, 'd) morph
    | Const_max :
        'a obj -> ('a, 'c, disallowed * 'r) morph
        (** Discards an arbitrary input and apply a simple morphism to
            the maximum of a lattice *)
    | Const_min :
        'a obj -> ('a, 'c, 'l * disallowed) morph
        (** Discards an arbitrary input and apply a simple morphism to
            the minimum of a lattice *)
    | Const :
        'a obj * 'c -> ('a, 'c, neither) morph
        (** A constant function on any constant: we don't allow arbitrary
            constant functions to appear on either side *)
    | Proj_and :
        ('p, 'q, 'd) simple_morph * ('s, 'p) Axis.t * 's obj
        -> ('s, 'q, 'd) morph
        (** Composition of projecting out an axis and a simple morphism. *)
    | And_max_with :
         ('s, 'q) Axis.t * ('p, 'q, disallowed * 'r) simple_morph
        -> ('p, 's, disallowed * 'r) morph
        (** Composition of a morphism and combining an axis with the
            maxima along other axes. *)
    | And_min_with :
        ('s, 'q) Axis.t * ('p, 'q, 'l * disallowed) simple_morph
        -> ('p, 's, 'l * disallowed) morph
        (** Composition of a morphism and combining an axis with the
            minima along other axes. *)
    | Compose : ('b, 'c, neither) morph * ('a, 'b, neither) morph
                -> ('a, 'c, neither) morph
        (** Compoistion of two morphisms. We don't allow compositions to
            appear on either side to ensure that there are a finite
            number of morphisms we can encounter in practice. *)

  include Magic_allow_disallow (struct
    type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph
      constraint 'd = _ * _

    let allow_left_locality :
          type a b l r. (a, b, allowed * r) locality_morph
               -> (a, b, l * r) locality_morph =
      function
      | Local_to_regional -> Local_to_regional
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_local -> Regional_to_local
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality

    let allow_left_core :
          type a b l r. (a, b, allowed * r) core_morph -> (a, b, l * r) core_morph =
      function
      | Locality_restricted m -> Locality_restricted (allow_left_locality m)
      | Locality_full m -> Locality_full (allow_left_locality m)
      | Uniqueness_op_to_linearity -> Uniqueness_op_to_linearity
      | Linearity_to_uniqueness_op -> Linearity_to_uniqueness_op
      | Contention_op_to_portability -> Contention_op_to_portability
      | Portability_to_contention_op -> Portability_to_contention_op
      | Visibility_op_to_statefulness -> Visibility_op_to_statefulness
      | Statefulness_to_visibility_op -> Statefulness_to_visibility_op
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic_min a -> Comonadic_to_monadic_min a

    let allow_left_simple :
        type a b l r. (a, b, allowed * r) simple_morph -> (a, b, l * r) simple_morph =
      function
      | Id -> Id
      | Core m -> Core (allow_left_core m)
      | Meet_const c -> Meet_const c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, allow_left_core m)

    let allow_left :
        type a b l r. (a, b, allowed * r) morph -> (a, b, l * r) morph =
      function
      | Simple m -> Simple (allow_left_simple m)
      | Proj_and(m, ax, src) -> Proj_and(allow_left_simple m, ax, src)
      | And_min_with(ax, m) -> And_min_with(ax, allow_left_simple m)
      | Const_min src -> Const_min src

    let allow_right_locality :
        type a b l r. (a, b, l * allowed) locality_morph
             -> (a, b, l * r) locality_morph =
      function
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let allow_right_core :
        type a b l r. (a, b, l * allowed) core_morph -> (a, b, l * r) core_morph =
      function
      | Locality_restricted m -> Locality_restricted (allow_right_locality m)
      | Locality_full m -> Locality_full (allow_right_locality m)
      | Uniqueness_op_to_linearity -> Uniqueness_op_to_linearity
      | Linearity_to_uniqueness_op -> Linearity_to_uniqueness_op
      | Contention_op_to_portability -> Contention_op_to_portability
      | Portability_to_contention_op -> Portability_to_contention_op
      | Visibility_op_to_statefulness -> Visibility_op_to_statefulness
      | Statefulness_to_visibility_op -> Statefulness_to_visibility_op
      | Comonadic_to_monadic_max a -> Comonadic_to_monadic_max a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max

    let allow_right_simple :
        type a b l r. (a, b, l * allowed) simple_morph
             -> (a, b, l * r) simple_morph =
      function
      | Id -> Id
      | Core m -> Core (allow_right_core m)
      | Imply_const c -> Imply_const c
      | Imply_const_and_core(m, c) -> Imply_const_and_core(allow_right_core m, c)

    let allow_right :
        type a b l r. (a, b, l * allowed) morph -> (a, b, l * r) morph =
      function
      | Simple m -> Simple (allow_right_simple m)
      | Proj_and(m, ax, src) -> Proj_and(allow_right_simple m, ax, src)
      | And_max_with(ax, m) -> And_max_with(ax, allow_right_simple m)
      | Const_max src -> Const_max src

    let disallow_left_locality :
        type a b l r.
             (a, b, l * r) locality_morph -> (a, b, disallowed * r) locality_morph =
      function
      | Local_to_regional -> Local_to_regional
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let disallow_left_core :
        type a b l r.
             (a, b, l * r) core_morph -> (a, b, disallowed * r) core_morph =
      function
      | Locality_restricted m -> Locality_restricted (disallow_left_locality m)
      | Locality_full m -> Locality_full (disallow_left_locality m)
      | Uniqueness_op_to_linearity -> Uniqueness_op_to_linearity
      | Linearity_to_uniqueness_op -> Linearity_to_uniqueness_op
      | Contention_op_to_portability -> Contention_op_to_portability
      | Portability_to_contention_op -> Portability_to_contention_op
      | Visibility_op_to_statefulness -> Visibility_op_to_statefulness
      | Statefulness_to_visibility_op -> Statefulness_to_visibility_op
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic_min a -> Comonadic_to_monadic_min a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Comonadic_to_monadic_max a -> Comonadic_to_monadic_max a

    let rec disallow_left_simple :
        type a b l r. (a, b, l * r) simple_morph
             -> (a, b, disallowed * r) simple_morph =
      function
      | Id -> Id
      | Core m -> Core (disallow_left_core m)
      | Meet_const c -> Meet_const c
      | Imply_const c -> Imply_const c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, disallow_left_core m)
      | Imply_const_and_core(m, c) -> Imply_const_and_core(disallow_left_core m, c)
      | Compose (mb, ma) ->
        let mb = disallow_left_simple mb in
        let ma = disallow_left_simple ma in
        Compose (mb, ma)

    let rec disallow_left :
        type a b l r. (a, b, l * r) morph -> (a, b, disallowed * r) morph =
      function
      | Simple m -> Simple (disallow_left_simple m)
      | Proj_and(m, ax, src) -> Proj_and(disallow_left_simple m, ax, src)
      | And_max_with(ax, m) -> And_max_with(ax, disallow_left_simple m)
      | And_min_with(ax, m) -> And_min_with(ax, disallow_left_simple m)
      | Const_max src -> Const_max src
      | Const_min src -> Const_min src
      | Const (src,c) -> Const (src,c)
      | Compose (mb, ma) ->
        let mb = disallow_left mb in
        let ma = disallow_left ma in
        Compose (mb, ma)

    let disallow_right_locality :
        type a b l r.
             (a, b, l * r) locality_morph -> (a, b, l * disallowed) locality_morph =
      function
      | Local_to_regional -> Local_to_regional
      | Regional_to_local -> Regional_to_local
      | Locality_as_regionality -> Locality_as_regionality
      | Regional_to_global -> Regional_to_global
      | Local_to_regional_regionality -> Local_to_regional_regionality
      | Regional_to_local_regionality -> Regional_to_local_regionality
      | Regional_to_global_regionality -> Regional_to_global_regionality

    let disallow_right_core :
        type a b l r.
             (a, b, l * r) core_morph -> (a, b, l * disallowed) core_morph =
      function
      | Locality_restricted m -> Locality_restricted (disallow_right_locality m)
      | Locality_full m -> Locality_full (disallow_right_locality m)
      | Uniqueness_op_to_linearity -> Uniqueness_op_to_linearity
      | Linearity_to_uniqueness_op -> Linearity_to_uniqueness_op
      | Contention_op_to_portability -> Contention_op_to_portability
      | Portability_to_contention_op -> Portability_to_contention_op
      | Visibility_op_to_statefulness -> Visibility_op_to_statefulness
      | Statefulness_to_visibility_op -> Statefulness_to_visibility_op
      | Monadic_to_comonadic_min -> Monadic_to_comonadic_min
      | Comonadic_to_monadic_min a -> Comonadic_to_monadic_min a
      | Monadic_to_comonadic_max -> Monadic_to_comonadic_max
      | Comonadic_to_monadic_max a -> Comonadic_to_monadic_max a

    let rec disallow_right_simple :
        type a b l r. (a, b, l * r) simple_morph
             -> (a, b, l * disallowed) simple_morph =
      function
      | Id -> Id
      | Core m -> Core (disallow_right_core m)
      | Meet_const c -> Meet_const c
      | Imply_const c -> Imply_const c
      | Core_and_meet_const(c, m) -> Core_and_meet_const(c, disallow_right_core m)
      | Imply_const_and_core(m, c) -> Imply_const_and_core(disallow_right_core m, c)
      | Compose (mb, ma) ->
        let mb = disallow_right_simple mb in
        let ma = disallow_right_simple ma in
        Compose (mb, ma)

    let rec disallow_right :
        type a b l r. (a, b, l * r) morph -> (a, b, l * disallowed) morph =
      function
      | Simple m -> Simple (disallow_right_simple m)
      | Proj_and(m, ax, src) -> Proj_and(disallow_right_simple m, ax, src)
      | And_max_with(ax, m) ->
          And_max_with(ax, disallow_right_simple m)
      | And_min_with(ax, m) ->
          And_min_with(ax, disallow_right_simple m)
      | Const_max src -> Const_max src
      | Const_min src -> Const_min src
      | Const (src,c) -> Const (src,c)
      | Compose (mb, ma) ->
        let mb = disallow_right mb in
        let ma = disallow_right ma in
        Compose (mb, ma)

  end)

  let proj_obj : type t r. (t, r) Axis.t -> t obj -> r obj =
   fun ax obj ->
    match ax, obj with
    | Areality, Comonadic_with_locality -> Locality
    | Areality, Comonadic_with_regionality -> Regionality
    | Linearity, Comonadic_with_locality -> Linearity
    | Linearity, Comonadic_with_regionality -> Linearity
    | Portability, Comonadic_with_locality -> Portability
    | Portability, Comonadic_with_regionality -> Portability
    | Forkable, Comonadic_with_locality -> Forkable
    | Forkable, Comonadic_with_regionality -> Forkable
    | Yielding, Comonadic_with_locality -> Yielding
    | Yielding, Comonadic_with_regionality -> Yielding
    | Statefulness, Comonadic_with_locality -> Statefulness
    | Statefulness, Comonadic_with_regionality -> Statefulness
    | Uniqueness, Monadic_op -> Uniqueness_op
    | Contention, Monadic_op -> Contention_op
    | Visibility, Monadic_op -> Visibility_op
    | Staticity, Monadic_op -> Staticity_op

  let areality_comonadic_obj : type a. a areality -> a comonadic_with obj =
    function
    | Locality -> Comonadic_with_locality
    | Regionality -> Comonadic_with_regionality

  let src_locality_restricted :
        type a b d. (a, b, d) locality_morph -> a obj = function
    | Local_to_regional -> Locality
    | Regional_to_local -> Regionality
    | Locality_as_regionality -> Locality
    | Regional_to_global -> Regionality
    | Local_to_regional_regionality -> Regionality
    | Regional_to_local_regionality -> Regionality
    | Regional_to_global_regionality -> Regionality

  let src_locality_full :
        type a b d. (a, b, d) locality_morph -> (a comonadic_with) obj = function
    | Local_to_regional -> Comonadic_with_locality
    | Regional_to_local -> Comonadic_with_regionality
    | Locality_as_regionality -> Comonadic_with_locality
    | Regional_to_global -> Comonadic_with_regionality
    | Local_to_regional_regionality -> Comonadic_with_regionality
    | Regional_to_local_regionality -> Comonadic_with_regionality
    | Regional_to_global_regionality -> Comonadic_with_regionality

  let src_core : type a b d. (a, b, d) core_morph -> a obj = function
    | Locality_restricted m -> src_locality_restricted m
    | Locality_full m -> src_locality_full m
    | Uniqueness_op_to_linearity -> Uniqueness_op
    | Linearity_to_uniqueness_op -> Linearity
    | Contention_op_to_portability -> Contention_op
    | Portability_to_contention_op -> Portability
    | Visibility_op_to_statefulness -> Visibility_op
    | Statefulness_to_visibility_op -> Statefulness
    | Monadic_to_comonadic_min -> Monadic_op
    | Comonadic_to_monadic_min ar -> areality_comonadic_obj ar
    | Monadic_to_comonadic_max -> Monadic_op
    | Comonadic_to_monadic_max ar -> areality_comonadic_obj ar


  let comonadic_obj_areality : type a. a comonadic_with obj -> a areality =
    function
    | Comonadic_with_locality -> Locality
    | Comonadic_with_regionality -> Regionality

  let comonadic_with_obj : type a. a obj -> a comonadic_with obj =
   fun a0 ->
    match a0 with
    | Locality -> Comonadic_with_locality
    | Regionality -> Comonadic_with_regionality
    | Uniqueness_op | Linearity | Monadic_op | Comonadic_with_regionality
    | Comonadic_with_locality | Contention_op | Visibility_op | Portability
    | Forkable | Yielding | Statefulness | Staticity_op ->
      assert false

  let rec src_simple
          : type a b d. b obj -> (a, b, d) simple_morph -> a obj =
   fun dst f ->
    match f with
    | Id -> dst
    | Core m -> src_core m
    | Meet_const _ -> dst
    | Imply_const _ -> dst
    | Core_and_meet_const(_, m) -> src_core m
    | Imply_const_and_core(m, _) -> src_core m
    | Compose (mb, ma) ->
      let mid = src_simple dst mb in
      src_simple mid ma

  let rec src : type a b d. b obj -> (a, b, d) morph -> a obj =
   fun dst f ->
    match f with
    | Simple m -> src_simple dst m
    | Proj_and(_, _, src) -> src
    | And_max_with(ax, m) -> src_simple (proj_obj ax dst) m
    | And_min_with(ax, m) -> src_simple (proj_obj ax dst) m
    | Const_min src -> src
    | Const_max src -> src
    | Const (src,_) -> src
    | Compose (mb, ma) ->
      let mid = src dst mb in
      src mid ma

  let equal_locality_morph :
        type a1 l1 r1 a2 b l2 r2.
        (a1, b, l1 * r1) locality_morph ->
        (a2, b, l2 * r2) locality_morph ->
        (a1, a2) equality =
     fun f1 f2 ->
      match f1, f2 with
      | Local_to_regional, Local_to_regional -> Equal
      | Regional_to_local, Regional_to_local -> Equal
      | Locality_as_regionality, Locality_as_regionality -> Equal
      | Regional_to_global, Regional_to_global -> Equal
      | Local_to_regional_regionality, Local_to_regional_regionality -> Equal
      | Regional_to_local_regionality, Regional_to_local_regionality -> Equal
      | Regional_to_global_regionality, Regional_to_global_regionality -> Equal
      | ( ( Local_to_regional | Regional_to_local | Locality_as_regionality
          | Regional_to_global | Local_to_regional_regionality
          | Regional_to_local_regionality | Regional_to_global_regionality ),
          _ ) ->
        Not_equal

  let equal_core_morph :
        type a1 l1 r1 a2 b l2 r2.
        (a1, b, l1 * r1) core_morph ->
        (a2, b, l2 * r2) core_morph ->
        (a1, a2) equality =
     fun f1 f2 ->
      match f1, f2 with
      | Locality_restricted l1, Locality_restricted l2 ->
          equal_locality_morph l1 l2
      | Locality_full l1, Locality_full l2 -> begin
          match equal_locality_morph l1 l2 with
          | Not_equal as eq -> eq
          | Equal as eq -> eq
        end
      | Uniqueness_op_to_linearity, Uniqueness_op_to_linearity -> Equal
      | Linearity_to_uniqueness_op, Linearity_to_uniqueness_op -> Equal
      | Contention_op_to_portability, Contention_op_to_portability -> Equal
      | Portability_to_contention_op, Portability_to_contention_op -> Equal
      | Visibility_op_to_statefulness, Visibility_op_to_statefulness -> Equal
      | Statefulness_to_visibility_op, Statefulness_to_visibility_op -> Equal
      | Monadic_to_comonadic_min, Monadic_to_comonadic_min -> Equal
      | Comonadic_to_monadic_min a1, Comonadic_to_monadic_min a2 -> begin
          match equal_areality a1 a2 with
          | Not_equal as eq -> eq
          | Equal as eq -> eq
        end
      | Monadic_to_comonadic_max, Monadic_to_comonadic_max -> Equal
      | Comonadic_to_monadic_max a1, Comonadic_to_monadic_max a2 -> begin
          match equal_areality a1 a2 with
          | Not_equal as eq -> eq
          | Equal as eq -> eq
        end
      | ( ( Locality_restricted _ | Locality_full _
          | Uniqueness_op_to_linearity | Linearity_to_uniqueness_op
          | Contention_op_to_portability | Portability_to_contention_op
          | Visibility_op_to_statefulness | Statefulness_to_visibility_op
          | Monadic_to_comonadic_min | Comonadic_to_monadic_min _
          | Monadic_to_comonadic_max | Comonadic_to_monadic_max _),
          _ ) ->
        Not_equal

  let rec equal_simple_morph :
        type a1 l1 r1 a2 b l2 r2.
        b obj ->
        (a1, b, l1 * r1) simple_morph ->
        (a2, b, l2 * r2) simple_morph ->
        (a1, a2) equality =
     fun dst f1 f2 ->
      match f1, f2 with
      | Id, Id -> Equal
      | Core m1, Core m2 -> equal_core_morph m1 m2
      | Meet_const c1, Meet_const c2 ->
        if equal dst c1 c2 then Equal else Not_equal
      | Imply_const c1, Imply_const c2 ->
          if equal dst c1 c2 then Equal else Not_equal
      | Core_and_meet_const(c1, m1), Core_and_meet_const(c2, m2) ->
          if equal dst c1 c2 then begin
            match equal_core_morph m1 m2 with
            | Not_equal as eq -> eq
            | Equal as eq -> eq
          end else Not_equal
      | Imply_const_and_core(m1, c1), Imply_const_and_core(m2, c2) -> begin
          match equal_core_morph m1 m2 with
          | Not_equal -> Not_equal
          | Equal ->
              if equal (src_core m1) c1 c2 then Equal
              else Not_equal
        end
      | Compose(mb1, ma1), Compose(mb2, ma2) -> begin
          match equal_simple_morph dst mb1 mb2 with
          | Not_equal -> Not_equal
          | Equal ->
              match equal_simple_morph (src_simple dst mb1) ma1 ma2 with
              | Not_equal as eq -> eq
              | Equal as eq -> eq
        end
      | ( ( Id | Core _ | Meet_const _ | Imply_const _
            | Core_and_meet_const _ | Imply_const_and_core _
            | Compose _ ),
          _ ) ->
        Not_equal

  let rec equal_morph :
        type a1 l1 r1 a2 b l2 r2.
        b obj ->
        (a1, b, l1 * r1) morph ->
        (a2, b, l2 * r2) morph ->
        (a1, a2) equality =
     fun dst f1 f2 ->
      match f1, f2 with
      | Simple m1, Simple m2 -> equal_simple_morph dst m1 m2
      | Const_max src1, Const_max src2 -> equal_obj src1 src2
      | Const_min src1, Const_min src2 -> equal_obj src1 src2
      | Const (src1,c1), Const (src2,c2) -> begin
          match equal_obj src1 src2 with
          | Not_equal -> Not_equal
          | Equal -> if equal dst c1 c2 then Equal else Not_equal
        end
      | Proj_and(m1, ax1, src1), Proj_and(m2, ax2, src2) -> begin
          match equal_simple_morph dst m1 m2 with
          | Not_equal -> Not_equal
          | Equal ->
              match equal_obj src1 src2 with
              | Not_equal -> Not_equal
              | Equal ->
                  match Axis.equal ax1 ax2 with
                  | Not_equal as eq -> eq
                  | Equal as eq -> eq
        end
      | And_max_with(ax1, m1), And_max_with(ax2, m2) -> begin
          match Axis.equal ax1 ax2 with
          | Not_equal as eq -> eq
          | Equal -> equal_simple_morph (proj_obj ax1 dst) m1 m2
        end
      | And_min_with(ax1, m1), And_min_with(ax2, m2) -> begin
          match Axis.equal ax1 ax2 with
          | Not_equal as eq -> eq
          | Equal -> equal_simple_morph (proj_obj ax1 dst) m1 m2
        end
      | Compose(mb1, ma1), Compose(mb2, ma2) -> begin
          match equal_morph dst mb1 mb2 with
          | Not_equal -> Not_equal
          | Equal ->
              match equal_morph (src dst mb1) ma1 ma2 with
              | Not_equal as eq -> eq
              | Equal as eq -> eq
        end
      | ((Simple _ | Proj_and _ | And_max_with _ | And_min_with _ | Const_max _
          | Const_min _ | Const _ | Compose _), _) ->
          Not_equal

  let compare_locality_morph :
        type a1 l1 r1 a2 b l2 r2.
        (a1, b, l1 * r1) locality_morph ->
        (a2, b, l2 * r2) locality_morph ->
        (a1, a2) comparison =
    fun m1 m2 ->
    match m1, m2 with
    | Local_to_regional, Local_to_regional -> Equal
    | Local_to_regional, _ -> Less_than
    | _, Local_to_regional -> Greater_than
    | Regional_to_local, Regional_to_local -> Equal
    | Regional_to_local, _ -> Less_than
    | _, Regional_to_local -> Greater_than
    | Locality_as_regionality, Locality_as_regionality -> Equal
    | Locality_as_regionality, _ -> Less_than
    | _, Locality_as_regionality -> Greater_than
    | Regional_to_global, Regional_to_global -> Equal
    | Regional_to_global, _ -> .
    | _, Regional_to_global -> .
    | Local_to_regional_regionality, Local_to_regional_regionality -> Equal
    | Local_to_regional_regionality, _ -> Less_than
    | _, Local_to_regional_regionality -> Greater_than
    | Regional_to_local_regionality, Regional_to_local_regionality -> Equal
    | Regional_to_local_regionality, _ -> Less_than
    | _, Regional_to_local_regionality -> Greater_than
    | Regional_to_global_regionality, Regional_to_global_regionality -> Equal
    | Regional_to_global_regionality, _ -> .
    | _, Regional_to_global_regionality -> .

  let compare_core_morph :
            type a1 d1 a2 b d2.
        (a1, b, d1) core_morph ->
        (a2, b, d2) core_morph ->
        (a1, a2) comparison =
    fun m1 m2 ->
    match m1, m2 with
    | Locality_restricted l1, Locality_restricted l2 ->
        compare_locality_morph l1 l2
    | Locality_restricted _, _ -> .
    | _, Locality_restricted _ -> .
    | Locality_full l1, Locality_full l2 -> begin
        match compare_locality_morph l1 l2 with
        | Equal as c -> c
        | Less_than | Greater_than as c -> c
      end
    | Locality_full _, _ -> Less_than
    | _, Locality_full _ -> Greater_than
    | Uniqueness_op_to_linearity, Uniqueness_op_to_linearity -> Equal
    | Uniqueness_op_to_linearity, _ -> .
    | _, Uniqueness_op_to_linearity -> .
    | Linearity_to_uniqueness_op, Linearity_to_uniqueness_op -> Equal
    | Linearity_to_uniqueness_op, _ -> .
    | _, Linearity_to_uniqueness_op -> .
    | Contention_op_to_portability, Contention_op_to_portability -> Equal
    | Contention_op_to_portability, _ -> .
    | _, Contention_op_to_portability -> .
    | Portability_to_contention_op, Portability_to_contention_op -> Equal
    | Portability_to_contention_op, _ -> .
    | _, Portability_to_contention_op -> .
    | Visibility_op_to_statefulness, Visibility_op_to_statefulness -> Equal
    | Visibility_op_to_statefulness, _ -> .
    | _, Visibility_op_to_statefulness -> .
    | Statefulness_to_visibility_op, Statefulness_to_visibility_op -> Equal
    | Statefulness_to_visibility_op, _ -> .
    | _, Statefulness_to_visibility_op -> .
    | Monadic_to_comonadic_min, Monadic_to_comonadic_min -> Equal
    | Monadic_to_comonadic_min, _ -> Less_than
    | _, Monadic_to_comonadic_min -> Greater_than
    | Comonadic_to_monadic_min ar1, Comonadic_to_monadic_min ar2 -> begin
        match compare_areality ar1 ar2 with
        | Equal -> Equal
        | Less_than -> Less_than
        | Greater_than -> Greater_than
      end
    | Comonadic_to_monadic_min _, _ -> Less_than
    | _, Comonadic_to_monadic_min _ -> Greater_than
    | Monadic_to_comonadic_max, Monadic_to_comonadic_max -> Equal
    | Monadic_to_comonadic_max, _ -> .
    | _, Monadic_to_comonadic_max -> .
    | Comonadic_to_monadic_max ar1, Comonadic_to_monadic_max ar2 -> begin
        match compare_areality ar1 ar2 with
        | Equal -> Equal
        | Less_than -> Less_than
        | Greater_than -> Greater_than
      end
    | Comonadic_to_monadic_max _, _ -> .
    | _, Comonadic_to_monadic_max _ -> .


  let rec compare_simple_morph :
            type a1 d1 a2 b d2.
        b obj ->
        (a1, b, d1) simple_morph ->
        (a2, b, d2) simple_morph ->
        (a1, a2) comparison =
    fun dst m1 m2 ->
    match m1, m2 with
    | Id, Id -> Equal
    | Id, _ -> Less_than
    | _, Id -> Greater_than
    | Core m1, Core m2 -> compare_core_morph m1 m2
    | Core _, _ -> Less_than
    | _, Core _ -> Greater_than
    | Meet_const c1, Meet_const c2 ->
        if le dst c1 c2 then
          if equal dst c1 c2 then Equal
          else Less_than
        else Greater_than
    | Meet_const _, _ -> Less_than
    | _, Meet_const _ -> Greater_than
    | Imply_const c1, Imply_const c2 ->
        if le dst c1 c2 then
          if equal dst c1 c2 then Equal
          else Less_than
        else Greater_than
    | Imply_const _, _ -> Less_than
    | _, Imply_const _ -> Greater_than
    | Core_and_meet_const(c1, m1), Core_and_meet_const(c2, m2) ->
        if le dst c1 c2 then
          if equal dst c1 c2 then
            compare_core_morph m1 m2
          else Less_than
        else Greater_than
    | Core_and_meet_const _, _ -> Less_than
    | _, Core_and_meet_const _ -> Greater_than
    | Imply_const_and_core(m1, c1), Imply_const_and_core(m2, c2) -> begin
        match compare_core_morph m1 m2 with
        | Less_than -> Less_than
        | Greater_than -> Greater_than
        | Equal ->
            let src = src_core m1 in
            if le src c1 c2 then
              if equal src c1 c2 then
                Equal
              else Less_than
            else Greater_than
      end
    | Imply_const_and_core _, _ -> Less_than
    | _, Imply_const_and_core _ -> Greater_than
    | Compose(mb1, ma1), Compose(mb2, ma2) -> begin
        match compare_simple_morph dst mb1 mb2 with
        | Greater_than | Less_than as c -> c
        | Equal -> compare_simple_morph (src_simple dst mb1) ma1 ma2
      end
    | Compose _, _ -> .
    | _, Compose _ -> .

  let rec compare_morph :
            type a1 d1 a2 b d2.
        b obj ->
        (a1, b, d1) morph ->
        (a2, b, d2) morph ->
        (a1, a2) comparison =
    fun dst m1 m2 ->
    match m1, m2 with
    | Simple m1, Simple m2 -> compare_simple_morph dst m1 m2
    | Simple _, _ -> Less_than
    | _, Simple _ -> Greater_than
    | Const_max(obj1), Const_max(obj2) -> compare_obj obj1 obj2
    | Const_max _, _ -> Less_than
    | _, Const_max _ -> Greater_than
    | Const_min(obj1), Const_min(obj2) -> compare_obj obj1 obj2
    | Const_min _, _ -> Less_than
    | _, Const_min _ -> Greater_than
    | Const(obj1,c1), Const(obj2,c2) -> begin
          match compare_obj obj1 obj2 with
          | Less_than | Greater_than as c -> c
          | Equal ->
              if le dst c1 c2 then
                if equal dst c1 c2 then
                  Equal
                else Less_than
              else Greater_than
        end
     | Const _, _ -> Less_than
     | _, Const _ -> Greater_than
     | Proj_and(m1, ax1, src1), Proj_and(m2, ax2, src2) -> begin
          match compare_simple_morph dst m1 m2 with
          | Less_than | Greater_than as c -> c
          | Equal ->
              match compare_obj src1 src2 with
              | Less_than | Greater_than as c -> c
              | Equal ->
                  match Axis.compare ax1 ax2 with
                  | Less_than | Greater_than as c -> c
                  | Equal as c -> c
        end
    | Proj_and _, _ -> Less_than
    | _, Proj_and _ -> Greater_than
    | And_max_with(ax1, m1), And_max_with(ax2, m2) -> begin
        match Axis.compare ax1 ax2 with
        | Less_than | Greater_than as c -> c
        | Equal -> compare_simple_morph (proj_obj ax1 dst) m1 m2
      end
    | And_max_with _, _ -> Less_than
    | _, And_max_with _ -> Greater_than
    | And_min_with(ax1, m1), And_min_with(ax2, m2) -> begin
        match Axis.compare ax1 ax2 with
        | Less_than | Greater_than as c -> c
        | Equal -> compare_simple_morph (proj_obj ax1 dst) m1 m2
      end
    | And_min_with _, _ -> Less_than
    | _, And_min_with _ -> Greater_than
    | Compose(mb1, ma1), Compose(mb2, ma2) -> begin
        match compare_morph dst mb1 mb2 with
        | Greater_than | Less_than as c -> c
        | Equal -> compare_morph (src dst mb1) ma1 ma2
      end
    | Compose _, _ -> .
    | _, Compose _ -> .

  let print_locality_morph :
      type a b d. Fmt.formatter -> (a, b, d) locality_morph -> unit =
    fun ppf -> function
    | Local_to_regional -> Fmt.fprintf ppf "local_to_regional"
    | Regional_to_local -> Fmt.fprintf ppf "regional_to_local"
    | Locality_as_regionality -> Fmt.fprintf ppf "locality_as_regionality"
    | Regional_to_global -> Fmt.fprintf ppf "regional_to_global"
    | Local_to_regional_regionality ->
        Fmt.fprintf ppf "local_to_regional_regionality"
    | Regional_to_local_regionality ->
        Fmt.fprintf ppf "regional_to_local_regionality"
    | Regional_to_global_regionality ->
        Fmt.fprintf ppf "regional_to_global_regionality"

  let print_core_morph :
      type a b d. Fmt.formatter -> (a, b, d) core_morph -> unit =
    fun ppf -> function
    | Locality_restricted l -> print_locality_morph ppf l
    | Locality_full l ->
        Fmt.fprintf ppf "%a_full" print_locality_morph l
    | Uniqueness_op_to_linearity ->
        Fmt.fprintf ppf "uniqueness_op_to_linearity"
    | Linearity_to_uniqueness_op ->
        Fmt.fprintf ppf "linearity_to_uniqueness_op"
    | Contention_op_to_portability ->
        Fmt.fprintf ppf "contention_op_to_portability"
    | Portability_to_contention_op ->
        Fmt.fprintf ppf "portability_to_contention_op"
    | Visibility_op_to_statefulness ->
        Fmt.fprintf ppf "visibility_op_to_statefulness"
    | Statefulness_to_visibility_op ->
        Fmt.fprintf ppf "statefulnes_to_visibility_op"
    | Monadic_to_comonadic_min ->
        Fmt.fprintf ppf "monadic_to_comonadic_min"
    | Comonadic_to_monadic_min _ ->
        Fmt.fprintf ppf "comonadic_to_monadic_min"
    | Monadic_to_comonadic_max ->
        Fmt.fprintf ppf "monadic_to_comonadic_max"
    | Comonadic_to_monadic_max _ ->
        Fmt.fprintf ppf "comonadic_to_monadic_max"

  let rec print_simple_morph :
      type a b d. b obj -> Fmt.formatter -> (a, b, d) simple_morph -> unit =
   fun dst ppf -> function
    | Id -> Fmt.fprintf ppf "id"
    | Core m -> print_core_morph ppf m
    | Meet_const c -> Fmt.fprintf ppf "meet(%a)" (print dst) c
    | Imply_const c -> Fmt.fprintf ppf "imply(%a)" (print dst) c
    | Core_and_meet_const(c, m) ->
        Fmt.fprintf ppf "meet(%a) . %a"
          (print dst) c print_core_morph m
    | Imply_const_and_core(m, c) ->
        Fmt.fprintf ppf "%a . imply(%a)"
          print_core_morph m (print (src_core m)) c
    | Compose (mb, ma) ->
        let mid = src_simple dst mb in
        Fmt.fprintf ppf "%a . %a"
          (print_simple_morph dst) mb
          (print_simple_morph mid) ma

  let rec print_morph :
      type a b d. b obj -> Fmt.formatter -> (a, b, d) morph -> unit =
   fun dst ppf -> function
    | Simple m -> print_simple_morph dst ppf m
    | Proj_and(Id, ax, src) ->
        Fmt.fprintf ppf "proj_%a" print_obj (proj_obj ax src)
    | Proj_and(m, ax, src) ->
        Fmt.fprintf ppf "%a . proj_%a"
          (print_simple_morph dst) m print_obj (proj_obj ax src)
    | And_max_with(ax, Id) ->
        Fmt.fprintf ppf "max_with_%a" print_obj (proj_obj ax dst)
    | And_max_with(ax, m) ->
        let mid = proj_obj ax dst in
        Fmt.fprintf ppf "max_with_%a . %a"
          print_obj mid (print_simple_morph mid) m
    | And_min_with(ax, Id) ->
        Fmt.fprintf ppf "min_with_%a" print_obj (proj_obj ax dst)
    | And_min_with(ax, m) ->
        let mid = proj_obj ax dst in
        Fmt.fprintf ppf "min_with_%a . %a"
          print_obj mid (print_simple_morph mid) m
    | Const_max _ ->
        Format.fprintf ppf "const_%a" (print dst) (max dst)
    | Const_min _ ->
        Format.fprintf ppf "const_%a" (print dst) (min dst)
    | Const (_, c) ->
        Format.fprintf ppf "const_%a" (print dst) c
    | Compose (mb, ma) ->
        let mid = src dst mb in
        Format.fprintf ppf "%a . %a"
          (print_morph dst) mb
          (print_morph mid) ma

  let id = Simple Id

  let local_to_regional = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Regional

  let regional_to_local = function
    | Regionality.Global -> Locality.Global
    | Regionality.Regional -> Locality.Local
    | Regionality.Local -> Locality.Local

  let locality_as_regionality = function
    | Locality.Global -> Regionality.Global
    | Locality.Local -> Regionality.Local

  let regional_to_global = function
    | Regionality.Global -> Locality.Global
    | Regionality.Regional -> Locality.Global
    | Regionality.Local -> Locality.Local

  let local_to_regional_regionality = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Regional
    | Regionality.Local -> Regionality.Regional

  let regional_to_local_regionality = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Local
    | Regionality.Local -> Regionality.Local

  let regional_to_global_regionality = function
    | Regionality.Global -> Regionality.Global
    | Regionality.Regional -> Regionality.Global
    | Regionality.Local -> Regionality.Local

  let uniqueness_op_to_linearity = function
    | Uniqueness.Unique -> Linearity.Once
    | Uniqueness.Aliased -> Linearity.Many

  let linearity_to_uniqueness_op = function
    | Linearity.Many -> Uniqueness.Aliased
    | Linearity.Once -> Uniqueness.Unique

  let contention_op_to_portability = function
    | Contention.Contended -> Portability.Portable
    | Contention.Shared -> Portability.Shareable
    | Contention.Uncontended -> Portability.Nonportable

  let portability_to_contention_op = function
    | Portability.Portable -> Contention.Contended
    | Portability.Shareable -> Contention.Shared
    | Portability.Nonportable -> Contention.Uncontended

  let visibility_op_to_statefulness = function
    | Visibility.Immutable -> Statefulness.Stateless
    | Visibility.Read -> Statefulness.Observing
    | Visibility.Read_write -> Statefulness.Stateful

  let statefulness_to_visibility_op = function
    | Statefulness.Stateless -> Visibility.Immutable
    | Statefulness.Observing -> Visibility.Read
    | Statefulness.Stateful -> Visibility.Read_write

  let min_with dst ax a = Axis.set ax a (min dst)

  let max_with dst ax a = Axis.set ax a (max dst)

  let monadic_to_comonadic_min : type a.
      a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj m ->
    let areality : a =
      match obj with
      | Comonadic_with_locality -> Locality.min
      | Comonadic_with_regionality -> Regionality.min
    in
    let linearity = uniqueness_op_to_linearity m.uniqueness in
    let portability = contention_op_to_portability m.contention in
    let forkable = Forkable.min in
    let yielding = Yielding.min in
    let statefulness = visibility_op_to_statefulness m.visibility in
    { areality; linearity; portability; forkable; yielding; statefulness }

  let comonadic_to_monadic_min :
      type a. a areality -> a comonadic_with -> Monadic_op.t =
   fun _ m ->
     let uniqueness = linearity_to_uniqueness_op m.linearity in
     let contention = portability_to_contention_op m.portability in
     let visibility = statefulness_to_visibility_op m.statefulness in
     let staticity = Staticity.min in
     { uniqueness; contention; visibility; staticity }

  let monadic_to_comonadic_max : type a.
      a comonadic_with obj -> Monadic_op.t -> a comonadic_with =
   fun obj m ->
    let areality : a =
      match obj with
      | Comonadic_with_locality -> Locality.max
      | Comonadic_with_regionality -> Regionality.max
    in
    let linearity = uniqueness_op_to_linearity m.uniqueness in
    let portability = contention_op_to_portability m.contention in
    let forkable = Forkable.max in
    let yielding = Yielding.max in
    let statefulness = visibility_op_to_statefulness m.visibility in
    { areality; linearity; portability; forkable; yielding; statefulness }

  let comonadic_to_monadic_max : type a.
      a areality -> a comonadic_with -> Monadic_op.t =
   fun _ m ->
    let uniqueness = linearity_to_uniqueness_op m.linearity in
    let contention = portability_to_contention_op m.portability in
    let visibility = statefulness_to_visibility_op m.statefulness in
    let staticity = Staticity_op.max in
    { uniqueness; contention; visibility; staticity }

  let apply_locality : type a b d. (a, b, d) locality_morph -> a -> b =
   fun f a ->
     match f with
     | Local_to_regional -> local_to_regional a
     | Regional_to_local -> regional_to_local a
     | Locality_as_regionality -> locality_as_regionality a
     | Regional_to_global -> regional_to_global a
     | Local_to_regional_regionality -> local_to_regional_regionality a
     | Regional_to_local_regionality -> regional_to_local_regionality a
     | Regional_to_global_regionality -> regional_to_global_regionality a

  let apply_core : type a b d. b obj -> (a, b, d) core_morph -> a -> b =
   fun dst f a ->
     match f with
     | Locality_restricted m -> apply_locality m a
     | Locality_full m ->
         let areality = apply_locality m a.areality in
         { a with areality }
     | Uniqueness_op_to_linearity -> uniqueness_op_to_linearity a
     | Linearity_to_uniqueness_op -> linearity_to_uniqueness_op a
     | Contention_op_to_portability -> contention_op_to_portability a
     | Portability_to_contention_op -> portability_to_contention_op a
     | Visibility_op_to_statefulness -> visibility_op_to_statefulness a
     | Statefulness_to_visibility_op -> statefulness_to_visibility_op a
     | Monadic_to_comonadic_min -> monadic_to_comonadic_min dst a
     | Comonadic_to_monadic_min ar -> comonadic_to_monadic_min ar a
     | Monadic_to_comonadic_max -> monadic_to_comonadic_max dst a
     | Comonadic_to_monadic_max ar -> comonadic_to_monadic_max ar a

  let rec apply_simple : type a b d. b obj -> (a, b, d) simple_morph -> a -> b =
   fun dst f a ->
    match f with
    | Id -> a
    | Core m -> apply_core dst m a
    | Meet_const c -> meet dst c a
    | Imply_const c -> imply dst c a
    | Core_and_meet_const(c, m) ->
        meet dst c (apply_core dst m a)
    | Imply_const_and_core(m, c) ->
        apply_core dst m (imply (src_core m) c a)
    | Compose (mb, ma) ->
      let mid = src_simple dst mb in
      apply_simple dst mb (apply_simple mid ma a)

  let rec apply : type a b d. b obj -> (a, b, d) morph -> a -> b =
   fun dst f a ->
    match f with
    | Simple m -> apply_simple dst m a
    | Proj_and(m, ax, _) -> apply_simple dst m (Axis.proj ax a)
    | And_max_with(ax, m) ->
        let mid = proj_obj ax dst in
        max_with dst ax (apply_simple mid m a)
    | And_min_with(ax, m) ->
        let mid = proj_obj ax dst in
        min_with dst ax (apply_simple mid m a)
    | Const_max _ -> max dst
    | Const_min _ -> min dst
    | Const (_, c) -> c
    | Compose (mb, ma) ->
      let mid = src dst mb in
      apply dst mb (apply mid ma a)

  let right_adjoint_locality :
     type a b r.
       (a, b, allowed * r) locality_morph
       -> (b, a, disallowed * allowed) locality_morph =
    function
     | Local_to_regional -> Regional_to_local
     | Regional_to_local -> Locality_as_regionality
     | Locality_as_regionality -> Regional_to_global
     | Local_to_regional_regionality -> Regional_to_local_regionality
     | Regional_to_local_regionality -> Regional_to_global_regionality

  let right_adjoint_core :
     type a b r.
       b obj
       -> (a, b, allowed * r) core_morph
       -> (b, a, disallowed * allowed) core_morph =
   fun dst -> function
     | Locality_restricted m -> Locality_restricted (right_adjoint_locality m)
     | Locality_full m -> Locality_full (right_adjoint_locality m)
     | Uniqueness_op_to_linearity -> Linearity_to_uniqueness_op
     | Linearity_to_uniqueness_op -> Uniqueness_op_to_linearity
     | Contention_op_to_portability -> Portability_to_contention_op
     | Portability_to_contention_op -> Contention_op_to_portability
     | Visibility_op_to_statefulness -> Statefulness_to_visibility_op
     | Statefulness_to_visibility_op -> Visibility_op_to_statefulness
     | Monadic_to_comonadic_min ->
         Comonadic_to_monadic_max (comonadic_obj_areality dst)
     | Comonadic_to_monadic_min _ -> Monadic_to_comonadic_max

  let right_adjoint_simple :
      type a b r.
        b obj
        -> (a, b, allowed * r) simple_morph
        -> (b, a, disallowed * allowed) simple_morph =
    fun dst f ->
    match f with
    | Id -> Id
    | Core m -> Core (right_adjoint_core dst m)
    | Meet_const c -> Imply_const c
    | Core_and_meet_const(c, m) -> Imply_const_and_core(right_adjoint_core dst m, c)

  let right_adjoint :
      type a b r.
      b obj -> (a, b, allowed * r) morph -> (b, a, disallowed * allowed) morph =
    fun dst f ->
    match f with
    | Simple m -> Simple (right_adjoint_simple dst m)
    | Proj_and(m, ax, _) -> And_max_with(ax, right_adjoint_simple dst m)
    | And_min_with(ax, m) ->
        let mid = proj_obj ax dst in
        Proj_and(right_adjoint_simple mid m, ax, dst)
    | Const_min _ -> Const_max dst

  let left_adjoint_locality :
      type a b l.
        (a, b, l * allowed) locality_morph
        -> (b, a, allowed * disallowed) locality_morph =
    function
    | Regional_to_local -> Local_to_regional
    | Locality_as_regionality -> Regional_to_local
    | Regional_to_global -> Locality_as_regionality
    | Regional_to_local_regionality -> Local_to_regional_regionality
    | Regional_to_global_regionality -> Regional_to_local_regionality

  let left_adjoint_core :
      type a b l.
        b obj
        -> (a, b, l * allowed) core_morph
        -> (b, a, allowed * disallowed) core_morph =
    fun dst -> function
    | Locality_restricted m -> Locality_restricted (left_adjoint_locality m)
    | Locality_full m -> Locality_full (left_adjoint_locality m)
    | Uniqueness_op_to_linearity -> Linearity_to_uniqueness_op
    | Linearity_to_uniqueness_op -> Uniqueness_op_to_linearity
    | Contention_op_to_portability -> Portability_to_contention_op
    | Portability_to_contention_op -> Contention_op_to_portability
    | Visibility_op_to_statefulness -> Statefulness_to_visibility_op
    | Statefulness_to_visibility_op -> Visibility_op_to_statefulness
    | Monadic_to_comonadic_max ->
        Comonadic_to_monadic_min (comonadic_obj_areality dst)
    | Comonadic_to_monadic_max _ -> Monadic_to_comonadic_min

  let left_adjoint_simple :
      type a b l.
        b obj
        -> (a, b, l * allowed) simple_morph
        -> (b, a, allowed * disallowed) simple_morph =
   fun dst f ->
    match f with
    | Id -> Id
    | Core m -> Core (left_adjoint_core dst m)
    | Imply_const c -> Meet_const c
    | Imply_const_and_core(m, c) -> Core_and_meet_const(c, left_adjoint_core dst m)

  let left_adjoint :
      type a b l.
      b obj -> (a, b, l * allowed) morph -> (b, a, allowed * disallowed) morph =
   fun dst f ->
    match f with
    | Simple m -> Simple (left_adjoint_simple dst m)
    | Proj_and(m, ax, _) -> And_min_with(ax, left_adjoint_simple dst m)
    | And_max_with(ax, m) ->
        let mid = proj_obj ax dst in
        Proj_and(left_adjoint_simple mid m, ax, dst)
    | Const_max(_) -> Const_min(dst)

  type ('a, 'b, 'd) maybe_allowed_right_locality =
    | Allowed_right :
        ('a, 'b, 'l * allowed) locality_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_right_locality
    | Not_allowed_right : ('a, 'b, 'l * disallowed) maybe_allowed_right_locality

  let maybe_allowed_right_locality :
      type a b d.
        (a, b, d) locality_morph -> (a, b, d) maybe_allowed_right_locality =
    function
    | Regional_to_local as m -> Allowed_right m
    | Locality_as_regionality as m -> Allowed_right m
    | Regional_to_global as m -> Allowed_right m
    | Regional_to_local_regionality as m -> Allowed_right m
    | Regional_to_global_regionality as m -> Allowed_right m
    | Local_to_regional -> Not_allowed_right
    | Local_to_regional_regionality -> Not_allowed_right

  type ('a, 'b, 'd) maybe_allowed_left_locality =
    | Allowed_left :
        ('a, 'b, allowed * 'r) locality_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_left_locality
    | Not_allowed_left : ('a, 'b, disallowed * 'r) maybe_allowed_left_locality

  let maybe_allowed_left_locality :
      type a b d.
        (a, b, d) locality_morph -> (a, b, d) maybe_allowed_left_locality =
    function
    | Local_to_regional as m -> Allowed_left m
    | Regional_to_local as m -> Allowed_left m
    | Locality_as_regionality as m -> Allowed_left m
    | Local_to_regional_regionality as m -> Allowed_left m
    | Regional_to_local_regionality as m -> Allowed_left m
    | Regional_to_global -> Not_allowed_left
    | Regional_to_global_regionality -> Not_allowed_left

  type ('a, 'b, 'd) locality_composition =
    | Id : ('a, 'a, 'd) locality_composition
    | Morph : ('a, 'b, 'd) locality_morph -> ('a, 'b, 'd) locality_composition
    | Disallowed : ('a, 'b, disallowed * disallowed) locality_composition

  let compose_locality :
      type a b c d.
        (b, c, d) locality_morph
        -> (a, b, d) locality_morph
        -> (a, c, d) locality_composition =
    fun m1 m2 ->
      match m1, m2 with
      | Local_to_regional, Regional_to_local ->
          Morph Local_to_regional_regionality
      | Regional_to_local, Local_to_regional -> Id
      | Regional_to_local, Locality_as_regionality -> Id
      | Regional_to_local, Local_to_regional_regionality ->
          Morph Regional_to_local
      | Regional_to_local, Regional_to_local_regionality ->
          Morph Regional_to_local
      | Regional_to_local, Regional_to_global_regionality ->
          Morph Regional_to_global
      | Locality_as_regionality, Regional_to_local ->
          Morph Regional_to_local_regionality
      | Locality_as_regionality, Regional_to_global ->
          Morph Regional_to_global_regionality
      | Regional_to_global, Locality_as_regionality -> Id
      | Regional_to_global, Regional_to_local_regionality ->
          Morph Regional_to_local
      | Regional_to_global, Regional_to_global_regionality ->
          Morph Regional_to_global
      | Local_to_regional_regionality, Local_to_regional ->
          Morph Local_to_regional
      | Local_to_regional_regionality, Locality_as_regionality ->
          Morph Local_to_regional
      | Local_to_regional_regionality, Local_to_regional_regionality ->
          Morph Local_to_regional_regionality
      | Local_to_regional_regionality, Regional_to_local_regionality ->
          Morph Local_to_regional_regionality
      | Regional_to_local_regionality, Local_to_regional ->
          Morph Locality_as_regionality
      | Regional_to_local_regionality, Locality_as_regionality ->
          Morph Locality_as_regionality
      | Regional_to_local_regionality, Local_to_regional_regionality ->
          Morph Regional_to_local_regionality
      | Regional_to_local_regionality, Regional_to_local_regionality ->
          Morph Regional_to_local_regionality
      | Regional_to_local_regionality, Regional_to_global_regionality ->
          Morph Regional_to_global_regionality
      | Regional_to_global_regionality, Locality_as_regionality ->
          Morph Locality_as_regionality
      | Regional_to_global_regionality, Regional_to_local_regionality ->
          Morph Regional_to_local_regionality
      | Regional_to_global_regionality, Regional_to_global_regionality ->
          Morph Regional_to_global_regionality
      (* Operations that cannot appear on the same side *)
      | Local_to_regional, Regional_to_global -> Disallowed
      | Regional_to_global, Local_to_regional -> Disallowed
      | Regional_to_global, Local_to_regional_regionality -> Disallowed
      | Local_to_regional_regionality, Regional_to_global_regionality ->
          Disallowed
      | Regional_to_global_regionality, Local_to_regional -> Disallowed
      | Regional_to_global_regionality, Local_to_regional_regionality ->
          Disallowed

  let compose_core :
      type a b c d.
        c obj
        -> (b, c, d) core_morph
        -> (a, b, d) core_morph
        -> (a, c, d) simple_morph =
    fun dst m1 m2 ->
      match m1, m2 with
      | Locality_restricted lm1, Locality_restricted lm2 -> begin
          match compose_locality lm1 lm2 with
          | Id -> Id
          | Morph lm -> Core (Locality_restricted lm)
          | Disallowed -> Compose(Core m1, Core m2)
        end
      | Locality_full lm1, Locality_full lm2 -> begin
          match compose_locality lm1 lm2 with
          | Id -> Id
          | Morph lm -> Core (Locality_full lm)
          | Disallowed -> Compose(Core m1, Core m2)
        end
      | Uniqueness_op_to_linearity, Linearity_to_uniqueness_op -> Id
      | Linearity_to_uniqueness_op, Uniqueness_op_to_linearity -> Id
      | Contention_op_to_portability, Portability_to_contention_op -> Id
      | Portability_to_contention_op, Contention_op_to_portability -> Id
      | Visibility_op_to_statefulness, Statefulness_to_visibility_op -> Id
      | Statefulness_to_visibility_op, Visibility_op_to_statefulness -> Id
      | Comonadic_to_monadic_min areality, Monadic_to_comonadic_min ->
          let c =
            match areality with
            | Locality ->
                apply_core dst
                  (Comonadic_to_monadic_min Locality)
                  Comonadic_with_locality.max
            | Regionality ->
                apply_core dst
                  (Comonadic_to_monadic_min Regionality)
                  Comonadic_with_regionality.max
          in
          Meet_const c
      | Monadic_to_comonadic_min, Comonadic_to_monadic_min areality -> begin
          let c = apply_core dst Monadic_to_comonadic_min Monadic_op.max in
          match areality, dst with
          | Locality, Comonadic_with_locality -> Meet_const c
          | Regionality, Comonadic_with_locality ->
              Core_and_meet_const(c, Locality_full Regional_to_local)
          | Locality, Comonadic_with_regionality ->
              Core_and_meet_const(c, Locality_full Locality_as_regionality)
          | Regionality, Comonadic_with_regionality ->
              Meet_const c
        end
      | Monadic_to_comonadic_max, Comonadic_to_monadic_max areality -> begin
          let src = areality_comonadic_obj areality in
          let c = apply_core src Monadic_to_comonadic_min Monadic_op.max in
          match areality, dst with
          | Locality, Comonadic_with_locality -> Imply_const c
          | Regionality, Comonadic_with_locality ->
              Imply_const_and_core(Locality_full Regional_to_local, c)
          | Locality, Comonadic_with_regionality ->
              Imply_const_and_core(Locality_full Locality_as_regionality, c)
          | Regionality, Comonadic_with_regionality -> Imply_const c
        end
      | Comonadic_to_monadic_max areality, Monadic_to_comonadic_max ->
          let c =
            match areality with
            | Locality ->
                apply_core dst
                  (Comonadic_to_monadic_min Locality)
                  Comonadic_with_locality.max
            | Regionality ->
                apply_core dst
                  (Comonadic_to_monadic_min Regionality)
                  Comonadic_with_regionality.max
          in
          Imply_const c
      | Comonadic_to_monadic_min _, Monadic_to_comonadic_max ->
          Compose(Core m1, Core m2)
      | Monadic_to_comonadic_max, Comonadic_to_monadic_min _ ->
          Compose(Core m1, Core m2)
      | Comonadic_to_monadic_max _, Monadic_to_comonadic_min ->
          Compose(Core m1, Core m2)
      | Monadic_to_comonadic_min, Comonadic_to_monadic_max _ ->
          Compose(Core m1, Core m2)
      | Comonadic_to_monadic_min _, Locality_full m2 ->
          let src = src_locality_full m2 in
          Core (Comonadic_to_monadic_min (comonadic_obj_areality src))
      | Comonadic_to_monadic_max _, Locality_full m2 ->
          let src = src_locality_full m2 in
          Core (Comonadic_to_monadic_max (comonadic_obj_areality src))
      | Locality_full lm1, Monadic_to_comonadic_min -> begin
          match maybe_allowed_left_locality lm1 with
          | Allowed_left _ ->
              (* Has a right adjoint so it preserves min *)
              Core Monadic_to_comonadic_min
          | Not_allowed_left ->
              Compose(Core m1, Core Monadic_to_comonadic_min)
          end
      | Locality_full lm1, Monadic_to_comonadic_max -> begin
          match maybe_allowed_right_locality lm1 with
          | Allowed_right _ ->
              (* Has a left adjoint so it preserves max *)
              Core Monadic_to_comonadic_max
          | Not_allowed_right ->
              Compose(Core m1, Core Monadic_to_comonadic_max)
          end
      | Locality_restricted _, _ -> .
      | _, Locality_restricted _ -> .
      | Locality_full _, _ -> .
      | _, Locality_full _ -> .

  type ('a, 'b, 'd) maybe_allowed_right_core =
    | Allowed_right :
        ('a, 'b, 'l * allowed) core_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_right_core
    | Not_allowed_right : ('a, 'b, 'l * disallowed) maybe_allowed_right_core

  let maybe_allowed_right_core :
      type a b d.
        (a, b, d) core_morph -> (a, b, d) maybe_allowed_right_core =
    function
    | Locality_restricted lm -> begin
        match maybe_allowed_right_locality lm with
        | Allowed_right lm -> Allowed_right (Locality_restricted lm)
        | Not_allowed_right -> Not_allowed_right
      end
    | Locality_full lm -> begin
        match maybe_allowed_right_locality lm with
        | Allowed_right lm -> Allowed_right (Locality_full lm)
        | Not_allowed_right -> Not_allowed_right
      end
    | Uniqueness_op_to_linearity as m -> Allowed_right m
    | Linearity_to_uniqueness_op as m -> Allowed_right m
    | Contention_op_to_portability as m -> Allowed_right m
    | Portability_to_contention_op as m -> Allowed_right m
    | Visibility_op_to_statefulness as m -> Allowed_right m
    | Statefulness_to_visibility_op as m -> Allowed_right m
    | Monadic_to_comonadic_min -> Not_allowed_right
    | Comonadic_to_monadic_min _ -> Not_allowed_right
    | Monadic_to_comonadic_max as m -> Allowed_right m
    | Comonadic_to_monadic_max a -> Allowed_right (Comonadic_to_monadic_max a)

  type ('a, 'b, 'd) maybe_allowed_left_core =
    | Allowed_left :
        ('a, 'b, allowed * 'r) core_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_left_core
    | Not_allowed_left : ('a, 'b, disallowed * 'r) maybe_allowed_left_core

  let maybe_allowed_left_core :
      type a b d.
        (a, b, d) core_morph -> (a, b, d) maybe_allowed_left_core =
    function
    | Locality_restricted lm -> begin
        match maybe_allowed_left_locality lm with
        | Allowed_left lm -> Allowed_left (Locality_restricted lm)
        | Not_allowed_left -> Not_allowed_left
      end
    | Locality_full lm -> begin
        match maybe_allowed_left_locality lm with
        | Allowed_left lm -> Allowed_left (Locality_full lm)
        | Not_allowed_left -> Not_allowed_left
      end
    | Uniqueness_op_to_linearity as m -> Allowed_left m
    | Linearity_to_uniqueness_op as m -> Allowed_left m
    | Contention_op_to_portability as m -> Allowed_left m
    | Portability_to_contention_op as m -> Allowed_left m
    | Visibility_op_to_statefulness as m -> Allowed_left m
    | Statefulness_to_visibility_op as m -> Allowed_left m
    | Monadic_to_comonadic_min as m -> Allowed_left m
    | Comonadic_to_monadic_min a -> Allowed_left (Comonadic_to_monadic_min a)
    | Monadic_to_comonadic_max -> Not_allowed_left
    | Comonadic_to_monadic_max _ -> Not_allowed_left

  type ('a, 'b, 'd) maybe_allowed_right_simple =
    | Allowed_right :
        ('a, 'b, 'l * allowed) simple_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_right_simple
    | Not_allowed_right : ('a, 'b, 'l * disallowed) maybe_allowed_right_simple

  let maybe_allowed_right_simple :
      type a b l r.
        (a, b, l * r) simple_morph -> (a, b, l * r) maybe_allowed_right_simple =
    function
    | Id -> Allowed_right Id
    | Core m -> begin
        match maybe_allowed_right_core m with
        | Allowed_right m -> Allowed_right (Core m)
        | Not_allowed_right -> Not_allowed_right
      end
    | Meet_const _ -> Not_allowed_right
    | Imply_const c -> Allowed_right (Imply_const c)
    | Core_and_meet_const _ -> Not_allowed_right
    | Imply_const_and_core (m, c) -> begin
        match maybe_allowed_right_core m with
        | Allowed_right m -> Allowed_right (Imply_const_and_core (m, c))
        | Not_allowed_right -> Not_allowed_right
      end
    | Compose _ -> Not_allowed_right

  type ('a, 'b, 'd) maybe_allowed_left_simple =
    | Allowed_left :
        ('a, 'b, allowed * 'r) simple_morph
        -> ('a, 'b, 'l * 'r) maybe_allowed_left_simple
    | Not_allowed_left : ('a, 'b, disallowed * 'r) maybe_allowed_left_simple

  let maybe_allowed_left_simple :
      type a b l r.
        (a, b, l * r) simple_morph -> (a, b, l * r) maybe_allowed_left_simple =
    function
    | Id -> Allowed_left Id
    | Core m -> begin
        match maybe_allowed_left_core m with
        | Allowed_left m -> Allowed_left (Core m)
        | Not_allowed_left -> Not_allowed_left
      end
    | Meet_const c -> Allowed_left (Meet_const c)
    | Imply_const _ -> Not_allowed_left
    | Core_and_meet_const (c, m) -> begin
        match maybe_allowed_left_core m with
        | Allowed_left m -> Allowed_left (Core_and_meet_const (c, m))
        | Not_allowed_left -> Not_allowed_left
      end
    | Imply_const_and_core _ -> Not_allowed_left
    | Compose _ -> Not_allowed_left

  let compose_meet_const_left :
      type a b l.
      b obj -> b
      -> (a, b, l * disallowed) simple_morph
      -> (a, b, l * disallowed) simple_morph =
    fun dst c m ->
    match m with
    | Id -> Meet_const c
    | Core m -> Core_and_meet_const(c, m)
    | Meet_const c' -> Meet_const (meet dst c c')
    | Core_and_meet_const(c', m) ->
        Core_and_meet_const(meet dst c c', m)
    | Imply_const _ as m -> Compose(Meet_const c, m)
    | Imply_const_and_core _ as m -> Compose(Meet_const c, m)
    | Compose _ as m -> Compose(Meet_const c, m)

  let compose_imply_const_right :
      type a b r. b obj
        -> (a, b, disallowed * r) simple_morph
        -> a -> (a, b, disallowed * r) simple_morph =
    fun dst m c ->
    match m with
    | Id -> Imply_const c
    | Core m -> Imply_const_and_core(m, c)
    | Imply_const c' -> Imply_const (meet dst c c')
    | Imply_const_and_core(m, c') ->
        Imply_const_and_core(m, meet (src_core m) c' c)
    | Meet_const _ as m -> Compose(m, Imply_const c)
    | Core_and_meet_const _ as m -> Compose(m, Imply_const c)
    | Compose _ as m -> Compose(m, Imply_const c)

  (* Commutes a meet through a morphism from the right, such that:
     [apply dst m (meet_const c x)] is equivalent to
     [meet_const (commute_meet_const_from_right dst m c) (apply dst m x)] *)
  let commute_meet_const_from_right :
      type a b d.
      b obj -> (a, b, d) core_morph -> a -> b =
    fun dst m c ->
    (* All our morphisms preserve binary meets *)
    apply_core dst m c

  (* Commutes an implication through a morphism from the left, such that:
     [apply dst m (meet_const c x)] is equivalent to
     [meet_const (commute_imply_from_left dst m c) (apply dst m x)] *)
  let commute_imply_from_left :
      type a b l.
      b obj -> b -> (a, b, l * allowed) core_morph -> a =
    fun dst c m ->
    (* As all our morphisms preserve binary meets, implications can be
       pushed inside of [m] by applying the left adjoint of [m] to the
       implicant. *)
    apply_core (src_core m) (left_adjoint_core dst m) c

  let compose_meet_const_right :
      type a b l.
      b obj -> (a, b, l * disallowed) simple_morph -> a
      -> (a, b, l * disallowed) simple_morph =
    fun dst m c ->
    match m with
    | Id -> Meet_const c
    | Core m ->
        let c = commute_meet_const_from_right dst m c in
        Core_and_meet_const(c, m)
    | Meet_const c' -> Meet_const (meet dst c' c)
    | Core_and_meet_const(c', m) ->
        let c = commute_meet_const_from_right dst m c in
        Core_and_meet_const(meet dst c' c, m)
    | Imply_const _ as m -> Compose(m, Meet_const c)
    | Imply_const_and_core _ as m -> Compose(m, Meet_const c)
    | Compose _ as m -> Compose(m, Meet_const c)

  let compose_imply_const_left :
      type a b r. b obj
        -> b -> (a, b, disallowed * r) simple_morph
        -> (a, b, disallowed * r) simple_morph =
    fun dst c m ->
    match m with
    | Id -> Imply_const c
    | Core m -> begin
        match maybe_allowed_right_core m with
        | Not_allowed_right -> Compose(Imply_const c, Core m)
        | Allowed_right m' ->
            let c = commute_imply_from_left dst c m' in
            Imply_const_and_core(m, c)
      end
    | Imply_const c' -> Imply_const (meet dst c c')
    | Imply_const_and_core(m, c') -> begin
        match maybe_allowed_right_core m with
        | Not_allowed_right -> Compose(Imply_const c, Imply_const_and_core(m, c'))
        | Allowed_right m' ->
            let c = commute_imply_from_left dst c m' in
            Imply_const_and_core(m, meet (src_core m) c c')
      end
    | Meet_const _ as m -> Compose(Imply_const c, m)
    | Core_and_meet_const _ as m -> Compose(Imply_const c, m)
    | Compose _ as m -> Compose(Imply_const c, m)

  let compose_core_right :
      type a b c d. c obj
        -> (b, c, d) simple_morph
        -> (a, b, d) core_morph
        -> (a, c, d) simple_morph =
    fun dst m1 m2 ->
    match m1 with
    | Id -> Core m2
    | Core m1 -> compose_core dst m1 m2
    | Imply_const c1 -> begin
        match maybe_allowed_right_core m2 with
        | Not_allowed_right -> Compose(Imply_const c1, Core m2)
        | Allowed_right m2' ->
            let c1 = commute_imply_from_left dst c1 m2' in
            Imply_const_and_core(m2, c1)
      end
    | Imply_const_and_core(m1, c1) -> begin
        match maybe_allowed_right_core m2 with
        | Not_allowed_right -> Compose(Imply_const_and_core(m1, c1), Core m2)
        | Allowed_right m2' ->
            let mid = src_core m1 in
            let c1 = commute_imply_from_left mid c1 m2' in
            let m = compose_core dst m1 m2 in
            compose_imply_const_right dst m c1
      end
    | Meet_const c1 -> Core_and_meet_const(c1, m2)
    | Core_and_meet_const(c1, m1) ->
        compose_meet_const_left dst c1 (compose_core dst m1 m2)
    | Compose _ as m1 -> Compose(m1, Core m2)

  let compose_core_left :
      type a b c d. c obj
        -> (b, c, d) core_morph
        -> (a, b, d) simple_morph
        -> (a, c, d) simple_morph =
    fun dst m1 m2 ->
    match m2 with
    | Id -> Core m1
    | Core m2 -> compose_core dst m1 m2
    | Imply_const c2 -> Imply_const_and_core(m1, c2)
    | Imply_const_and_core(m2, c2) ->
        compose_imply_const_right dst (compose_core dst m1 m2) c2
    | Meet_const c2 ->
        let c2 = commute_meet_const_from_right dst m1 c2 in
        Core_and_meet_const(c2, m1)
    | Core_and_meet_const(c2, m2) ->
        let c2 = commute_meet_const_from_right dst m1 c2 in
        let m = compose_core dst m1 m2 in
        compose_meet_const_left dst c2 m
    | Compose _ as m2 -> Compose(Core m1, m2)

  let compose_simple :
      type a b c d.
           c obj
           -> (b, c, d) simple_morph
           -> (a, b, d) simple_morph
           -> (a, c, d) simple_morph =
    fun dst m1 m2 ->
      match m1, m2 with
      | m1, Id -> m1
      | Id, m2 -> m2
      | m1, Meet_const c2 -> compose_meet_const_right dst m1 c2
      | Meet_const c1, m2 -> compose_meet_const_left dst c1 m2
      | m1, Imply_const c2 -> compose_imply_const_right dst m1 c2
      | Imply_const c1, m2 -> compose_imply_const_left dst c1 m2
      | m1, Core m2 -> compose_core_right dst m1 m2
      | Core m1, m2 -> compose_core_left dst m1 m2
      | m1, Core_and_meet_const(c2, m2) ->
          compose_core_right dst
            (compose_meet_const_right dst m1 c2) m2
      | Core_and_meet_const(c1, m1), m2 ->
          compose_meet_const_left dst c1
            (compose_core_left dst m1 m2)
      | m1, Imply_const_and_core(m2, c2) ->
          compose_imply_const_right dst
            (compose_core_right dst m1 m2) c2
      | Imply_const_and_core(m1, c1), m2 ->
          let mid = src_core m1 in
          compose_core_left dst m1
            (compose_imply_const_left mid c1 m2)
      | (Compose _ as m1), m2 -> Compose(m1, m2)
      | _, Compose _ -> .


  type ('a, 'b, 'd) compose_proj_result =
    | Proj_core :
        ('p, 'b, 'd) core_morph * ('a, 'p) Axis.t * 'a obj
        -> ('a, 'b, 'd) compose_proj_result
    | Proj_id :
        ('a comonadic_with, 'p) Axis.t * ('a comonadic_with) obj
        -> ('a comonadic_with, 'p, 'd) compose_proj_result
    | Proj_const_max :
        'a obj -> ('a, 'b, disallowed * 'r) compose_proj_result
    | Proj_const_min :
        'a obj -> ('a, 'b, 'l * disallowed) compose_proj_result

  let compose_projection_locality_morph :
      type a b p l r.
      (a, b, l * r) locality_morph -> (b comonadic_with, p) Axis.t -> (a comonadic_with, p, l * r) compose_proj_result =
    fun lm1 ax0 ->
      match ax0 with
      | Forkable -> Proj_id (Forkable,src_locality_full lm1)
      | Yielding -> Proj_id (Yielding,src_locality_full lm1)
      | Linearity -> Proj_id (Linearity,src_locality_full lm1)
      | Statefulness -> Proj_id (Statefulness,src_locality_full lm1)
      | Portability -> Proj_id (Portability,src_locality_full lm1)
      | Areality -> Proj_core (Locality_restricted lm1,Areality,src_locality_full lm1)

  let compose_projection_core :
      type a b p d.
      (b, p) Axis.t -> (a, b, d) core_morph -> (a, p, d) compose_proj_result =
    fun ax0 m1 ->
      match (m1 : (a, b, d) core_morph), (ax0 : (b, p) Axis.t) with
      | Monadic_to_comonadic_min, Areality -> Proj_const_min Monadic_op
      | Monadic_to_comonadic_min, Forkable -> Proj_const_min Monadic_op
      | Monadic_to_comonadic_min, Yielding -> Proj_const_min Monadic_op
      | Monadic_to_comonadic_min, Linearity ->
          Proj_core (Uniqueness_op_to_linearity,Uniqueness,Monadic_op)
      | Monadic_to_comonadic_min, Statefulness ->
          Proj_core (Visibility_op_to_statefulness,Visibility,Monadic_op)
      | Monadic_to_comonadic_min, Portability ->
          Proj_core (Contention_op_to_portability,Contention,Monadic_op)
      | Comonadic_to_monadic_min areality, Uniqueness ->
          Proj_core (Linearity_to_uniqueness_op,Linearity,areality_comonadic_obj areality)
      | Comonadic_to_monadic_min areality, Visibility ->
          Proj_core (Statefulness_to_visibility_op,Statefulness,areality_comonadic_obj areality)
      | Comonadic_to_monadic_min areality, Contention ->
          Proj_core (Portability_to_contention_op,Portability,areality_comonadic_obj areality)
      | Comonadic_to_monadic_min areality, Staticity ->
          Proj_const_min (areality_comonadic_obj areality)
      | Monadic_to_comonadic_max, Areality -> Proj_const_max Monadic_op
      | Monadic_to_comonadic_max, Forkable -> Proj_const_max Monadic_op
      | Monadic_to_comonadic_max, Yielding -> Proj_const_max Monadic_op
      | Monadic_to_comonadic_max, Linearity ->
          Proj_core (Uniqueness_op_to_linearity,Uniqueness,Monadic_op)
      | Monadic_to_comonadic_max, Statefulness ->
          Proj_core (Visibility_op_to_statefulness,Visibility,Monadic_op)
      | Monadic_to_comonadic_max, Portability ->
          Proj_core (Contention_op_to_portability,Contention,Monadic_op)
      | Comonadic_to_monadic_max areality, Uniqueness ->
          Proj_core (Linearity_to_uniqueness_op,Linearity,areality_comonadic_obj areality)
      | Comonadic_to_monadic_max areality, Visibility ->
          Proj_core (Statefulness_to_visibility_op,Statefulness,areality_comonadic_obj areality)
      | Comonadic_to_monadic_max areality, Contention ->
          Proj_core (Portability_to_contention_op,Portability,areality_comonadic_obj areality)
      | Comonadic_to_monadic_max areality, Staticity ->
          Proj_const_max (areality_comonadic_obj areality)
      | Locality_full lm, (_ as ax0) -> compose_projection_locality_morph lm ax0
      | _, _ -> .

  type ('a, 'b, 'd) compose_and_max_result =
    | And_max_core :
        ('s, 'q) Axis.t * ('p, 'q, disallowed * 'r) core_morph
        -> ('p, 's, disallowed * 'r) compose_and_max_result
    | Const_max_core :
        ('a, 'b, disallowed * 'r) compose_and_max_result
    | And_max_id :
        ('c comonadic_with, 'q) Axis.t
        -> ('q, 'c comonadic_with, 'd) compose_and_max_result
    | Disallowed : ('a, 'b, neither) compose_and_max_result

  let compose_and_max_with_locality_morph :
      type b c q r.
      (b, c, disallowed * r) locality_morph
      -> (b comonadic_with, q) Axis.t
      -> (q, c comonadic_with, disallowed * r) compose_and_max_result =
    fun lm1 ax0 ->
      match ax0 with
      | Forkable -> And_max_id Forkable
      | Yielding -> And_max_id Yielding
      | Linearity -> And_max_id Linearity
      | Statefulness -> And_max_id Statefulness
      | Portability -> And_max_id Portability
      | Areality -> And_max_core (Areality,Locality_restricted lm1)

  let compose_and_max_with_core :
      type b c q r.
      (b, q) Axis.t -> (b, c, disallowed * r) core_morph -> (q, c, disallowed * r) compose_and_max_result =
    fun ax1 m0 ->
      match (m0 : (b, c, disallowed * r) core_morph), (ax1 : (b, q) Axis.t) with
      | Comonadic_to_monadic_max _, Portability ->
          And_max_core (Contention,Portability_to_contention_op)
      | Comonadic_to_monadic_max _, Statefulness ->
          And_max_core (Visibility,Statefulness_to_visibility_op)
      | Comonadic_to_monadic_max _, Linearity ->
          And_max_core (Uniqueness,Linearity_to_uniqueness_op)
      | Comonadic_to_monadic_max _, Yielding -> Const_max_core
      | Comonadic_to_monadic_max _, Forkable -> Const_max_core
      | Comonadic_to_monadic_max _, Areality -> Const_max_core
      | Monadic_to_comonadic_max, Staticity -> Const_max_core
      | Monadic_to_comonadic_max, Contention ->
          And_max_core (Portability,Contention_op_to_portability)
      | Monadic_to_comonadic_max, Visibility ->
          And_max_core (Statefulness,Visibility_op_to_statefulness)
      | Monadic_to_comonadic_max, Uniqueness ->
          And_max_core (Linearity,Uniqueness_op_to_linearity)
      | Locality_full lm, (_ as ax0) -> compose_and_max_with_locality_morph lm ax0
      | Monadic_to_comonadic_min, _ -> Disallowed
      | Comonadic_to_monadic_min _, _ -> Disallowed
      | _, _ -> .

  type ('a, 'b, 'd) compose_and_min_result =
    | And_min_core :
        ('s, 'q) Axis.t * ('p, 'q, 'l * disallowed) core_morph
        -> ('p, 's, 'l * disallowed) compose_and_min_result
    | Const_min_core :
        ('a, 'b, 'l * disallowed) compose_and_min_result
    | And_min_id :
        ('c comonadic_with, 'q) Axis.t
        -> ('q, 'c comonadic_with, 'd) compose_and_min_result
    | Disallowed : ('a, 'b, neither) compose_and_min_result

  let compose_and_min_with_locality_morph :
      type b c q l.
      (b, c, l * disallowed) locality_morph
      -> (b comonadic_with, q) Axis.t
      -> (q, c comonadic_with, l * disallowed) compose_and_min_result =
    fun lm1 ax0 ->
      match ax0 with
      | Forkable -> And_min_id Forkable
      | Yielding -> And_min_id Yielding
      | Linearity -> And_min_id Linearity
      | Statefulness -> And_min_id Statefulness
      | Portability -> And_min_id Portability
      | Areality -> And_min_core (Areality,Locality_restricted lm1)

  let compose_and_min_with_core :
      type b c q l.
      (b, q) Axis.t -> (b, c, l * disallowed) core_morph -> (q, c, l * disallowed) compose_and_min_result =
    fun ax1 m0 ->
      match (m0 : (b, c, l * disallowed) core_morph), (ax1 : (b, q) Axis.t) with
      | Comonadic_to_monadic_min _, Portability ->
          And_min_core (Contention,Portability_to_contention_op)
      | Comonadic_to_monadic_min _, Statefulness ->
          And_min_core (Visibility,Statefulness_to_visibility_op)
      | Comonadic_to_monadic_min _, Linearity ->
          And_min_core (Uniqueness,Linearity_to_uniqueness_op)
      | Comonadic_to_monadic_min _, Yielding -> Const_min_core
      | Comonadic_to_monadic_min _, Forkable -> Const_min_core
      | Comonadic_to_monadic_min _, Areality -> Const_min_core
      | Monadic_to_comonadic_min, Staticity -> Const_min_core
      | Monadic_to_comonadic_min, Contention ->
          And_min_core (Portability,Contention_op_to_portability)
      | Monadic_to_comonadic_min, Visibility ->
          And_min_core (Statefulness,Visibility_op_to_statefulness)
      | Monadic_to_comonadic_min, Uniqueness ->
          And_min_core (Linearity,Uniqueness_op_to_linearity)
      | Locality_full lm, (_ as ax0) -> compose_and_min_with_locality_morph lm ax0
      | Monadic_to_comonadic_max, _ -> Disallowed
      | Comonadic_to_monadic_max _, _ -> Disallowed
      | _, _ -> .

  let compose_simple_with_proj_core :
      type a c p d.
      c obj -> (p, c, d) simple_morph -> (a, p, d) compose_proj_result -> (a, c, d) morph =
    fun dst m0 pm1 ->
      match pm1 with
        | Proj_core (m1, ax1, obj1) ->
          Proj_and (compose_simple dst m0 (Core m1), ax1, obj1)
        | Proj_id (ax1, obj1) -> Proj_and (m0, ax1, obj1)
        | Proj_const_max obj1 -> Const_max obj1
        | Proj_const_min obj1 -> Const_min obj1

  let compose_simple_with_proj_core_and_meet_const :
      type a c p l.
        c obj
        -> (p, c, l * disallowed) simple_morph
        -> p
        -> (a, p, l * disallowed) compose_proj_result
        -> (a, c, l * disallowed) morph =
    fun dst m0 c1 pm1 ->
      match pm1 with
        | Proj_core (m1, ax1, obj1) ->
          Proj_and (compose_simple dst m0 (Core_and_meet_const (c1, m1)), ax1, obj1)
        | Proj_id (ax1, obj1) ->
          Proj_and (compose_simple dst m0 (Meet_const c1), ax1, obj1)
        | Proj_const_max obj1 -> Const_max obj1
        | Proj_const_min obj1 -> Const_min obj1

  let compose_simple_with_proj_imply_const_and_core :
      type a c p r.
        c obj
        -> (p, c, disallowed * r) simple_morph
        -> a
        -> (a, p, disallowed * r) compose_proj_result
        -> (a, c, disallowed * r) morph =
    fun dst m0 c1 pm1 ->
      match pm1 with
        | Proj_core (m1, ax1, obj1) ->
          let c1 = Axis.proj ax1 c1 in
          Proj_and (compose_simple dst m0 (Imply_const_and_core (m1, c1)), ax1, obj1)
        | Proj_id (ax1, obj1) ->
          let c1 = Axis.proj ax1 c1 in
          Proj_and (compose_simple dst m0 (Imply_const c1), ax1, obj1)
        | Proj_const_max obj1 -> Const_max obj1
        | Proj_const_min obj1 -> Const_min obj1

  let rec lift_simple_morph_max :
      type a b p q.
      a obj
      -> b obj
      -> (p, q, disallowed * allowed) simple_morph
      -> (a, p) Axis.t
      -> (b, q) Axis.t
      -> (a, b, disallowed * allowed) simple_morph =
   fun src dst m ax0 ax1 ->
    let m : (a, b, disallowed * allowed) simple_morph =
      match m with
      | Id -> begin
          match src, dst, ax0, ax1 with
          | Comonadic_with_regionality, Comonadic_with_locality, _, _ ->
            Core (Locality_full Regional_to_global)
          | Comonadic_with_locality, Comonadic_with_regionality, _, _ ->
            Core (Locality_full Locality_as_regionality)
          | Comonadic_with_regionality, Comonadic_with_regionality, _, _ -> Id
          | Comonadic_with_locality, Comonadic_with_locality, _, _ -> Id
          | Monadic_op, Monadic_op, _, _ -> Id
          | _, _, _, _ -> .
        end
      | Core m -> begin
          match m, ax0, ax1, src, dst with
          | Uniqueness_op_to_linearity, Uniqueness, Linearity, _, _ ->
            Core Monadic_to_comonadic_max
          | Linearity_to_uniqueness_op, Linearity, Uniqueness, _, _ ->
            Core (Comonadic_to_monadic_max (comonadic_obj_areality src))
          | Contention_op_to_portability, Contention, Portability, _, _ ->
            Core Monadic_to_comonadic_max
          | Portability_to_contention_op, Portability, Contention, _, _ ->
            Core (Comonadic_to_monadic_max (comonadic_obj_areality src))
          | Visibility_op_to_statefulness, Visibility, Statefulness, _, _ ->
            Core Monadic_to_comonadic_max
          | Statefulness_to_visibility_op, Statefulness, Visibility, _, _ ->
            Core (Comonadic_to_monadic_max (comonadic_obj_areality src))
          | Locality_restricted lm, Areality, Areality, _, _ ->
            Core (Locality_full lm)
          | _,_,_,_,_ -> .
        end
      | Imply_const c ->
        let c = Axis.set ax1 c (max dst) in
        begin match src, dst, ax0, ax1 with
        | Comonadic_with_regionality, Comonadic_with_locality, _, _ ->
          compose_simple dst (Imply_const c) (Core (Locality_full Regional_to_global))
        | Comonadic_with_locality, Comonadic_with_regionality, _, _ ->
          compose_simple dst (Imply_const c) (Core (Locality_full Locality_as_regionality))
        | Comonadic_with_regionality, Comonadic_with_regionality, _, _ -> Imply_const c
        | Comonadic_with_locality, Comonadic_with_locality, _, _ -> Imply_const c
        | Monadic_op, Monadic_op, _, _ -> Imply_const c
        | _, _, _, _ -> .
        end
      | Imply_const_and_core (m, c) ->
        let m = lift_simple_morph_max src dst (Core m) ax0 ax1 in
        let c = Axis.set ax0 c (max src) in
        compose_simple dst m (Imply_const c)
    in
    let q_obj = proj_obj ax1 dst in
    let c = min_with dst ax1 (max q_obj) in
    compose_simple dst (Imply_const c) m

  let rec lift_simple_morph_min :
      type a b p q.
      a obj
      -> b obj
      -> (p, q, allowed * disallowed) simple_morph
      -> (a, p) Axis.t
      -> (b, q) Axis.t
      -> (a, b, allowed * disallowed) simple_morph =
   fun src dst m ax0 ax1 ->
    let m : (a, b, allowed * disallowed) simple_morph =
      match m with
      | Id -> begin
          match src, dst, ax0, ax1 with
          | Comonadic_with_regionality, Comonadic_with_locality, _, _ ->
            Core (Locality_full Regional_to_local)
          | Comonadic_with_locality, Comonadic_with_regionality, _, _ ->
            Core (Locality_full Locality_as_regionality)
          | Comonadic_with_regionality, Comonadic_with_regionality, _, _ -> Id
          | Comonadic_with_locality, Comonadic_with_locality, _, _ -> Id
          | Monadic_op, Monadic_op, _, _ -> Id
          | _, _, _, _ -> .
        end
      | Core m -> begin
          match m, ax0, ax1, src, dst with
          | Uniqueness_op_to_linearity, Uniqueness, Linearity, _, _ ->
            Core Monadic_to_comonadic_min
          | Linearity_to_uniqueness_op, Linearity, Uniqueness, _, _ ->
            Core (Comonadic_to_monadic_min (comonadic_obj_areality src))
          | Contention_op_to_portability, Contention, Portability, _, _ ->
            Core Monadic_to_comonadic_min
          | Portability_to_contention_op, Portability, Contention, _, _ ->
            Core (Comonadic_to_monadic_min (comonadic_obj_areality src))
          | Visibility_op_to_statefulness, Visibility, Statefulness, _, _ ->
            Core Monadic_to_comonadic_min
          | Statefulness_to_visibility_op, Statefulness, Visibility, _, _ ->
            Core (Comonadic_to_monadic_min (comonadic_obj_areality src))
          | Locality_restricted lm, Areality, Areality, _, _ ->
            Core (Locality_full lm)
          | _,_,_,_,_ -> .
        end
      | Meet_const c ->
        let c = Axis.set ax1 c (min dst) in
        begin match src, dst, ax0, ax1 with
        | Comonadic_with_regionality, Comonadic_with_locality, _, _ ->
          compose_simple dst (Meet_const c) (Core (Locality_full Regional_to_local))
        | Comonadic_with_locality, Comonadic_with_regionality, _, _ ->
          compose_simple dst (Meet_const c) (Core (Locality_full Locality_as_regionality))
        | Comonadic_with_regionality, Comonadic_with_regionality, _, _ -> Meet_const c
        | Comonadic_with_locality, Comonadic_with_locality, _, _ -> Meet_const c
        | Monadic_op, Monadic_op, _, _ -> Meet_const c
        | _, _, _, _ -> .
        end
      | Core_and_meet_const (c, m) ->
        let m = lift_simple_morph_min src dst (Core m) ax0 ax1 in
        let c = Axis.set ax1 c (min dst) in
        compose_simple dst (Meet_const c) m
    in
    let q_obj = proj_obj ax1 dst in
    let c = min_with dst ax1 (max q_obj) in
    compose_simple dst (Meet_const c) m

  let equal_mode : type a b. a obj -> b obj -> a -> b -> bool =
   fun a_obj b_obj a b ->
    match equal_obj a_obj b_obj with
    | Equal -> Misc.Le_result.equal ~le:(le a_obj) a b
    | Not_equal -> false

  let compose :
      type a b c d.
      c obj -> (b, c, d) morph -> (a, b, d) morph -> (a, c, d) morph =
   fun dst m0 m1 ->
    let res : (a, c, d) morph = match m0, m1 with
    | Simple m0, Simple m1 -> Simple (compose_simple dst m0 m1)
    | Const_max b_obj, _ -> Const_max (src b_obj m1)
    | Const_min b_obj, _ -> Const_min (src b_obj m1)
    | Const (b_obj,c), _ -> Const (src b_obj m1,c)
    | Simple m0, Proj_and(m1, ax1, obj1) ->
      Proj_and (compose_simple dst m0 m1, ax1, obj1)
    | Proj_and (m0, ax0, obj0) as m0', (Simple m1 as m1') -> begin
        match m1 with
        | Id -> m0'
        | Core m1 ->
          let pm1 = compose_projection_core ax0 m1 in
          compose_simple_with_proj_core dst m0 pm1
        | Meet_const c1 ->
          let c1 = Axis.proj ax0 c1 in
          Proj_and (compose_simple dst m0 (Meet_const c1), ax0, obj0)
        | Imply_const c1 ->
          let c1 = Axis.proj ax0 c1 in
          Proj_and (compose_simple dst m0 (Imply_const c1), ax0, obj0)
        | Core_and_meet_const (c1, m1) ->
          let c1 = Axis.proj ax0 c1 in
          let pm1 = compose_projection_core ax0 m1 in
          compose_simple_with_proj_core_and_meet_const dst m0 c1 pm1
        | Imply_const_and_core (m1, c1) ->
          let pm1 = compose_projection_core ax0 m1 in
          compose_simple_with_proj_imply_const_and_core dst m0 c1 pm1
        | Compose _ -> Compose (m0',m1')
      end
    | And_max_with (ax0, m0), Simple m1 ->
      let dst = proj_obj ax0 dst in
      And_max_with (ax0, compose_simple dst m0 m1)
    | Simple m0 as m0', (And_max_with (ax1, m1) as m1') -> begin
        let b_obj = src_simple dst m0 in
        let a_obj = src b_obj m1' in
        match m0 with
        | Id -> m1'
        | Core m0 -> begin
            match compose_and_max_with_core ax1 m0 with
            | And_max_core (ax1, m0) ->
              let obj0 = proj_obj ax1 dst in
              And_max_with (ax1, compose_simple obj0 (Core m0) m1)
            | Const_max_core -> Const_max a_obj
            | And_max_id ax1 -> And_max_with (ax1, m1)
            | Disallowed -> Compose (m0',m1')
          end
        | Imply_const c0 ->
          let c0 = Axis.proj ax1 c0 in
          let obj0 = proj_obj ax1 dst in
          And_max_with (ax1, compose_simple obj0 (Imply_const c0) m1)
        | Imply_const_and_core (m0, c0) -> begin
          let c0 = Axis.proj ax1 c0 in
          match compose_and_max_with_core ax1 m0 with
          | And_max_core (ax1, m0) ->
            let obj0 = proj_obj ax1 dst in
            And_max_with (ax1, compose_simple obj0 (Imply_const_and_core (m0,c0)) m1)
          | Const_max_core -> Const_max a_obj
          | And_max_id ax1 -> And_max_with (ax1, m1)
          | Disallowed -> Compose (m0',m1')
          end
        | Core_and_meet_const _ -> Compose (m0',m1')
        | Meet_const _ -> Compose (m0',m1')
        | Compose _ -> Compose (m0',m1')
      end
    | And_min_with (ax0, m0), Simple m1 ->
      let dst = proj_obj ax0 dst in
      And_min_with (ax0, compose_simple dst m0 m1)
    | Simple m0 as m0', (And_min_with (ax1, m1) as m1') -> begin
        let b_obj = src_simple dst m0 in
        let a_obj = src b_obj m1' in
        match m0 with
        | Id -> m1'
        | Core m0 -> begin
            match compose_and_min_with_core ax1 m0 with
            | And_min_core (ax1, m0) ->
              let obj0 = proj_obj ax1 dst in
              And_min_with (ax1, compose_simple obj0 (Core m0) m1)
            | Const_min_core -> Const_min a_obj
            | And_min_id ax1 -> And_min_with (ax1, m1)
            | Disallowed -> Compose (m0',m1')
          end
        | Meet_const c0 ->
          let c0 = Axis.proj ax1 c0 in
          let obj0 = proj_obj ax1 dst in
          And_min_with (ax1, compose_simple obj0 (Meet_const c0) m1)
        | Imply_const _ -> Compose (m0',m1')
        | Core_and_meet_const (c0, m0) -> begin
            match compose_and_min_with_core ax1 m0 with
            | And_min_core (ax1, m0) ->
              let obj0 = proj_obj ax1 dst in
              let c0 = Axis.proj ax1 c0 in
              And_min_with (ax1, compose_simple obj0 (Core_and_meet_const (c0, m0)) m1)
            | Const_min_core -> Const_min a_obj
            | And_min_id ax1 -> And_min_with (ax1, m1)
            | Disallowed -> Compose (m0',m1')
        end
        | Imply_const_and_core _ -> Compose (m0',m1')
        | Compose _ -> Compose (m0',m1')
      end
    | Proj_and(m0, ax0, obj1), And_max_with (ax1, m1) -> begin
        match Axis.equal ax0 ax1 with
        | Equal -> Simple (compose_simple dst m0 m1)
        | Not_equal ->
          let b_obj = src dst (Proj_and(m0, ax0, obj1)) in
          let a_obj = src b_obj (And_max_with (ax1, m1)) in
          Const_max a_obj
      end
    | Proj_and(m0, ax0, obj1), And_min_with (ax1, m1) -> begin
        match Axis.equal ax0 ax1 with
        | Equal -> Simple (compose_simple dst m0 m1)
        | Not_equal ->
          let b_obj = src dst (Proj_and(m0, ax0, obj1)) in
          let a_obj = src b_obj (And_min_with (ax1, m1)) in
          Const_min a_obj
      end
    | And_max_with (ax0, m0) as m0', (Proj_and(m1, ax1, obj1) as m1') ->
      let q_obj = proj_obj ax0 dst in
      let b_obj = src dst (And_max_with (ax0, m0)) in
      let a_obj = src b_obj (Proj_and(m1, ax1, obj1)) in
      let m0m1 = compose_simple q_obj m0 m1 in
      begin
        match maybe_allowed_right_simple m0m1 with
        | Allowed_right m0m1 ->
          allow_right (Simple (lift_simple_morph_max a_obj dst m0m1 ax1 ax0))
        | Not_allowed_right -> Compose (m0',m1')
      end
    | And_min_with (ax0, m0) as m0', (Proj_and(m1, ax1, obj1) as m1') ->
      let q_obj = proj_obj ax0 dst in
      let b_obj = src dst (And_min_with (ax0, m0)) in
      let a_obj = src b_obj (Proj_and(m1, ax1, obj1)) in
      let m0m1 = compose_simple q_obj m0 m1 in begin
        match maybe_allowed_left_simple m0m1 with
        | Allowed_left m0m1 ->
          allow_left (Simple (lift_simple_morph_min a_obj dst m0m1 ax1 ax0))
        | Not_allowed_left -> Compose (m0',m1')
      end
    | Simple m0, Const_max a_obj -> begin
        match maybe_allowed_right_simple m0 with
        | Allowed_right _ -> Const_max a_obj
        | Not_allowed_right ->
          let b_obj = src_simple dst m0 in
          Const (a_obj, apply_simple dst m0 (max b_obj))
      end
    | Simple m0, Const_min a_obj -> begin
        match maybe_allowed_left_simple m0 with
        | Allowed_left _ -> Const_min a_obj
        | Not_allowed_left ->
          let b_obj = src_simple dst m0 in
          Const (a_obj, apply_simple dst m0 (min b_obj))
      end
    | Proj_and (_m0, _ax0, _obj0), Const_max obj1 -> Const_max obj1
    | Proj_and (_m0, _ax0, _obj0), Const_min obj1 -> Const_min obj1
    | And_min_with (ax0, m0), Const_max obj1 ->
      let q_obj = proj_obj ax0 dst in
      let b_obj = src_simple q_obj m0 in
      Const (obj1, min_with dst ax0 (apply_simple q_obj m0 (max b_obj)))
    | And_min_with (ax0, m0), Const_min obj1 -> begin
        match maybe_allowed_left_simple m0 with
        | Allowed_left _ -> Const_min obj1
        | Not_allowed_left ->
          let q_obj = proj_obj ax0 dst in
          let b_obj = src_simple q_obj m0 in
          Const (obj1, min_with dst ax0 (apply_simple q_obj m0 (min b_obj)))
      end
    | And_max_with (ax0, m0), Const_max obj1 -> begin
        match maybe_allowed_right_simple m0 with
        | Allowed_right _ -> Const_max obj1
        | Not_allowed_right ->
          let q_obj = proj_obj ax0 dst in
          let b_obj = src_simple q_obj m0 in
          Const (obj1, max_with dst ax0 (apply_simple q_obj m0 (max b_obj)))
      end
    | And_max_with (ax0, m0), Const_min obj1 ->
      let q_obj = proj_obj ax0 dst in
      let b_obj = src_simple q_obj m0 in
      Const (obj1, max_with dst ax0 (apply_simple q_obj m0 (min b_obj)))
    | (_ as m0), Const (obj1,c1) -> Const (obj1,apply dst m0 c1)
    | (_ as m0), Compose (m1, m2) -> Compose (Compose (m0, m1), m2)
    | Compose (m0, m1), (_ as m2) -> Compose (Compose (m0, m1), m2)
    | Proj_and (_, _ax0, _obj0), Proj_and (_, _ax1, _obj1) -> failwith "should be refuted"
    | And_max_with _, And_max_with _ -> failwith "should be refuted"
    | And_max_with _, And_min_with _ -> failwith "should be refuted"
    | And_min_with _, And_max_with _ -> failwith "should be refuted"
    | And_min_with _, And_min_with _ -> failwith "should be refuted"
    in

    let b_obj = src dst m0 in
    let a_obj = src b_obj m1 in
    let c = min a_obj in
    if not (equal_mode dst dst (apply dst m0 (apply b_obj m1 c)) (apply dst res c)) then begin
      Format.eprintf "left : %a \n right : %a \n res : %a \n" (print_morph dst) m0 (print_morph b_obj) m1 (print_morph dst) res;
      Format.eprintf "original(%a) = %a \n res(%a) = %a \n"
      (print a_obj) c (print dst) (apply dst m0 (apply b_obj m1 c)) (print a_obj) c (print dst) (apply dst res c);
      assert false
    end;
    res

  module For_hint = struct
    (** Describes the portion of the input that's responsible for a portion of
        the output of a morphism *)
    type 'a responsible_axis =
      | None_responsible : 'a responsible_axis
          (** The input is not responsible for the output; instead, the morphism
              is solely responsible for the output. *)
      | All_responsible : 'a responsible_axis
          (** The input of the morphism is all responsible for the output. *)
      | Axis : ('a, 'a_x) Axis.t -> 'a responsible_axis
          (** The specified axis of the input object is responsible for the
              output. *)

    (* CR zqian: the following functions are hard to write, and are redundant since all
       the information are already in [apply]. A general and simpler apporach would work
       like this: Say we have [b = f a] on RHS, and we want to figure out which axis of
       [a] is responsible for a specific axis [ax] of [b] being low. We will iterate
       through all axes; for each axis, set that to [max] and get [a'], and calculate [b'
       = f a']. If [b'] is not strictly higher than [b] on [ax], then the current axis of
       [a] is not responsible for the [ax] of [b] being low. The iteration might end with
       no axis of [a] being responsible, in which case the morphism is solely
       respoonsible. *)

    let find_responsible_axis_proj_core : type a b b_ax l r.
        (a, b, l * r) core_morph -> (b, b_ax) Axis.t -> a responsible_axis =
      fun m ax ->
        match m, ax with
        | Locality_restricted _, _ -> .
        | Uniqueness_op_to_linearity, _ -> .
        | Linearity_to_uniqueness_op, _ -> .
        | Contention_op_to_portability, _ -> .
        | Portability_to_contention_op, _ -> .
        | Visibility_op_to_statefulness, _ -> .
        | Statefulness_to_visibility_op, _ -> .
        | Locality_full _, (Areality as ax) -> Axis ax
        | Locality_full _, (Forkable as ax) -> Axis ax
        | Locality_full _, (Yielding as ax) -> Axis ax
        | Locality_full _, (Linearity as ax) -> Axis ax
        | Locality_full _, (Statefulness as ax) -> Axis ax
        | Locality_full _, (Portability as ax) -> Axis ax
        | Locality_full _, _ -> .
        | Monadic_to_comonadic_min, Areality -> None_responsible
        | Monadic_to_comonadic_min, Forkable -> None_responsible
        | Monadic_to_comonadic_min, Yielding -> None_responsible
        | Monadic_to_comonadic_min, Linearity -> Axis Uniqueness
        | Monadic_to_comonadic_min, Statefulness -> Axis Visibility
        | Monadic_to_comonadic_min, Portability -> Axis Contention
        | Comonadic_to_monadic_min _, Uniqueness -> Axis Linearity
        | Comonadic_to_monadic_min _, Visibility -> Axis Statefulness
        | Comonadic_to_monadic_min _, Contention -> Axis Portability
        | Comonadic_to_monadic_min _, Staticity -> None_responsible
        | Monadic_to_comonadic_max, Areality -> None_responsible
        | Monadic_to_comonadic_max, Forkable -> None_responsible
        | Monadic_to_comonadic_max, Yielding -> None_responsible
        | Monadic_to_comonadic_max, Linearity -> Axis Uniqueness
        | Monadic_to_comonadic_max, Statefulness -> Axis Visibility
        | Monadic_to_comonadic_max, Portability -> Axis Contention
        | Comonadic_to_monadic_max _, Uniqueness -> Axis Linearity
        | Comonadic_to_monadic_max _, Visibility -> Axis Statefulness
        | Comonadic_to_monadic_max _, Contention -> Axis Portability
        | Comonadic_to_monadic_max _, Staticity -> None_responsible

    let rec find_responsible_axis_proj_simple : type a b b_ax l r.
        (a, b, l * r) simple_morph -> (b, b_ax) Axis.t -> a responsible_axis =
      fun m ax ->
        match m with
        | Id -> Axis ax
        | Meet_const _ -> Axis ax
        | Imply_const _ -> Axis ax
        | Core m | Core_and_meet_const(_, (m : (_, _, l * r) core_morph))
        | Imply_const_and_core((m : (_, _, l * r) core_morph), _) ->
            find_responsible_axis_proj_core m ax
        | Compose (mb, ma) -> begin
            match find_responsible_axis_proj_simple mb ax with
            | None_responsible -> None_responsible
            | All_responsible -> All_responsible
            | Axis ax -> find_responsible_axis_proj_simple ma ax
          end

    (** Given a morphism and an axis, return the portion of the input that's
        responsible for the specified axis of the output. *)
    let rec find_responsible_axis_proj : type a b b_ax l r.
        (a, b, l * r) morph -> (b, b_ax) Axis.t -> a responsible_axis =
     fun m ax ->
       match m with
       | Simple m -> find_responsible_axis_proj_simple m ax
       | Proj_and(_, ax, _) -> Axis ax
       | And_max_with(m_ax, _) -> begin
           match Axis.equal m_ax ax with
           | Not_equal -> None_responsible
           | Equal -> All_responsible
         end
       | And_min_with(m_ax, _) -> begin
           match Axis.equal m_ax ax with
           | Not_equal -> None_responsible
           | Equal -> All_responsible
         end
       | Const_max _ | Const_min _ | Const _ -> None_responsible
       | Compose (mb, ma) -> begin
            match find_responsible_axis_proj mb ax with
            | None_responsible -> None_responsible
            | All_responsible -> All_responsible
            | Axis ax -> find_responsible_axis_proj ma ax
          end

    (** Given a morphism return the portion of the input that's
        responsible for all of the output. *)
    let rec find_responsible_axis_all : type a b l r.
        (a, b, l * r) morph -> a responsible_axis = function
      | Simple _ -> All_responsible
      | Proj_and(_, ax, _) -> Axis ax
      | And_max_with _ | And_min_with _ -> All_responsible
      | Const_max _ | Const_min _ | Const _ -> None_responsible
      | Compose (mb, ma) ->
          match find_responsible_axis_all mb with
          | None_responsible -> None_responsible
          | All_responsible -> find_responsible_axis_all ma
          | Axis ax -> find_responsible_axis_proj ma ax
  end

end

module C = Lattices_mono
module S = Solver_mono (Hint_for_solver) (C)

let erase_hints () = S.erase_hints ()

type monadic = C.monadic =
  { uniqueness : C.Uniqueness.t;
    contention : C.Contention.t;
    visibility : C.Visibility.t;
    staticity : C.Staticity.t
  }

type 'a comonadic_with = 'a C.comonadic_with =
  { areality : 'a;
    linearity : C.Linearity.t;
    portability : C.Portability.t;
    forkable : C.Forkable.t;
    yielding : C.Yielding.t;
    statefulness : C.Statefulness.t
  }

module Axis = C.Axis

type nonrec 'a simple_error = 'a simple_error

let print_longident =
  ref (fun _ _ -> assert false : Fmt.formatter -> Longident.t -> unit)

module Report = struct
  open Hint

  (** Human-readable mode error hints. Compared to [S.error]:
      - This doesn't contain branch, and thus forms a chain.
      - Each node on the chain talks about a single axis, instead of potentially
        on a product lattice. *)
  type 'd hint =
    | Apply : 'd morph * 'b C.obj * ('b, 'd) ahint -> 'd hint
    | Const : 'd const -> 'd hint
    | Irrelevant : ('l * 'r) hint
        (** The current mode is not responsible for the error (that is, the
            surrounding morphism is solely responsible), and should not be
            printed. *)
    constraint 'd = 'l * 'r
  [@@ocaml.warning "-62"]

  and ('a, 'd) ahint = 'a * 'd hint constraint 'd = 'l * 'r

  (** Human-readible mode error report. *)
  type 'a t =
    { left : ('a, left_only) ahint;
      right : ('a, right_only) ahint
    }

  (** Convert Solver error to report. *)
  module Of_solver = struct
    (** Given a branch of two constant bounds on a single axis, choose the the
        one that's responsible for the branch. *)
    let choose_branch_axis : type a l r.
        (l * r) Solver_intf.branch -> a C.obj -> a -> a -> [`First | `Second] =
     fun b a_obj x y ->
      (* CR-someday zqian: in the case where [x = y], we currently arbitrarily choose from
         [x] and [y], which are unordered anyway. In the future we might want to keep an
         order for better error messages. For example, order them by occurrence in the
         source code such that the more recent hint is returned. *)
      match b with
      | Join ->
        if C.le a_obj x y
        then `Second
        else if C.le a_obj y x
        then `First
        else Misc.fatal_error "A single axis should be a total ordering."
      | Meet ->
        if C.le a_obj x y
        then `First
        else if C.le a_obj y x
        then `Second
        else Misc.fatal_error "A single axis should be a total ordering."

    (** Given a solver hint on a product lattice, and an axis in that product
        that we are interested in, returns a human-readible hint.*)
    let rec hint_apply : type a b l r.
        a C.obj ->
        (l * r) morph ->
        (b, a, l * r) C.morph ->
        (b, l * r) S.ahint ->
        b C.For_hint.responsible_axis ->
        (l * r) hint =
     fun obj morph_hint morph ahint res ->
      let obj = C.src obj morph in
      match res with
      | None_responsible -> Irrelevant
      | All_responsible ->
        let ahint = ahint_all obj ahint in
        Apply (morph_hint, obj, ahint)
      | Axis ax ->
        let ahint = ahint_proj obj ax ahint in
        let obj = C.proj_obj ax obj in
        Apply (morph_hint, obj, ahint)

    and hint_proj : type t a l r.
        t C.obj -> (t, a) Axis.t -> (t, l * r) S.hint -> (l * r) hint =
     fun obj ax -> function
      | Apply (morph_hint, morph, ahint) ->
        hint_apply obj morph_hint morph ahint
          (C.For_hint.find_responsible_axis_proj morph ax)
      | Const c -> Const c
      | Branch (b, (a1, hint1), (a2, hint2)) ->
        let chosen_hint =
          let proj1 = Axis.proj ax a1 in
          let proj2 = Axis.proj ax a2 in
          let obj = C.proj_obj ax obj in
          match choose_branch_axis b obj proj1 proj2 with
          | `First -> hint1
          | `Second -> hint2
        in
        hint_proj obj ax chosen_hint

    and ahint_proj : type t a l r.
        t C.obj -> (t, a) Axis.t -> (t, l * r) S.ahint -> (a, l * r) ahint =
     fun obj ax (t, hint) ->
      let a = Axis.proj ax t in
      let hint = hint_proj obj ax hint in
      a, hint

    (** Given a solver hint on a single axis lattice, returns a human-readible
        hint. *)
    and hint_all : type a l r. a C.obj -> (a, l * r) S.hint -> (l * r) hint =
     fun obj -> function
      | Apply (morph_hint, morph, ahint) ->
        hint_apply obj morph_hint morph ahint
          (C.For_hint.find_responsible_axis_all morph)
      | Const c -> Const c
      | Branch (b, (a1, hint1), (a2, hint2)) ->
        let chosen_hint =
          match choose_branch_axis b obj a1 a2 with
          | `First -> hint1
          | `Second -> hint2
        in
        hint_all obj chosen_hint

    and ahint_all : type a l r.
        a Lattices_mono.obj -> (a, l * r) S.ahint -> (a, l * r) ahint =
     fun obj (a, hint) -> a, hint_all obj hint

    let error_proj : type r a. r C.obj -> (r, a) Axis.t -> r S.error -> a t =
     fun obj axis { left; right } ->
      let left = ahint_proj obj axis left in
      let right = ahint_proj obj axis right in
      { left; right }

    let error_all : type a. a C.obj -> a S.error -> a t =
     fun obj { left; right } ->
      let left = ahint_all obj left in
      let right = ahint_all obj right in
      { left; right }
  end

  [@@@warning "-4"]

  type sound =
    | Consonant
    | Vowel

  let print_article_noun ~definite ~capitalize sound s =
    let article =
      match definite, sound with
      | true, _ -> "the"
      | false, Consonant -> "a"
      | false, Vowel -> "an"
    in
    let article =
      if capitalize then String.capitalize_ascii article else article
    in
    Fmt.dprintf "%s %s" article s

  let print_lock_item : lock_item -> _ = function
    | Module -> print_article_noun Consonant "module"
    | Class -> print_article_noun Consonant "class"
    | Value -> print_article_noun Consonant "value"
    | Constructor -> print_article_noun Consonant "constructor"

  let print_structure_item : structure_item -> _ =
   fun (category, id) ~capitalize ->
    Fmt.dprintf "%t %a"
      (print_lock_item ~definite:true ~capitalize category)
      Misc.Style.inline_code (Ident.name id)

  let print_pinpoint_desc : pinpoint_desc -> _ = function
    | Unknown -> None
    | Ident { category; lid } ->
      Some
        (fun ~definite ~capitalize ->
          Fmt.dprintf "%t %a"
            (print_lock_item ~definite ~capitalize category)
            (Misc.Style.as_inline_code !print_longident)
            lid)
    | Function -> Some (print_article_noun Consonant "function")
    | Functor -> Some (print_article_noun Consonant "functor")
    | Lazy -> Some (print_article_noun Consonant "lazy expression")
    | Expression -> Some (print_article_noun Vowel "expression")
    | Allocation -> Some (print_article_noun Vowel "allocation")
    | Class -> Some (print_article_noun Consonant "class")
    | Object -> Some (print_article_noun Vowel "object")
    | Loop -> Some (print_article_noun Consonant "loop")
    | Letop -> Some (print_article_noun Consonant "letop")
    | Cases_result ->
      Some
        (fun ~definite ~capitalize ->
          Fmt.dprintf "%t of %t"
            (print_article_noun ~definite:true ~capitalize Consonant "result")
            (print_article_noun ~definite ~capitalize:false Consonant "cases"))
    | Pattern -> Some (print_article_noun Consonant "pattern")
    | Module -> Some (print_article_noun Consonant "module")
    | Structure -> Some (print_article_noun Consonant "structure")
    | Structure_item x ->
      Some
        (fun ~definite:_ ~capitalize ->
          Fmt.dprintf "%t in the structure" (print_structure_item ~capitalize x))

  let print_pinpoint : pinpoint -> _ =
   fun (loc, desc) ->
    print_pinpoint_desc desc
    |> Option.map (fun print_desc ~definite ~capitalize ppf ->
        match Location.is_none loc, definite with
        | true, _ -> print_desc ~definite:false ~capitalize ppf
        | false, true ->
          Fmt.fprintf ppf "%t at %a"
            (print_desc ~definite ~capitalize)
            (Location.Doc.loc ~capitalize_first:false)
            loc
        | false, false ->
          Fmt.fprintf ppf "%t (at %a)"
            (print_desc ~definite ~capitalize)
            (Location.Doc.loc ~capitalize_first:false)
            loc)

  let is_known_pinpoint : pinpoint -> bool = function
    | _, Unknown -> false
    | _ -> true

  let print_mutable_part ppf = function
    | Record_field s ->
      Fmt.fprintf ppf "mutable field %a" Misc.Style.inline_code s
    | Array_elements -> Fmt.fprintf ppf "array elements"

  let print_always_dynamic = function
    | Application -> Fmt.dprintf "function applications"
    | Try_with -> Fmt.dprintf "try-with clauses"

  let print_legacy = function
    | Toplevel -> print_article_noun Consonant "top-level clause"
    | Compilation_unit -> print_article_noun Consonant "compilation unit"
    | Class -> print_article_noun Consonant "class"

  let print_region_desc : region_desc -> _ = function
    | Borrow -> print_article_noun Consonant "borrow region"

  let print_region : capitalize:_ -> region -> _ =
   fun ~capitalize (loc, desc) ->
    Fmt.dprintf "%t at %a"
      (print_region_desc desc ~definite:true ~capitalize)
      (Location.Doc.loc ~capitalize_first:false)
      loc

  let print_containing ppf { containing; container } =
    match containing with
    | Tuple ->
        fprintf ppf "is an element of the tuple at %a"
          Location.print_loc container
    | Record (s, _) ->
        fprintf ppf "is the field %a of the record at %a"
          Misc.Style.inline_code s
          Location.print_loc container
    | Array _ ->
        fprintf ppf "is an element of the array at %a"
          Location.print_loc container
    | Constructor (s, _) ->
        fprintf ppf "is contained (via constructor %a) in the value at %a"
          Misc.Style.inline_code s Location.print_loc container

  let print_containing ppf { containing; container } =
    match containing with
    | Tuple ->
        fprintf ppf "is an element of the tuple at %a"
          Location.print_loc container
    | Record (s, _) ->
        fprintf ppf "is the field %a of the record at %a"
          Misc.Style.inline_code s
          Location.print_loc container
    | Array _ ->
        fprintf ppf "is an element of the array at %a"
          Location.print_loc container
    | Constructor (s, _) ->
        fprintf ppf "is contained (via constructor %a) in the value at %a"
          Misc.Style.inline_code s Location.print_loc container

  (** Given a pinpoint and a const, where the pinpoint has been expressed,
      prints the const to explain the mode on the pinpoint. *)
  let print_const (type l r) ((_, pp_desc) : pinpoint) ppf :
      (l * r) const -> unit = function
    | Unknown -> Misc.fatal_error "Unknown hint should not be printed"
    | Lazy_allocated_on_heap ->
      (match pp_desc with
      | Lazy ->
        (* if we already said it's a lazy, we don't need to emphasize it again. *)
        Fmt.pp_print_string ppf "lazy expressions always need"
      | _ -> Fmt.pp_print_string ppf "it is a lazy expression and thus needs");
      Fmt.pp_print_string ppf " to be allocated on the heap"
    | Legacy m ->
      (match pp_desc, m with
      | ( (Ident { category = Class; _ } | Class | Structure_item (Class, _)),
          Class ) ->
        (* if we already said it's a class, we don't need to emphasize it again. *)
        Fmt.pp_print_string ppf "classes are always"
      | _ ->
        Fmt.fprintf ppf "it is %t and thus always"
          (print_legacy m ~definite:false ~capitalize:false));
      Fmt.pp_print_string ppf " at the legacy modes"
    | Tailcall_function ->
      Fmt.pp_print_string ppf "it is the function in a tail call"
    | Tailcall_argument ->
      Fmt.pp_print_string ppf "it is an argument in a tail call"
    | Mutable_read m ->
      Fmt.fprintf ppf "its %a is being read" print_mutable_part m
    | Mutable_write m ->
      Fmt.fprintf ppf "its %a is being written" print_mutable_part m
    | Lazy_forced -> (
      match pp_desc with
      | Lazy ->
        (* if we already said it's a lazy, we don't need to emphasize it again. *)
        Fmt.pp_print_string ppf "it is being forced"
      | _ -> Fmt.pp_print_string ppf "it is a lazy value being forced")
    | Function_return ->
      Fmt.fprintf ppf
        "it is a function return value.@ Hint: Use exclave_ to return a local \
         value"
    | Stack_expression ->
      Fmt.fprintf ppf "it is %a-allocated" Misc.Style.inline_code "stack_"
    | Module_allocated_on_heap ->
      (match pp_desc with
      | Ident { category = Module; _ }
      | Functor | Module | Structure
      | Structure_item (Module, _) ->
        (* if we already said it's a module, we don't need to emphasize it again. *)
        Fmt.pp_print_string ppf "modules always need"
      | _ -> Fmt.pp_print_string ppf "it is a module and thus needs");
      Fmt.pp_print_string ppf " to be allocated on the heap"
    | Is_used_in pp ->
      let print_pp = print_pinpoint pp |> Option.get in
      Fmt.fprintf ppf "it is used in %t"
        (print_pp ~definite:false ~capitalize:false)
    | Always_dynamic x ->
      Fmt.fprintf ppf "%t are always dynamic" (print_always_dynamic x)
    | Branching -> Fmt.fprintf ppf "it has branches"
    | Borrowed _ -> Fmt.fprintf ppf "it is borrowed"
    | Escape_region reg ->
      Fmt.fprintf ppf "it escapes %t" (print_region ~capitalize:false reg)
    | Contained_by c -> print_containing ppf c

  let print_allocation_l : allocation -> Fmt.formatter -> unit =
   fun { txt; loc } ->
    match txt with
    | Unknown ->
      Fmt.dprintf "is allocated at %a containing data"
        (Location.Doc.loc ~capitalize_first:false)
        loc
    | Optional_argument ->
      Fmt.dprintf
        "is an optional argument wrapper (and thus allocated) of the value at \
         %a"
        (Location.Doc.loc ~capitalize_first:false)
        loc
    | Function_coercion ->
      Fmt.dprintf
        "is a partial application of the function at %a on omittable parameters"
        (Location.Doc.loc ~capitalize_first:false)
        loc
    | Float_projection ->
      Fmt.dprintf
        "is projected (at %a) from a float record (and thus allocated)"
        (Location.Doc.loc ~capitalize_first:false)
        loc

  let print_allocation_r : allocation -> Fmt.formatter -> unit =
   fun { txt; _ } ->
    match txt with
    | Unknown -> Fmt.dprintf "is an allocation"
    | Optional_argument ->
      Fmt.dprintf
        "is to be put in an optional argument wrapper (and thus an allocation)"
    | Function_coercion ->
      Fmt.dprintf
        "is to omit some parameters by partial application (and thus an \
         allocation)"
    | Float_projection ->
      Fmt.dprintf "is a float-record projection (and thus an allocation)"

  let modality_if_relevant ~fixpoint pp =
    if
      fixpoint
      (* if the modality doesn't change the bound, we omit the modality and
          print the remaining chain. *)
    then (fun _ppf Modality -> ()), pp
    else
      (* if the modality change the bound, we signal that. Moreover, since each
         axis is total ordering, the modality is solely responsible for the
         bound, and we omit the remaining chain. *)
      (* CR-someday zqian: print the modality on the offending axis. *)
      ( (fun ppf Modality -> Fmt.fprintf ppf " (with some modality)"),
        (Location.none, Unknown : pinpoint) )

  let print_contains :
      fixpoint:bool -> contains -> ((Fmt.formatter -> unit) * pinpoint) option =
   fun ~fixpoint { containing; contained } ->
    print_pinpoint contained
    |> Option.map (fun print_pp ->
        let print_pp = print_pp ~definite:true ~capitalize:false in
        let maybe_modality, contained =
          modality_if_relevant ~fixpoint contained
        in
        let pr =
          match containing with
          | Tuple -> Fmt.dprintf "is a tuple that contains %t" print_pp
          | Record (s, moda) ->
            Fmt.dprintf "is a record whose field %a%a is %t"
              Misc.Style.inline_code s maybe_modality moda print_pp
          | Array moda ->
            Fmt.dprintf "is an array that contains%a %t" maybe_modality moda
              print_pp
          | Constructor (s, moda) ->
            Fmt.dprintf "contains (via constructor %a)%a %t"
              Misc.Style.inline_code s maybe_modality moda print_pp
          | Structure (x, moda) ->
            Fmt.dprintf "contains %t%a defined as %t"
              (print_structure_item ~capitalize:false x)
              maybe_modality moda print_pp
        in
        pr, contained)

  let print_is_contained_by :
      fixpoint:bool -> is_contained_by -> (Fmt.formatter -> unit) * pinpoint =
   fun ~fixpoint { containing; container } ->
    let maybe_modality, pp = modality_if_relevant ~fixpoint container in
    (* CR-someday zqian: Use the full [container] to improve the printing below.
       E.g., insted of printing "the tuple at XXX", we can print "the tuple
       pattern at XXX" or "the tuple expression at XXX". *)
    let container = fst container in
    let pr =
      match containing with
      | Tuple ->
        Fmt.dprintf "is an element of the tuple at %a"
          (Location.Doc.loc ~capitalize_first:false)
          container
      | Record (s, moda) ->
        Fmt.dprintf "is the field %a%a of the record at %a"
          Misc.Style.inline_code s maybe_modality moda
          (Location.Doc.loc ~capitalize_first:false)
          container
      | Array moda ->
        Fmt.dprintf "is an element%a of the array at %a" maybe_modality moda
          (Location.Doc.loc ~capitalize_first:false)
          container
      | Constructor (s, moda) ->
        Fmt.dprintf "is contained (via constructor %a)%a in the value at %a"
          Misc.Style.inline_code s maybe_modality moda
          (Location.Doc.loc ~capitalize_first:false)
          container
      | Structure (x, moda) ->
        Fmt.dprintf "is %t%a in the structure at %a"
          (print_structure_item ~capitalize:false x)
          maybe_modality moda
          (Location.Doc.loc ~capitalize_first:false)
          container
    in
    pr, pp

  (** Given a pinpoint and a morph, where the pinpoint is the destination of the
      morph and have been expressed already, print the morph and return the
      source pinpoint. The source pinpoint could be [Unknown], in which case the
      rest of the chain will not be printed. *)
  let print_morph : type l r.
      fixpoint:bool ->
      pinpoint ->
      (l * r) morph ->
      ((Fmt.formatter -> unit) * pinpoint) option =
   fun ~fixpoint pp -> function
    | Skip -> Misc.fatal_error "Skip hint should not be printed"
    | Unknown -> None
    | Close_over (Comonadic, { closed = pp; _ }) ->
      print_pinpoint pp
      |> Option.map (fun print_pp ->
          ( Fmt.dprintf "closes over %t"
              (print_pp ~definite:true ~capitalize:false),
            pp ))
    | Close_over (Monadic, { closed = pp; _ }) ->
      print_pinpoint pp
      |> Option.map (fun print_pp ->
          ( Fmt.dprintf "contains a usage (of %t)"
              (print_pp ~definite:true ~capitalize:false),
            pp ))
    | Is_closed_by (_, { closure = pp; _ }) ->
      print_pinpoint pp
      |> Option.map (fun print_pp ->
          ( Fmt.dprintf "is used inside %t"
              (print_pp ~definite:true ~capitalize:false),
            pp ))
    | Captured_by_partial_application ->
      Some
        ( Fmt.dprintf "is captured by a partial application",
          (Location.none, Expression) )
    | Adj_captured_by_partial_application ->
      Some
        ( Fmt.dprintf "has a partial application capturing a value",
          (Location.none, Expression) )
    | Crossing -> Some (Fmt.dprintf "crosses with something", pp)
    | Allocation_r alloc -> Some (print_allocation_r alloc, pp)
    | Allocation_l alloc -> Some (print_allocation_l alloc, pp)
    | Contains_l (_, contains) -> print_contains ~fixpoint contains
    | Contains_r (_, contains) -> print_contains ~fixpoint contains
    | Is_contained_by (_, is_contained_by) ->
      Some (print_is_contained_by ~fixpoint is_contained_by)

  let print_mode : type a.
      [`Actual | `Expected] -> a C.obj -> Fmt.formatter -> a -> unit =
   fun side obj ppf x ->
    let mode_printer = Misc.Style.as_inline_code (C.print obj) in
    match side, obj, x with
    | `Actual, Regionality, Regional ->
      Fmt.fprintf ppf "%a to the parent region" mode_printer C.Regionality.Local
      (* CR-someday zqian: treat the following cases generally. *)
    | `Expected, Contention_op, Shared ->
      (* When "shared" is expected, we tell the user that either shared or
         uncontended is expected. *)
      Fmt.fprintf ppf "%a or %a" mode_printer C.Contention.Shared mode_printer
        C.Contention.Uncontended
    | `Expected, Visibility_op, Read ->
      Fmt.fprintf ppf "%a or %a" mode_printer C.Visibility.Read mode_printer
        C.Visibility.Read_write
    | `Expected, Regionality, Regional ->
      Fmt.fprintf ppf "%a to the parent region or %a" mode_printer
        C.Regionality.Local mode_printer C.Regionality.Global
    | _ -> mode_printer ppf x
  [@@ocaml.warning "-4"]

  let adjust_side : type a. a C.obj -> [`Left | `Right] -> [`Actual | `Expected]
      =
   fun obj side ->
    match C.is_opposite obj, side with
    | true, `Left -> `Expected
    | true, `Right -> `Actual
    | false, `Left -> `Actual
    | false, `Right -> `Expected

  let print_mode_with_side : type a.
      sub:bool -> [`Left | `Right] -> a C.obj -> Fmt.formatter -> a -> unit =
   fun ~sub side obj ppf a ->
    let side = adjust_side obj side in
    if sub
    then (
      Fmt.fprintf ppf "@ which ";
      match side with
      | `Actual -> Fmt.pp_print_string ppf "is "
      | `Expected -> Fmt.pp_print_string ppf "is expected to be ");
    print_mode side obj ppf a

  (** Some morph hints are said to be "non-rigid", because they should be
      printed only when they change modes. *)
  let is_rigid : type l r. (l * r) morph -> bool = function
    | Unknown -> true
    | Close_over _ | Is_closed_by _ | Captured_by_partial_application
    | Contains_l _ | Contains_r _ | Is_contained_by _
    | Adj_captured_by_partial_application ->
      true
    | Allocation_r _ | Allocation_l _ | Skip | Crossing -> false

  let equal_mode : type a b. a C.obj -> b C.obj -> a -> b -> bool =
   fun a_obj b_obj a b ->
    match C.equal_obj a_obj b_obj with
    | Equal -> Misc.Le_result.equal ~le:(C.le a_obj) a b
    | Not_equal -> false

  let rec print_ahint : type a l r.
      ?sub:bool ->
      [`Left | `Right] ->
      pinpoint ->
      a C.obj ->
      Fmt.formatter ->
      (a, l * r) ahint ->
      print_error_result option =
   fun ?(sub = false) side pp (obj : a C.obj) ppf (a, hint) ->
    match hint with
    | Apply (morph_hint, src, ahint) ->
      let fixpoint = equal_mode obj src a (fst ahint) in
      if (not (is_rigid morph_hint)) && fixpoint
      then print_ahint ~sub side pp src ppf ahint
      else (
        print_mode_with_side ~sub side obj ppf a;
        match print_morph ~fixpoint pp morph_hint with
        | None -> Some Mode
        | Some (t, pp) ->
          Fmt.fprintf ppf "@ because it %t" t;
          if is_known_pinpoint pp
          then ignore (print_ahint ~sub:true side pp src ppf ahint);
          Some Mode_with_hint)
    | Const Unknown ->
      print_mode_with_side ~sub side obj ppf a;
      Some Mode
    | Irrelevant ->
      if not sub
      then
        Misc.fatal_error
          "the current mode is not responsible for the error, so must be \
           inside a responsible morphism";
      None
    | Const c ->
      Fmt.fprintf ppf "%a@ because %a"
        (print_mode_with_side ~sub side obj)
        a (print_const pp) c;
      Some Mode_with_hint
  [@@ocaml.warning "-4"]

  type 'a ahint_sided =
    | Left of ('a, left_only) ahint
    | Right of ('a, right_only) ahint

  let print_ahint_sided : type a.
      pinpoint ->
      a C.obj ->
      Fmt.formatter ->
      a ahint_sided ->
      print_error_result option =
   fun pp obj ppf ahint_sided ->
    match ahint_sided with
    | Left ahint -> print_ahint `Left pp obj ppf ahint
    | Right ahint -> print_ahint `Right pp obj ppf ahint

  let print : type a. pinpoint -> a C.obj -> a t -> print_error =
   fun pp obj { left; right } ->
    let actual, expected =
      if C.is_opposite obj
      then Right right, Left left
      else Left left, Right right
    in
    let left ppf = Option.get (print_ahint_sided pp obj ppf actual) in
    let right ppf = Option.get (print_ahint_sided pp obj ppf expected) in
    { left; right }
end

let print_pinpoint = Report.print_pinpoint

type changes = S.changes

let undo_changes = S.undo_changes

(* To be filled in by [types.ml] *)
let append_changes : (changes ref -> unit) ref = ref (fun _ -> assert false)

let set_append_changes f = append_changes := f

type ('a, 'd) mode = ('a, 'd) S.mode

module Error = struct
  type 'a t = 'a S.error_raw

  type packed =
    | Proj : 'r C.obj * ('r, 'a) Axis.t * 'r t -> packed
    | All : 'a C.obj * 'a t -> packed

  let print_proj : type r a.
      Hint.pinpoint -> r C.obj -> (r, a) Axis.t -> r t -> print_error =
   fun pp obj ax err ->
    let err = S.populate_error obj err in
    let err = Report.Of_solver.error_proj obj ax err in
    let obj = C.proj_obj ax obj in
    Report.print pp obj err

  let print_all : type a. Hint.pinpoint -> a C.obj -> a t -> print_error =
   fun pp obj err ->
    let err = S.populate_error obj err in
    let err = Report.Of_solver.error_all obj err in
    Report.print pp obj err

  let print_packed : Hint.pinpoint -> packed -> print_error =
   fun pp -> function
    | Proj (obj, ax, err) -> print_proj pp obj ax err
    | All (obj, err) -> print_all pp obj err

  let print_packed_simple_context : Hint.pinpoint -> packed -> Location.error =
   fun pp packed ->
    let open Format_doc in
    let loc, desc = pp in
    let print ppf () =
      let open_box = Fmt.dprintf "@[<hov 2>" in
      let reopen_box = Fmt.dprintf "@]@ %t" open_box in
      let print_desc = Report.print_pinpoint_desc desc in
      (let print_desc =
         match print_desc with
         | None -> Fmt.dprintf "This"
         | Some print_desc -> print_desc ~definite:true ~capitalize:true
       in
       fprintf ppf "%t%t is " open_box print_desc);
      let ({ left; right } : print_error) = print_packed pp packed in
      (match left ppf with
      | Mode_with_hint ->
        let print_desc =
          match print_desc with
          | None -> Fmt.dprintf "the highlighted"
          | Some print_desc ->
            Fmt.dprintf "%t highlighted"
              (print_desc ~definite:true ~capitalize:false)
        in
        fprintf ppf ".%tHowever, %t is expected to be " reopen_box print_desc
      | Mode -> fprintf ppf "%tbut is expected to be " reopen_box);
      ignore (right ppf);
      fprintf ppf ".@]"
    in
    Location.error_of_printer ~loc print ()
end

exception Submode_error_simple_context of Hint.pinpoint * Error.packed

let () =
  Location.register_error_of_exn (function
    | Submode_error_simple_context (pp, err) ->
      Some (Error.print_packed_simple_context pp err)
    | _ -> None)

module type Common_axis_pos = sig
  module Const : Const

  include
    Common_axis
      with module Const := Const
       and type 'd t = (Const.t, 'd pos) mode
       and type 'd hint_const := 'd pos_hint_const
end

module type Common_axis_neg = sig
  module Const : Const

  include
    Common_axis
      with module Const := Const
       and type 'd t = (Const.t, 'd neg) mode
       and type 'd hint_const := 'd neg_hint_const
end

(** Representing a single object *)
module type Obj = sig
  type const

  val obj : const C.obj
end

let try_with_log op =
  let log' = ref S.empty_changes in
  let log = Some log' in
  match op ~log with
  | Ok _ as x ->
    !append_changes log';
    x
  | Error _ as x ->
    S.undo_changes !log';
    x
[@@inline]

let with_log op =
  let log' = ref S.empty_changes in
  let log = Some log' in
  let r = op ~log in
  !append_changes log';
  r
[@@inline]

let equate_from_submode submode_log m1 m2 ~log =
  match submode_log m1 m2 ~log with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode_log m2 m1 ~log with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())
[@@inline]

let equate_from_submode' submode m1 m2 =
  match submode m1 m2 with
  | Error e -> Error (Left_le_right, e)
  | Ok () -> (
    match submode m2 m1 with
    | Error e -> Error (Right_le_left, e)
    | Ok () -> Ok ())
[@@inline]

module Comonadic_gen (Obj : Obj) = struct
  open Obj

  type 'd t = (const, 'd) S.mode

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec simple_error = const simple_error

  type nonrec error = const Error.t

  type equate_error = equate_step * error

  type (_, _, 'd) sided = 'd t

  let disallow_right m = S.disallow_right m

  let disallow_left m = S.disallow_left m

  let allow_left m = S.allow_left m

  let allow_right m = S.allow_right m

  let newvar () = S.newvar obj

  let min : lr = S.min obj

  let max : lr = S.max obj

  let newvar_above m = S.newvar_above obj m

  let newvar_below m = S.newvar_below obj m

  let submode_log ?(pp = (Location.none, Unknown : Hint.pinpoint)) a b ~log =
    S.submode pp obj a b ~log

  let to_simple_error ({ left; right; _ } : error) : simple_error =
    { left; right }

  let submode ?pp a b = try_with_log (submode_log ?pp a b)

  let submode_err pp a b =
    match submode ~pp a b with
    | Ok () -> ()
    | Error e -> raise (Submode_error_simple_context (pp, All (obj, e)))

  let print_error pp err = Error.print_all pp obj err

  let join l = S.join obj l

  let meet l = S.meet obj l

  let submode_exn ?pp m1 m2 = submode ?pp m1 m2 |> Result.get_ok

  let equate a b = try_with_log (equate_from_submode (submode_log ?pp:None) a b)

  let equate_exn m1 m2 = equate m1 m2 |> Result.get_ok

  let print ?verbose () ppf m = S.print ?verbose obj ppf m

  let zap_to_ceil m = with_log (S.zap_to_ceil obj m)

  let zap_to_floor m = with_log (S.zap_to_floor obj m)

  let of_const : type l r. ?hint:(l * r) pos Hint.const -> const -> (l * r) t =
   fun ?hint a -> S.of_const ?hint obj a

  let to_const_exn m = S.to_const_exn obj m

  let unhint = S.Unhint.unhint

  let hint ?hint = S.Unhint.hint obj ?hint

  let wrap ?hint:h f m = m |> unhint |> f |> hint ?hint:h

  let apply_hint hint m = wrap ~hint Fun.id m

  let meet_const_unhint c m =
    S.Unhint.apply obj (Simple (Meet_const c)) m

  let meet_const ?hint c m =
    wrap ?hint (meet_const_unhint c) (disallow_right m)

  let imply_const_unhint c m =
    S.Unhint.apply obj (Simple (Imply_const c)) m

  let imply_const c m =
    m |> disallow_left |> wrap (imply_const_unhint c)

  module Guts = struct
    let get_floor m = S.get_floor obj m

    let get_ceil m = S.get_ceil obj m

    let get_loose_floor m = S.get_loose_floor obj m

    let get_loose_ceil m = S.get_loose_ceil obj m
  end
end
[@@inline]

module Monadic_gen (Obj : Obj) = struct
  (* Monadic lattices are flipped. See "Notes on flipping". *)
  open Obj

  type 'd t = (const, 'r * 'l) S.mode constraint 'd = 'l * 'r

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  type nonrec simple_error = const simple_error

  type nonrec error = const Error.t

  type equate_error = equate_step * error

  type (_, _, 'd) sided = 'd t

  let disallow_right m = S.disallow_left m

  let disallow_left m = S.disallow_right m

  let allow_left m = S.allow_right m

  let allow_right m = S.allow_left m

  let newvar () = S.newvar obj

  let min : lr = S.allow_left (S.max obj)

  let max : lr = S.allow_right (S.min obj)

  let newvar_above m = S.newvar_below obj m

  let newvar_below m = S.newvar_above obj m

  let submode_log ?(pp = (Location.none, Unknown : Hint.pinpoint)) a b ~log =
    S.submode pp obj b a ~log

  let to_simple_error ({ left; right; _ } : error) : simple_error =
    { left = right; right = left }

  let submode ?pp a b = try_with_log (submode_log ?pp a b)

  let submode_err pp a b =
    match submode ~pp a b with
    | Ok () -> ()
    | Error e -> raise (Submode_error_simple_context (pp, All (obj, e)))

  let print_error pp err = Error.print_all pp obj err

  let join l = S.meet obj l

  let meet l = S.join obj l

  let submode_exn ?pp m1 m2 = submode ?pp m1 m2 |> Result.get_ok

  let equate a b = try_with_log (equate_from_submode (submode_log ?pp:None) a b)

  let equate_exn m1 m2 = equate m1 m2 |> Result.get_ok

  let print ?verbose () ppf m = S.print ?verbose obj ppf m

  let zap_to_ceil m = with_log (S.zap_to_floor obj m)

  let zap_to_floor m = with_log (S.zap_to_ceil obj m)

  let of_const : type l r. ?hint:(l * r) neg Hint.const -> const -> (l * r) t =
   fun ?hint a -> S.of_const ?hint obj a

  let to_const_exn m = S.to_const_exn obj m

  let unhint = S.Unhint.unhint

  let hint ?hint = S.Unhint.hint obj ?hint

  let wrap ?hint:h f m = m |> unhint |> f |> hint ?hint:h

  let apply_hint hint m = wrap ~hint Fun.id m

  let join_const_unhint c m =
    S.Unhint.apply Obj.obj (Simple (Meet_const c)) m

  let join_const ?hint c m =
    wrap ?hint (join_const_unhint c) (disallow_left m)

  let subtract_const_unhint c m =
    S.Unhint.apply obj (Simple (Imply_const c)) m

  let subtract_const c m =
    m |> disallow_right |> wrap (subtract_const_unhint c)

  module Guts = struct
    let get_ceil m = S.get_floor obj m
  end
end
[@@inline]

module Locality = struct
  module Const = C.Locality

  module Obj = struct
    type const = Const.t

    let obj = C.Locality
  end

  include Comonadic_gen (Obj)

  let global = of_const Global

  let local = of_const Local

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor

  module Guts = struct
    let check_const m =
      let floor = Guts.get_floor m in
      let ceil = Guts.get_ceil m in
      if Const.le ceil floor then Some ceil else None

    let check_const_conservative m =
      let floor = Guts.get_loose_floor m in
      let ceil = Guts.get_loose_ceil m in
      if Const.le ceil floor then Some ceil else None
  end
end

module Regionality = struct
  module Const = C.Regionality

  module Obj = struct
    type const = Const.t

    let obj = C.Regionality
  end

  include Comonadic_gen (Obj)

  let local = of_const Const.Local

  let regional = of_const Const.Regional

  let global = of_const Const.Global

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Linearity = struct
  module Const = C.Linearity

  module Obj = struct
    type const = Const.t

    let obj : _ C.obj = C.Linearity
  end

  include Comonadic_gen (Obj)

  let many = of_const Many

  let once = of_const Once

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Statefulness = struct
  module Const = C.Statefulness

  module Obj = struct
    type const = Const.t

    let obj = C.Statefulness
  end

  include Comonadic_gen (Obj)

  let stateless = of_const Stateless

  let observing = of_const Observing

  let stateful = of_const Stateful

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module Visibility = struct
  module Const = C.Visibility

  module Obj = struct
    type const = Const.t

    let obj = C.Visibility_op
  end

  include Monadic_gen (Obj)

  let immutable = of_const Immutable

  let read = of_const Read

  let read_write = of_const Read_write

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_floor
end

module Portability = struct
  module Const = C.Portability

  module Obj = struct
    type const = Const.t

    let obj : _ C.obj = C.Portability
  end

  include Comonadic_gen (Obj)

  let legacy = of_const Const.legacy

  (* CR dkalinichenko: ideally, [observing] should zap to [sharable]. *)
  let zap_to_legacy ~statefulness =
    match statefulness with
    | Statefulness.Const.Stateful | Statefulness.Const.Observing -> zap_to_ceil
    | Statefulness.Const.Stateless -> zap_to_floor
end

module Uniqueness = struct
  module Const = C.Uniqueness

  module Obj = struct
    type const = Const.t

    let obj = C.Uniqueness_op
  end

  include Monadic_gen (Obj)

  let aliased = of_const Aliased

  let unique = of_const Unique

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module Contention = struct
  module Const = C.Contention

  module Obj = struct
    type const = Const.t

    let obj = C.Contention_op
  end

  include Monadic_gen (Obj)

  let legacy = of_const Const.legacy

  (* CR dkalinichenko: ideally, [read] should zap to [shared]. *)
  let zap_to_legacy ~visibility =
    match visibility with
    | Visibility.Const.Read_write | Visibility.Const.Read -> zap_to_floor
    | Visibility.Const.Immutable -> zap_to_ceil
end

module Forkable = struct
  module Const = C.Forkable

  module Obj = struct
    type const = Const.t

    let obj = C.Forkable
  end

  include Comonadic_gen (Obj)

  let unforkable = of_const Unforkable

  let forkable = of_const Forkable

  let legacy = of_const Const.legacy

  (* [forkable] is the default for [global]s and [unforkable] for [local]
     or [regional] values, so we vary [zap_to_legacy] accordingly. *)
  let zap_to_legacy ~global =
    match global with true -> zap_to_floor | false -> zap_to_ceil
end

module Yielding = struct
  module Const = C.Yielding

  module Obj = struct
    type const = Const.t

    let obj = C.Yielding
  end

  include Comonadic_gen (Obj)

  let yielding = of_const Yielding

  let unyielding = of_const Unyielding

  let legacy = of_const Const.legacy

  (* [unyielding] is the default for [global]s and [yielding] for [local]
     or [regional] values, so we vary [zap_to_legacy] accordingly. *)
  let zap_to_legacy ~global =
    match global with true -> zap_to_floor | false -> zap_to_ceil
end

module Staticity = struct
  module Const = C.Staticity

  type const = Const.t =
    | Static
    | Dynamic

  module Obj = struct
    type const = Const.t

    let obj = C.Staticity_op
  end

  include Monadic_gen (Obj)

  let legacy = of_const Const.legacy

  let zap_to_legacy = zap_to_ceil
end

module type Areality = sig
  module Const : C.Areality

  module Obj : Obj with type const = Const.t

  val zap_to_legacy : (Const.t, allowed * 'r) S.mode -> Const.t
end

module Lattice_Product (L : Lattice) = struct
  open L

  let min_with ax c = Axis.set ax c min

  let max_with ax c = Axis.set ax c max
end

module Comonadic_with (Areality : Areality) = struct
  module Obj = struct
    type const = Areality.Const.t C.comonadic_with

    let obj = C.comonadic_with_obj Areality.Obj.obj
  end

  include Comonadic_gen (Obj)

  module Axis = struct
    type 'a t = (Obj.const, 'a) Axis.t

    type packed = P : 'a t -> packed

    let print = Axis.print

    let compare = Axis.compare

    let proj = Axis.proj

    let all =
      [ P Areality;
        P Linearity;
        P Portability;
        P Forkable;
        P Yielding;
        P Statefulness ]
      |> List.sort
           (fun (P ax1) (P ax2) ->
             match compare ax1 ax2 with
             | Less_than -> -1
             | Equal -> 0
             | Greater_than -> 1)
  end

  let proj_obj ax = (C.proj_obj [@inlined hint]) ax Obj.obj

  module Const = struct
    include C.Comonadic_with (Areality.Const)
    include Lattice_Product (C.Comonadic_with (Areality.Const))

    let proj = Axis.proj

    module Per_axis = struct
      let print ax ppf a =
        let obj = proj_obj ax in
        C.print obj ppf a

      let le ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.le [@inlined hint]) obj a b

      let equal ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.equal [@inlined hint]) obj a b

      let join ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.join [@inlined hint]) obj a b

      let meet ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.meet [@inlined hint]) obj a b

      let max ax =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.max [@inlined hint]) obj

      let min ax =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.min [@inlined hint]) obj

      let equal_obj ax1 ax2 =
        let obj1 = proj_obj ax1 in
        let obj2 = proj_obj ax2 in
        C.equal_obj obj1 obj2

      let compare_obj ax1 ax2 =
        let obj1 = proj_obj ax1 in
        let obj2 = proj_obj ax2 in
        C.compare_obj obj1 obj2

      let print_obj ppf ax =
        let obj = proj_obj ax in
        C.print_obj ppf obj
    end
  end

  let proj ax m =
    S.apply ~hint:Skip (proj_obj ax) (Proj_and(Id, ax, Obj.obj)) m

  module Per_axis = struct
    let zap_to_floor ax m =
      let obj = proj_obj ax in
      with_log (S.zap_to_floor obj m)

    let zap_to_ceil ax m =
      let obj = proj_obj ax in
      with_log (S.zap_to_ceil obj m)
  end

  let min_with ax m =
    S.apply ~hint:Skip Obj.obj (And_min_with(ax, Id)) (disallow_right m)

  let max_with ax m =
    S.apply ~hint:Skip Obj.obj (And_max_with(ax, Id)) (disallow_left m)

  let meet_const_with ax c m =
    meet_const (C.max_with Obj.obj ax c) m

  let imply_const_with ax c m = imply_const (C.max_with Obj.obj ax c) m

  let zap_to_legacy m : Const.t =
    let areality = proj Areality m |> Areality.zap_to_legacy in
    let linearity = proj Linearity m |> Linearity.zap_to_legacy in
    let statefulness = proj Statefulness m |> Statefulness.zap_to_legacy in
    let portability =
      proj Portability m |> Portability.zap_to_legacy ~statefulness
    in
    let global = Areality.Const.equal areality Areality.Const.legacy in
    let forkable = proj Forkable m |> Forkable.zap_to_legacy ~global in
    let yielding = proj Yielding m |> Yielding.zap_to_legacy ~global in
    { areality; linearity; portability; forkable; yielding; statefulness }

  let legacy = of_const Const.legacy

  type simple_error =
    | Error : 'a Axis.t * 'a Mode_intf.simple_error -> simple_error

  let axis_of_error (actual : Obj.const) (expected : Obj.const) : simple_error =
    List.find_map
      (fun (Axis.P ax) ->
        let left = Const.proj ax actual in
        let right = Const.proj ax expected in
        if Const.Per_axis.le ax left right
        then None
        else Some (Error (ax, { left; right })))
      Axis.all
    |> Option.get

  (* overriding to report the offending axis *)
  let to_simple_error ({ left; right; _ } : error) = axis_of_error left right

  let submode_err pp a b =
    match submode ~pp a b with
    | Ok () -> ()
    | Error e ->
      let (Error (ax, _)) = to_simple_error e in
      raise (Submode_error_simple_context (pp, Proj (Obj.obj, ax, e)))

  let print_error pp err =
    let (Error (ax, _)) = to_simple_error err in
    Error.print_proj pp Obj.obj ax err
end
[@@inline]

module Monadic = struct
  (* Monadic lattices are flipped. See "Notes on flipping". *)
  module Obj = struct
    type const = C.Monadic_op.t

    let obj = C.Monadic_op
  end

  include Monadic_gen (Obj)

  module Axis = struct
    type 'a t = (Obj.const, 'a) C.Axis.t

    type packed = P : 'a t -> packed

    let compare = Axis.compare

    let print = Axis.print

    let proj = Axis.proj

    let all =
      [P Uniqueness; P Contention; P Visibility; P Staticity]
      |> List.sort
           (fun (P ax1) (P ax2) ->
             match compare ax1 ax2 with
             | Less_than -> -1
             | Equal -> 0
             | Greater_than -> 1)
  end

  let proj_obj ax = (C.proj_obj [@inlined hint]) ax Obj.obj

  module Const = struct
    include C.Monadic
    include Lattice_Product (C.Monadic)

    let proj = Axis.proj

    module Per_axis = struct
      let print ax ppf a =
        let obj = proj_obj ax in
        C.print obj ppf a

      (* See "Notes on flipping" *)

      let le ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.le [@inlined hint]) obj b a

      let equal ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.equal [@inlined hint]) obj b a

      let join ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.meet [@inlined hint]) obj a b

      let meet ax a b =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.join [@inlined hint]) obj a b

      let max ax =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.min [@inlined hint]) obj

      let min ax =
        let obj = (proj_obj [@inlined hint]) ax in
        (C.max [@inlined hint]) obj

      let equal_obj ax1 ax2 =
        let obj1 = proj_obj ax1 in
        let obj2 = proj_obj ax2 in
        C.equal_obj obj1 obj2

      let compare_obj ax1 ax2 =
        let obj1 = proj_obj ax1 in
        let obj2 = proj_obj ax2 in
        C.compare_obj obj1 obj2

      let print_obj ppf ax =
        let obj = proj_obj ax in
        C.print_obj ppf obj
    end
  end

  let proj ax m =
    S.apply ~hint:Skip (proj_obj ax) (Proj_and(Id, ax, Obj.obj)) m

  module Per_axis = struct
    let zap_to_floor ax m =
      let obj = proj_obj ax in
      with_log (S.zap_to_ceil obj m)

    let zap_to_ceil ax m =
      let obj = proj_obj ax in
      with_log (S.zap_to_floor obj m)
  end

  (* The monadic fragment is inverted. *)

  let join_const_with ax c m = join_const (C.min_with Obj.obj ax c) m

  let subtract_const_with ax c m = subtract_const (C.min_with Obj.obj ax c) m

  let min_with ax m =
    S.apply ~hint:Skip Obj.obj (And_max_with(ax, Id)) (S.disallow_left m)

  let zap_to_legacy m : Const.t =
    let uniqueness = proj Uniqueness m |> Uniqueness.zap_to_legacy in
    let visibility = proj Visibility m |> Visibility.zap_to_legacy in
    let contention =
      proj Contention m |> Contention.zap_to_legacy ~visibility
    in
    let staticity = proj Staticity m |> Staticity.zap_to_legacy in
    { uniqueness; contention; visibility; staticity }

  let legacy = of_const Const.legacy

  type simple_error =
    | Error : 'a Axis.t * 'a Mode_intf.simple_error -> simple_error

  let axis_of_error (actual : Obj.const) (expected : Obj.const) : simple_error =
    List.find_map
      (fun (Axis.P ax) ->
        let left = Const.proj ax actual in
        let right = Const.proj ax expected in
        if Const.Per_axis.le ax left right
        then None
        else Some (Error (ax, { left; right })))
      Axis.all
    |> Option.get

  let to_simple_error ({ left; right; _ } : error) =
    (* monadic fragment is flipped *)
    axis_of_error right left

  let submode_err pp a b =
    match submode ~pp a b with
    | Ok () -> ()
    | Error e ->
      let (Error (ax, _)) = to_simple_error e in
      raise (Submode_error_simple_context (pp, Proj (Obj.obj, ax, e)))

  let print_error pp err =
    let (Error (ax, _)) = to_simple_error err in
    Error.print_proj pp Obj.obj ax err
end

type ('mo, 'como) monadic_comonadic =
  { monadic : 'mo;
    comonadic : 'como
  }

module Value_with (Areality : Areality) = struct
  module Comonadic = Comonadic_with (Areality)
  module Monadic = Monadic

  type 'd t = ('d Monadic.t, 'd Comonadic.t) monadic_comonadic

  type l = (allowed * disallowed) t

  type r = (disallowed * allowed) t

  type lr = (allowed * allowed) t

  module Axis = struct
    type 'a t =
      | Comonadic : 'a Comonadic.Axis.t -> 'a t
      | Monadic : 'a Monadic.Axis.t -> 'a t

    let compare : type a b. a t -> b t -> (a, b) comparison =
     fun t1 t2 ->
       match t1, t2 with
       | Comonadic t1, Comonadic t2 -> Axis.compare t1 t2
       | Comonadic _, _ -> Less_than
       | _, Comonadic _ -> Greater_than
       | Monadic t1, Monadic t2 -> Axis.compare t1 t2

    type packed = P : 'a t -> packed

    let print (type a) ppf (t : a t) =
      match t with
      | Monadic ax -> Axis.print ppf ax
      | Comonadic ax -> Axis.print ppf ax

    let all =
      List.map (fun (Monadic.Axis.P ax) -> P (Monadic ax)) Monadic.Axis.all
      @ List.map
          (fun (Comonadic.Axis.P ax) -> P (Comonadic ax))
          Comonadic.Axis.all
      |> List.sort (fun (P ax1) (P ax2) ->
           match compare ax1 ax2 with
           | Less_than -> -1
           | Equal -> 0
           | Greater_than -> 1)
  end

  let proj_obj : type a. a Axis.t -> a C.obj = function
    | Monadic ax -> Monadic.proj_obj ax
    | Comonadic ax -> Comonadic.proj_obj ax

  (* CR-soon zqian: make a functor [Mode.Value.Const.Make] to generalize over any type
     operator applied on each mode constants. *)
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) modes =
    { areality : 'a;
      linearity : 'b;
      uniqueness : 'c;
      portability : 'd;
      contention : 'e;
      forkable : 'f;
      yielding : 'g;
      statefulness : 'h;
      visibility : 'i;
      staticity : 'j
    }

  let split
      { areality;
        linearity;
        portability;
        forkable;
        yielding;
        statefulness;
        uniqueness;
        contention;
        visibility;
        staticity
      } =
    let monadic : Monadic.Const.t =
      { uniqueness; contention; visibility; staticity }
    in
    let comonadic : Comonadic.Const.t =
      { areality; linearity; portability; forkable; yielding; statefulness }
    in
    { comonadic; monadic }

  let merge { comonadic; monadic } =
    let ({ areality; linearity; portability; forkable; yielding; statefulness }
          : Comonadic.Const.t) =
      comonadic
    in
    let ({ uniqueness; contention; visibility; staticity } : Monadic.Const.t) =
      monadic
    in
    { areality;
      linearity;
      portability;
      forkable;
      yielding;
      statefulness;
      uniqueness;
      contention;
      visibility;
      staticity
    }

  let print ?verbose () ppf { monadic; comonadic } =
    Fmt.fprintf ppf "%a;%a"
      (Comonadic.print ?verbose ())
      comonadic
      (Monadic.print ?verbose ())
      monadic

  let of_const ?hint_monadic ?hint_comonadic c =
    let { monadic; comonadic } = split c in
    let comonadic = Comonadic.of_const ?hint:hint_comonadic comonadic in
    let monadic = Monadic.of_const ?hint:hint_monadic monadic in
    { comonadic; monadic }

  let to_const_exn m =
    let { comonadic; monadic } = m in
    let comonadic = Comonadic.to_const_exn comonadic in
    let monadic = Monadic.to_const_exn monadic in
    { comonadic; monadic } |> merge

  let unhint { monadic; comonadic } =
    let comonadic = Comonadic.unhint comonadic in
    let monadic = Monadic.unhint monadic in
    { monadic; comonadic }

  let hint ?monadic ?comonadic t =
    let comonadic = Comonadic.hint ?hint:comonadic t.comonadic in
    let monadic = Monadic.hint ?hint:monadic t.monadic in
    { monadic; comonadic }

  let wrap ?monadic ?comonadic f t =
    t |> unhint |> f |> hint ?monadic ?comonadic

  module Const = struct
    (* CR-soon zqian: make a functor [Mode.Value.Const.Make] to generalize over any type
       operator applied on each mode constants. *)
    type t =
      ( Areality.Const.t,
        Linearity.Const.t,
        Uniqueness.Const.t,
        Portability.Const.t,
        Contention.Const.t,
        Forkable.Const.t,
        Yielding.Const.t,
        Statefulness.Const.t,
        Visibility.Const.t,
        Staticity.Const.t )
      modes

    let min =
      merge { comonadic = Comonadic.Const.min; monadic = Monadic.Const.min }

    let max =
      merge { comonadic = Comonadic.Const.max; monadic = Monadic.Const.max }

    let le m1 m2 =
      let m1 = split m1 in
      let m2 = split m2 in
      Comonadic.Const.le m1.comonadic m2.comonadic
      && Monadic.Const.le m1.monadic m2.monadic

    let equal m1 m2 =
      let m1 = split m1 in
      let m2 = split m2 in
      Comonadic.Const.equal m1.comonadic m2.comonadic
      && Monadic.Const.equal m1.monadic m2.monadic

    let print ppf m =
      let { monadic; comonadic } = split m in
      Fmt.fprintf ppf "%a,%a"
        Comonadic.Const.print comonadic Monadic.Const.print monadic

    let legacy =
      merge
        { comonadic = Comonadic.Const.legacy;
          monadic = Monadic.Const.legacy }

    let meet m1 m2 =
      let m1 = split m1 in
      let m2 = split m2 in
      let monadic = Monadic.Const.meet m1.monadic m2.monadic in
      let comonadic = Comonadic.Const.meet m1.comonadic m2.comonadic in
      merge { monadic; comonadic }

    let join m1 m2 =
      let m1 = split m1 in
      let m2 = split m2 in
      let monadic = Monadic.Const.join m1.monadic m2.monadic in
      let comonadic = Comonadic.Const.join m1.comonadic m2.comonadic in
      merge { monadic; comonadic }

    module Option = struct
      type some = t

      type t =
        ( Areality.Const.t option,
          Linearity.Const.t option,
          Uniqueness.Const.t option,
          Portability.Const.t option,
          Contention.Const.t option,
          Forkable.Const.t option,
          Yielding.Const.t option,
          Statefulness.Const.t option,
          Visibility.Const.t option,
          Staticity.Const.t option )
        modes

      let none =
        { areality = None;
          uniqueness = None;
          linearity = None;
          portability = None;
          contention = None;
          forkable = None;
          yielding = None;
          statefulness = None;
          visibility = None;
          staticity = None
        }

      let value opt ~default =
        let areality = Option.value opt.areality ~default:default.areality in
        let uniqueness =
          Option.value opt.uniqueness ~default:default.uniqueness
        in
        let linearity = Option.value opt.linearity ~default:default.linearity in
        let portability =
          Option.value opt.portability ~default:default.portability
        in
        let contention =
          Option.value opt.contention ~default:default.contention
        in
        let yielding = Option.value opt.yielding ~default:default.yielding in
        let forkable = Option.value opt.forkable ~default:default.forkable in
        let statefulness =
          Option.value opt.statefulness ~default:default.statefulness
        in
        let visibility =
          Option.value opt.visibility ~default:default.visibility
        in
        let staticity = Option.value opt.staticity ~default:default.staticity in
        { areality;
          uniqueness;
          linearity;
          portability;
          contention;
          forkable;
          yielding;
          statefulness;
          visibility;
          staticity
        }

      let proj (type a) (ax : a Axis.t) (t : t) : a option =
        match ax with
        | Monadic ax -> (
          match ax with
          | Uniqueness -> t.uniqueness
          | Contention -> t.contention
          | Visibility -> t.visibility
          | Staticity -> t.staticity)
        | Comonadic ax -> (
          match ax with
          | Areality -> t.areality
          | Linearity -> t.linearity
          | Portability -> t.portability
          | Forkable -> t.forkable
          | Yielding -> t.yielding
          | Statefulness -> t.statefulness)

      let set (type a) (ax : a Axis.t) (a : a option) (t : t) : t =
        match ax with
        | Monadic ax -> (
          match ax with
          | Uniqueness -> { t with uniqueness = a }
          | Contention -> { t with contention = a }
          | Visibility -> { t with visibility = a }
          | Staticity -> { t with staticity = a })
        | Comonadic ax -> (
          match ax with
          | Areality -> { t with areality = a }
          | Linearity -> { t with linearity = a }
          | Portability -> { t with portability = a }
          | Yielding -> { t with yielding = a }
          | Forkable -> { t with forkable = a }
          | Statefulness -> { t with statefulness = a })

      let print ppf
          { areality;
            uniqueness;
            linearity;
            portability;
            contention;
            forkable;
            yielding;
            statefulness;
            visibility;
            staticity
          } =
        let option_print print ppf = function
          | None -> Fmt.fprintf ppf "None"
          | Some a -> Fmt.fprintf ppf "Some %a" print a
        in
        Fmt.fprintf ppf "%a,%a,%a,%a,%a,%a,%a,%a,%a,%a"
          (option_print Areality.Const.print)
          areality
          (option_print Linearity.Const.print)
          linearity
          (option_print Uniqueness.Const.print)
          uniqueness
          (option_print Portability.Const.print)
          portability
          (option_print Contention.Const.print)
          contention
          (option_print Forkable.Const.print)
          forkable
          (option_print Yielding.Const.print)
          yielding
          (option_print Statefulness.Const.print)
          statefulness
          (option_print Visibility.Const.print)
          visibility
          (option_print Staticity.Const.print)
          staticity
    end

    let diff m1 m2 =
      let diff le a1 a2 = if le a1 a2 && le a2 a1 then None else Some a1 in
      let areality = diff Areality.Const.le m1.areality m2.areality in
      let linearity = diff Linearity.Const.le m1.linearity m2.linearity in
      let uniqueness = diff Uniqueness.Const.le m1.uniqueness m2.uniqueness in
      let portability =
        diff Portability.Const.le m1.portability m2.portability
      in
      let contention = diff Contention.Const.le m1.contention m2.contention in
      let forkable = diff Forkable.Const.le m1.forkable m2.forkable in
      let yielding = diff Yielding.Const.le m1.yielding m2.yielding in
      let statefulness =
        diff Statefulness.Const.le m1.statefulness m2.statefulness
      in
      let visibility = diff Visibility.Const.le m1.visibility m2.visibility in
      let staticity = diff Staticity.Const.le m1.staticity m2.staticity in
      { areality;
        linearity;
        uniqueness;
        portability;
        contention;
        forkable;
        yielding;
        statefulness;
        visibility;
        staticity
      }

    let comonadic_to_monadic_min =
      C.comonadic_to_monadic_min Areality.Const.areality

    let monadic_to_comonadic_min =
      C.monadic_to_comonadic_min (C.comonadic_with_obj Areality.Obj.obj)

    (** See [Alloc.close_over] for explanation. *)
    let close_over m =
      let { monadic; comonadic } = split m in
      Comonadic.Const.join comonadic
        (monadic_to_comonadic_min monadic)

    (** See [Alloc.partial_apply] for explanation. *)
    let partial_apply m =
      let { comonadic; _ } = split m in
      comonadic

    let print_axis : type a. a Axis.t -> Fmt.formatter -> a -> unit =
     fun ax ppf a ->
      let obj = proj_obj ax in
      C.print obj ppf a

    let le_axis : type a. a Axis.t -> a -> a -> bool =
     fun ax m1 m2 ->
      match ax with
      | Comonadic ax -> Comonadic.Const.Per_axis.le ax m1 m2
      | Monadic ax -> Monadic.Const.Per_axis.le ax m1 m2

    let min_axis : type a. a Axis.t -> a = function
      | Comonadic ax -> Comonadic.Const.Per_axis.min ax
      | Monadic ax -> Monadic.Const.Per_axis.min ax

    let max_axis : type a. a Axis.t -> a = function
      | Comonadic ax -> Comonadic.Const.Per_axis.max ax
      | Monadic ax -> Monadic.Const.Per_axis.max ax

    let is_max : type a. a Axis.t -> a -> bool =
     fun ax m -> le_axis ax (max_axis ax) m

    let is_min : type a. a Axis.t -> a -> bool =
     fun ax m -> le_axis ax m (min_axis ax)

    let split = split

    let merge = merge
  end

  let min = { comonadic = Comonadic.min; monadic = Monadic.min }

  let max = { comonadic = Comonadic.max; monadic = Monadic.max }

  include Magic_allow_disallow (struct
    type (_, _, 'd) sided = 'd t

    let allow_left { monadic; comonadic } =
      let monadic = Monadic.allow_left monadic in
      let comonadic = Comonadic.allow_left comonadic in
      { monadic; comonadic }

    let allow_right { monadic; comonadic } =
      let monadic = Monadic.allow_right monadic in
      let comonadic = Comonadic.allow_right comonadic in
      { monadic; comonadic }

    let disallow_left { monadic; comonadic } =
      let monadic = Monadic.disallow_left monadic in
      let comonadic = Comonadic.disallow_left comonadic in
      { monadic; comonadic }

    let disallow_right { monadic; comonadic } =
      let monadic = Monadic.disallow_right monadic in
      let comonadic = Comonadic.disallow_right comonadic in
      { monadic; comonadic }
  end)

  let newvar () =
    let comonadic = Comonadic.newvar () in
    let monadic = Monadic.newvar () in
    { comonadic; monadic }

  let newvar_above { comonadic; monadic } =
    let comonadic, b1 = Comonadic.newvar_above comonadic in
    let monadic, b2 = Monadic.newvar_above monadic in
    { monadic; comonadic }, b1 || b2

  let newvar_below { comonadic; monadic } =
    let comonadic, b1 = Comonadic.newvar_below comonadic in
    let monadic, b2 = Monadic.newvar_below monadic in
    { monadic; comonadic }, b1 || b2

  type atom = Atom : 'a Axis.t * 'a -> atom

  type error =
    | Monadic of Monadic.error
    | Comonadic of Comonadic.error

  type equate_error = equate_step * error

  type simple_error =
    | Error : 'a Axis.t * 'a Mode_intf.simple_error -> simple_error

  let to_simple_error = function
    | Monadic e ->
      let (Error (ax, e)) = Monadic.to_simple_error e in
      Error (Monadic ax, e)
    | Comonadic e ->
      let (Error (ax, e)) = Comonadic.to_simple_error e in
      Error (Comonadic ax, e)

  let print_error pp = function
    | Monadic e -> Monadic.print_error pp e
    | Comonadic e -> Comonadic.print_error pp e

  let submode_log ?pp { monadic = monadic1; comonadic = comonadic1 }
      { monadic = monadic2; comonadic = comonadic2 } ~log : (_, error) result =
    (* comonadic before monadic, so that locality errors dominate
       (error message backward compatibility) *)
    match Comonadic.submode_log ?pp comonadic1 comonadic2 ~log with
    | Error e -> Error (Comonadic e)
    | Ok () -> (
      match Monadic.submode_log ?pp monadic1 monadic2 ~log with
      | Error e -> Error (Monadic e)
      | Ok () -> Ok ())

  let submode ?pp a b = try_with_log (submode_log ?pp a b)

  let submode_err pp a b =
    Comonadic.submode_err pp a.comonadic b.comonadic;
    Monadic.submode_err pp a.monadic b.monadic

  let equate a b = try_with_log (equate_from_submode (submode_log ?pp:None) a b)

  let submode_exn ?pp m1 m2 =
    match submode ?pp m1 m2 with
    | Ok () -> ()
    | Error _ -> invalid_arg "submode_exn"

  let equate_exn m1 m2 =
    match equate m1 m2 with Ok () -> () | Error _ -> invalid_arg "equate_exn"

  let legacy =
    let comonadic = Comonadic.legacy in
    let monadic = Monadic.legacy in
    { comonadic; monadic }

  let proj_monadic ax { monadic; _ } = Monadic.proj ax monadic

  let proj_comonadic ax { comonadic; _ } = Comonadic.proj ax comonadic

  let max_with_comonadic ax m =
    let comonadic = Comonadic.max_with ax m in
    let monadic = Monadic.max |> Monadic.disallow_left |> Monadic.allow_right in
    { comonadic; monadic }

  let min_with_comonadic ax m =
    let comonadic = Comonadic.min_with ax m in
    let monadic = Monadic.min |> Monadic.disallow_right |> Monadic.allow_left in
    { comonadic; monadic }

  let min_with_monadic ax m =
    let monadic = Monadic.min_with ax m in
    let comonadic =
      Comonadic.min |> Comonadic.disallow_right |> Comonadic.allow_left
    in
    { comonadic; monadic }

  let join_const_with ax c { monadic; comonadic } =
    let monadic = Monadic.join_const_with ax c monadic in
    let comonadic = Comonadic.disallow_left comonadic in
    { monadic; comonadic }

  let meet_const_with ax c { monadic; comonadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_const_with ax c comonadic in
    { comonadic; monadic }

  let imply_const_with ax c { monadic; comonadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.imply_const_with ax c comonadic in
    { monadic; comonadic }

  let subtract_const_with ax c { monadic; comonadic } =
    let monadic = Monadic.subtract_const_with ax c monadic in
    let comonadic = Comonadic.disallow_right comonadic in
    { comonadic; monadic }

  let join l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.join como in
    let monadic = Monadic.join mo in
    { comonadic; monadic }

  let meet l =
    let como, mo =
      List.fold_left
        (fun (como, mo) { comonadic; monadic } ->
          comonadic :: como, monadic :: mo)
        ([], []) l
    in
    let comonadic = Comonadic.meet como in
    let monadic = Monadic.meet mo in
    { comonadic; monadic }

  let comonadic_to_monadic_min ?hint m =
    S.apply ?hint Monadic.Obj.obj
      (Simple (Core (Comonadic_to_monadic_max Areality.Const.areality)))
      (Comonadic.disallow_left m)

  let monadic_to_comonadic_min m =
    S.apply Comonadic.Obj.obj
      (Simple (Core Monadic_to_comonadic_min))
      (Monadic.disallow_left m)

  let monadic_to_comonadic_max m =
    S.apply Comonadic.Obj.obj
      (Simple (Core Monadic_to_comonadic_max))
      (Monadic.disallow_right m)

  let meet_const c { comonadic; monadic } =
    let monadic = Monadic.disallow_right monadic in
    let comonadic = Comonadic.meet_const c comonadic in
    { monadic; comonadic }

  let join_const c { comonadic; monadic } =
    let monadic = Monadic.join_const c monadic in
    let comonadic = Comonadic.disallow_left comonadic in
    { monadic; comonadic }

  let imply_const c { comonadic; monadic } =
    let monadic = Monadic.disallow_left monadic in
    let comonadic = Comonadic.imply_const c comonadic in
    { monadic; comonadic }

  let subtract_const c { comonadic; monadic } =
    let monadic = Monadic.subtract_const c monadic in
    let comonadic = Comonadic.disallow_right comonadic in
    { monadic; comonadic }

  let zap_to_ceil { comonadic; monadic } =
    let monadic = Monadic.zap_to_ceil monadic in
    let comonadic = Comonadic.zap_to_ceil comonadic in
    merge { monadic; comonadic }

  let zap_to_floor { comonadic; monadic } =
    let monadic = Monadic.zap_to_floor monadic in
    let comonadic = Comonadic.zap_to_floor comonadic in
    merge { monadic; comonadic }

  let zap_to_legacy { comonadic; monadic } =
    let monadic = Monadic.zap_to_legacy monadic in
    let comonadic = Comonadic.zap_to_legacy comonadic in
    merge { monadic; comonadic }

  (** This is about partially applying [A -> B -> C] to [A] and getting
      [B -> C]. [comonadic] and [monadic] constutute the mode of [A], and we
      need to give the lower bound mode of [B -> C]. *)
  let close_over { comonadic; monadic } =
    let comonadic = Comonadic.disallow_right comonadic in
    (* The comonadic of the returned function is constrained by the monadic of the closed argument via the dualizing morphism. *)
    let comonadic_dual = monadic_to_comonadic_min monadic in
    (* It's also constrained by the comonadic of the closed argument. *)
    let comonadic = Comonadic.join [comonadic; comonadic_dual] in
    (* The closure will access [A] at the specified monadic modes, and thus the
       monadic mode of the closure itself is not constrained by it. *)
    let monadic = Monadic.disallow_right Monadic.min in
    { comonadic; monadic }

  (** Similar to above, but we are given the mode of [A -> B -> C], and need to
      give the lower bound mode of [B -> C]. *)
  let partial_apply { comonadic; _ } =
    (* The closure will invoke the original function at the specified monadic
       modes, and thus the monadic mode of the closure itself is not constrained by
       it. *)
    let monadic = Monadic.disallow_right Monadic.min in
    let comonadic = Comonadic.disallow_right comonadic in
    { comonadic; monadic }

  module List = struct
    type nonrec 'd t = 'd t list

    include Magic_allow_disallow (struct
      type (_, _, 'd) sided = 'd t

      let allow_left l = List.map allow_left l

      let allow_right l = List.map allow_right l

      let disallow_left l = List.map disallow_left l

      let disallow_right l = List.map disallow_right l
    end)
  end
end
[@@inline]

module Value = Value_with (Regionality)
module Alloc = Value_with (Locality)

module Const = struct
  let locality_as_regionality = C.locality_as_regionality

  let alloc_as_value
      ({ areality;
         linearity;
         portability;
         uniqueness;
         contention;
         forkable;
         yielding;
         statefulness;
         visibility;
         staticity
       } :
        Alloc.Const.t) : Value.Const.t =
    let areality = C.locality_as_regionality areality in
    { areality;
      linearity;
      portability;
      uniqueness;
      contention;
      forkable;
      yielding;
      statefulness;
      visibility;
      staticity
    }

  module Axis = struct
    let is_areality (type a) :
        a Alloc.Axis.t ->
        ((a, Locality.Const.t) Misc.eq, a Value.Axis.t) Either.t = function
      | Comonadic Areality -> Left Refl
      | Comonadic Linearity -> Right (Comonadic Linearity)
      | Comonadic Portability -> Right (Comonadic Portability)
      | Comonadic Forkable -> Right (Comonadic Forkable)
      | Comonadic Yielding -> Right (Comonadic Yielding)
      | Comonadic Statefulness -> Right (Comonadic Statefulness)
      | Monadic Uniqueness -> Right (Monadic Uniqueness)
      | Monadic Contention -> Right (Monadic Contention)
      | Monadic Visibility -> Right (Monadic Visibility)
      | Monadic Staticity -> Right (Monadic Staticity)

    let alloc_as_value : Alloc.Axis.packed -> Value.Axis.packed =
     fun (P ax) ->
      match is_areality ax with
      | Left Refl -> P (Comonadic Areality)
      | Right ax -> P ax
  end

end

(* CR-someday zqian: all the function that converts between [Alloc] and [Value] should
   operate on [Unhint] so they can be composed and assigned hint as a whole. *)

let comonadic_locality_as_regionality comonadic =
  S.Unhint.apply Value.Comonadic.Obj.obj
    (Simple (Core (Locality_full Locality_as_regionality)))
    comonadic

let comonadic_regional_to_local comonadic =
  S.Unhint.apply Alloc.Comonadic.Obj.obj
    (Simple (Core (Locality_full Regional_to_local)))
    comonadic

let locality_as_regionality_unhint l =
  S.Unhint.apply C.Regionality
    (Simple (Core (Locality_restricted Locality_as_regionality)))
    l

let locality_as_regionality m =
  m
  |> Locality.unhint
  |> locality_as_regionality_unhint
  |> Regionality.hint

let alloc_as_value_unhint m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Unhint.apply Value.Comonadic.Obj.obj
      (Simple (Core (Locality_full Locality_as_regionality)))
      comonadic
  in
  { comonadic; monadic }

let alloc_as_value m =
  m |> Alloc.unhint |> alloc_as_value_unhint |> Value.hint ~monadic:Skip

let alloc_to_value_l2r_unhint m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Unhint.apply Value.Comonadic.Obj.obj
      (Simple (Core (Locality_full Local_to_regional)))
      comonadic
  in
  { comonadic; monadic }

let alloc_to_value_l2r m =
  m |> Alloc.disallow_right |> Alloc.unhint |> alloc_to_value_l2r_unhint
  |> Value.hint ~monadic:Skip

let value_to_alloc_r2g_unhint m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Unhint.apply Alloc.Comonadic.Obj.obj
      (Simple (Core (Locality_full Regional_to_global)))
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2g m =
  m |> Value.disallow_left |> Value.unhint |> value_to_alloc_r2g_unhint
  |> Alloc.hint ~monadic:Skip

let value_r2g ?hint m =
  Value.wrap ~monadic:Skip ?comonadic:hint
    (fun m -> m |> value_to_alloc_r2g_unhint |> alloc_as_value_unhint)
    (Value.disallow_left m)

let value_to_alloc_r2l_unhint m =
  let { comonadic; monadic } = m in
  let comonadic =
    S.Unhint.apply Alloc.Comonadic.Obj.obj
      (Simple (Core (Locality_full Regional_to_local)))
      comonadic
  in
  { comonadic; monadic }

let value_to_alloc_r2l m =
  m |> Value.unhint |> value_to_alloc_r2l_unhint |> Alloc.hint ~monadic:Skip

module Modality = struct
  (* Inferred modalities

      Similar to constant modalities, an inferred modality maps the mode of a
      record/structure to the mode of a value therein. An inferred modality [f]
      is inferred from the structure/record mode [mm] and the value mode [m]. It
      will only be applied on some [x >= mm]: That is, it will only be applied
      on the original module.

      It should satisfy the following conditions:

      Zapping: [f] should be of the form [join_c] for monadic axes, or [meet_c]
      for comonadic axes.

      Soundness: You should not get a value from a record/structure at a mode
      strictly stronger than how it was put in. That is, for any [x >= mm], [f x
      >= m].

      Completeness: Ideally we also want [f mm <= m].

      Monadic axes

      Soundness condition says [join_c x >= m] for any [x >= mm]. Equivalently,
      [join_c mm >= m]. By adjunction, [c >= subtract_mm m]. We take the lower
      bound [c := subtract_mm m]. Note that this is equivalent to taking [c := m
      >= subtract_mm m]. Proof:

      - [join_m x >= join_(subtract_mm m) x] is trivial since [m >= subtract_mm
        m].
      - [join_m x <= join_(subtract_mm m) x], or equivalently [m <=
      join_(subtract_mm m) x], or equivalently [subtract_x m <= subtract_mm m],
      which is trivial since [x >= mm].

      Taking [c := subtract_mm m] is better for zapping since it's lower and
      thus closer to identity modality. Taking [c := m] is easier for [apply]
      and [sub].

      Comonadic axes

      Soundness condition says [meet_c x >= m] for any [x >= mm]. Equivalently,
      [meet_c mm >= m]. By def. of [meet], we have both [c >= m] and [mm >= m].
      The latter is guaranteed by the user of [infer]. We guarantee the former
      by taking [c := imply_mm m >= m]. One might worry that this is too relaxed
      and will be "less complete" than taking [c := m]; however, note that
      [imply_mm m <= imply_mm m] and thus by adjunction [meet_(imply_mm m) mm <=
      m], which means the chosen [c] is complete.

      Taking [c := m] is easier for [apply] and [sub]. Taking [c := imply_mm m]
      is better for zapping since it's higher and thus closer to identity
      modality. However, note that we DON'T have [meet_m x = meet_(imply_mm m)
      x], which means [apply/sub] and [zap] might behave in a confusing (albeit
      sound) manner.

      CR zqian: once we support binary mode solver, [c := imply_mm m] will be
      used uniformly by [apply] [sub] and [zap].
  *)

  module Monadic = struct
    module Mode = Value.Monadic

    type 'a axis = 'a Mode.Axis.t

    module Atom = struct
      type 'a t = Join_const of 'a [@@unboxed]

      let is_id ax (Join_const c) = Mode.Const.Per_axis.(le ax c (min ax))

      let is_constant ax (Join_const c) = Mode.Const.Per_axis.(le ax (max ax) c)
    end

    type error = Error : 'a axis * 'a Atom.t simple_error -> error

    module Const = struct
      type t = Join_const of Mode.Const.t [@@unboxed]

      let id = Join_const Mode.Const.min

      let is_id t = t = id

      let max = Join_const Mode.Const.max

      let sub left right : (_, error) Result.t =
        match left, right with
        | Join_const c1, Join_const c2 ->
          if Mode.Const.le c1 c2
          then Ok ()
          else
            let (Error (ax, { left; right })) = Mode.axis_of_error c1 c2 in
            Error
              (Error (ax, { left = Join_const left; right = Join_const right }))

      let concat ~then_ t =
        match then_, t with
        | Join_const c1, Join_const c2 -> Join_const (Mode.Const.join c1 c2)

      let apply_right : type l. ?hint:left_only Hint.morph ->
        t -> (l * allowed) Mode.t -> Mode.r =
        fun ?(hint = Hint.Unknown_non_rigid) t x ->
          match t with
          | Join_const c ->
              Mode.join_const ~hint c (Mode.disallow_left x)

      let apply_left : type r. ?hint:(allowed * r) neg Hint.const ->
        t -> (allowed * r) Mode.t -> Mode.l =
        fun ?(hint = (Hint.Unknown : _ Hint.const)) t x ->
           match t with
           | Join_const c ->
               Mode.join
                 [Mode.disallow_right (Mode.of_const ~hint c);
                  Mode.disallow_right x]

      let proj ax (Join_const c) : _ Atom.t = Join_const (Axis.proj ax c)

      let set ax (Join_const a : _ Atom.t) (Join_const c) =
        Join_const (Axis.set ax a c)

      let print ppf = function
        | Join_const c -> Fmt.fprintf ppf "join_const(%a)" Mode.Const.print c
    end

    type t =
      | Const of Const.t
      | Diff of Mode.lr * Mode.lr  (** See "Inferred modalities" comments *)
      | Undefined

    let sub_log left right ~log : (unit, error) Result.t =
      match left, right with
      | Const c1, Const c2 -> Const.sub c1 c2
      | Diff (mm, m), Const (Join_const c) -> (
        (* Check that for any x >= mm, join(x, m) <= join(x, c), which (by
           definition of join) is equivalent to m <= join(x, c). This has to
           hold for all x >= mm, so we check m <= join(mm, c). *)
        match Mode.submode_log m
                (Mode.join_const c (Mode.disallow_left mm)) ~log with
        | Ok () -> Ok ()
        | Error err ->
          let (Error (ax, { left; _ })) = Mode.to_simple_error err in
          Error
            (Error
               ( ax,
                 { left = Join_const left; right = Join_const (Axis.proj ax c) }
               )))
      | Diff (_, _m1), Diff (_, _m2) ->
        (* [m1] is a left mode so it cannot appear on the right. So we can't do
           a proper check. However, this branch is only hit by
           [wrap_constraint_with_shape], in which case LHS and RHS should be
           physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Diff _ ->
        Misc.fatal_error
          "inferred modality Diff should not be on the RHS of sub."
      | Undefined, _ | _, Undefined ->
        Misc.fatal_error "modality Undefined should not be in sub."

    let apply_left : type r.
        ?hint:(allowed * r) neg Hint.const ->
        t ->
        (allowed * r) Mode.t ->
        Mode.l =
     fun ?hint t x ->
      match t with
      | Const c -> Const.apply_left ?hint c x |> Mode.disallow_right
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be applied."
      | Diff (_, m) -> Mode.join [Mode.allow_right m; x]

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Fmt.fprintf ppf "undefined"
      | Diff _ -> Fmt.fprintf ppf "diff"

    (* All zapping functions mutate [mm] and [m] to the degree that's sufficient
       to fix [subtract_mm m], and return it. [subtract] is antitone for [mm]
       and monotone for [m]. *)

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Diff (mm, m) ->
        (* Ideally we will take [c = subtract_mm m] and zap it to floor.
           However, [subtract] requires [mm] to be constant. We get the ceil of
           [mm] to construct the floor of [c]. *)
        let cc = Mode.Guts.get_ceil mm in
        let c = Mode.subtract_const cc m in
        let c = Mode.zap_to_floor c in
        (* Note that we did not mutate [mm] but simply took its ceil, which
           might be mutated later. To satisfy the coherence condition (see the
           comment in the mli), we want to:

           - make it impossible that [subtract_mm m < c], which is trivial since
           [mm <= cc] and thus [subtract_mm m >= subtract_cc m = c].
           - make it impossible that [subtract_mm m > c], which is to ensure
           [subtract_mm m <= c], equivalently [m <= join_mm c], which is
           achieved by the following [submode].
        *)
        Mode.submode_exn m (Mode.join_const c (Mode.disallow_left mm));
        Const.Join_const c

    let zap_to_id = zap_to_floor

    let to_const_opt = function
      | Const c -> Some c
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be looked at"
      | Diff _ -> None

    let of_const c = Const c

    let infer ~md_mode ~mode = Diff (md_mode, mode)

    let max = Const Const.max
  end

  module Comonadic = struct
    module Mode = Value.Comonadic

    type 'a axis = 'a Mode.Axis.t

    module Atom = struct
      type 'a t = Meet_const of 'a [@@unboxed]

      let is_id ax (Meet_const c) = Mode.Const.Per_axis.(le ax (max ax) c)

      let is_constant ax (Meet_const c) = Mode.Const.Per_axis.(le ax c (min ax))
    end

    type error = Error : 'a axis * 'a Atom.t simple_error -> error

    module Const = struct
      type t = Meet_const of Mode.Const.t [@@unboxed]

      let id = Meet_const Mode.Const.max

      let is_id t = t = id

      let max = Meet_const Mode.Const.max

      let sub left right : (_, error) Result.t =
        match left, right with
        | Meet_const c1, Meet_const c2 ->
          if Mode.Const.le c1 c2
          then Ok ()
          else
            let (Error (ax, { left; right })) = Mode.axis_of_error c1 c2 in
            Error
              (Error (ax, { left = Meet_const left; right = Meet_const right }))

      let concat ~then_ t =
        match then_, t with
        | Meet_const c1, Meet_const c2 -> Meet_const (Mode.Const.meet c1 c2)

      let apply_left : type r.
          ?hint:left_only Hint.morph -> t -> (allowed * r) Mode.t -> Mode.l
          =
       fun ?(hint = Hint.Unknown_non_rigid) t x ->
        match t with
        | Meet_const c -> Mode.meet_const ~hint c (Mode.disallow_right x)

      let apply_right : type l. ?hint:(l * allowed) pos Hint.const
        -> t -> (l * allowed) Mode.t -> Mode.r =
       fun ?(hint = (Hint.Unknown : _ Hint.const)) t x ->
        match t with
        | Meet_const c ->
            Mode.meet
              [Mode.disallow_left (Mode.of_const ~hint c);
               Mode.disallow_left x]

      let proj ax (Meet_const c) : _ Atom.t = Meet_const (Axis.proj ax c)

      let set ax (Meet_const a : _ Atom.t) (Meet_const c) =
        Meet_const (Axis.set ax a c)

      let print ppf = function
        | Meet_const c -> Fmt.fprintf ppf "meet_const(%a)" Mode.Const.print c
    end

    type t =
      | Const of Const.t
      | Undefined
      | Exactly of Mode.lr * Mode.lr  (** See "Inferred modalities" comments *)

    let sub_log left right ~log : (unit, error) Result.t =
      match left, right with
      | Const c1, Const c2 -> Const.sub c1 c2
      | Exactly (_mm, m), Const (Meet_const c) -> (
        (* Check for all [x >= mm], [meet_(imply_mm m) x <= meet_c x], or
           equivalently [meet_(imply_mm m) x <= c], or equivalently [meet_(imply_mm
           m) max <= c], or equivalently [imply_mm m <= c]. We can't check this
           without binary mode solver.

           So instead we check [meet_m x <= meet_c x] (See "Inferred modalities"
           comments), which amounts to [m <= c]. *)
        match Mode.submode_log m (Mode.of_const c) ~log with
        | Ok () -> Ok ()
        | Error err ->
          let (Error (ax, { left; _ })) = Mode.to_simple_error err in
          Error
            (Error
               ( ax,
                 { left = Meet_const left; right = Meet_const (Axis.proj ax c) }
               )))
      | Exactly (_, _m1), Exactly (_, _m2) ->
        (* [m1] is a left mode, so there is no good way to check.
           However, this branch only hit by [wrap_constraint_with_shape],
           in which case LHS and RHS should be physically equal. *)
        assert (left == right);
        Ok ()
      | Const _, Exactly _ ->
        Misc.fatal_error
          "inferred modaltiy Exactly should not be on the RHS of sub."
      | Undefined, _ | _, Undefined ->
        Misc.fatal_error "modality Undefined should not be in sub."

    let apply_left : type r.
        ?hint:left_only Hint.morph ->
        t ->
        (allowed * r) Mode.t ->
        Mode.l =
     fun ?hint t x ->
      match t with
      | Const c -> Const.apply_left ?hint c x |> Mode.disallow_right
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be applied."
      | Exactly (_mm, m) ->
        (* Ideally want to return [meet_(imply_mm m) x], which we can't do
           without binary mode solver, so instead we return [meet_m x] (See
           "Inferred modalities" comments), which because of [x >= mm >= m] is
           equal to [m]. *)
        Mode.disallow_right m

    let print ppf = function
      | Const c -> Const.print ppf c
      | Undefined -> Fmt.fprintf ppf "undefined"
      | Exactly _ -> Fmt.fprintf ppf "exactly"

    let infer ~md_mode ~mode = Exactly (md_mode, mode)

    let max = Const Const.max

    (* All zapping functions mutate [mm] and [m] to the degree that's sufficient
       to fix [imply_mm m], and return it. [imply] is antitone for [mm] and
       monotone for [m]. *)

    let zap_to_ceil = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Exactly (mm, m) ->
        (* Ideally we will take [c = imply_mm m] and zap it to ceil. However,
           [imply] requires [mm] to be constant. We get the floor of [mm] to
           construct the ceil of [c]. *)
        let cc = Mode.Guts.get_floor mm in
        let c = Mode.imply_const cc m in
        let c = Mode.zap_to_ceil c in
        (* Note that we did not mutate [mm] but simply took its floor, which
           might be mutated later. To satisfy the coherence condition (see the
           comment in the mli), we want to:

           - make it impossible that [imply_mm m > c], which is trivial since
           [mm >= cc] and thus [imply_mm m <= imply_cc m = c].
           - make it impossible that [imply_mm m < c], which is to ensure
           [imply_mm m >= c], equivalently [m >= meet_mm c], which is achieved
           by the following [submode].
        *)
        Mode.submode_exn (Mode.meet_const c (Mode.disallow_right mm)) m;
        Const.Meet_const c

    let zap_to_id = zap_to_ceil

    let zap_to_floor = function
      | Const c -> c
      | Undefined -> Misc.fatal_error "modality Undefined should not be zapped."
      | Exactly (mm, m) ->
        (* The following zaps [mm] to ceil, which might conflict with future
           mode constraints on [mm]. We find constraining [mm] to [legacy] a
           good workaround. *)
        (* CR zqian: Find a better solution *)
        Mode.submode mm Mode.legacy |> ignore;
        let m = Mode.zap_to_floor m in
        let mm = Mode.zap_to_ceil mm in
        let c = Mode.Const.imply mm m in
        Const.Meet_const c

    let to_const_opt = function
      | Const c -> Some c
      | Undefined ->
        Misc.fatal_error "modality Undefined should not be looked at"
      | Exactly _ -> None

    let of_const c = Const c
  end

  module Axis = struct
    type 'a t =
      | Monadic : 'a Monadic.axis -> 'a Monadic.Atom.t t
      | Comonadic : 'a Comonadic.axis -> 'a Comonadic.Atom.t t

    type packed = P : 'a t -> packed

    let of_value : Value.Axis.packed -> packed = function
      | P (Monadic ax) -> P (Monadic ax)
      | P (Comonadic ax) -> P (Comonadic ax)

    let to_value : packed -> Value.Axis.packed = function
      | P (Monadic ax) -> P (Monadic ax)
      | P (Comonadic ax) -> P (Comonadic ax)
  end

  type atom = Atom : 'a Axis.t * 'a -> atom

  module Per_axis = struct
    open struct
      module Monadic = Monadic.Atom
      module Comonadic = Comonadic.Atom
    end

    let is_id : type a. a Axis.t -> a -> bool = function
      | Monadic ax -> Monadic.is_id ax
      | Comonadic ax -> Comonadic.is_id ax

    let is_constant : type a. a Axis.t -> a -> bool = function
      | Monadic ax -> Monadic.is_constant ax
      | Comonadic ax -> Comonadic.is_constant ax

    let print (type a) (ax : a Axis.t) ppf (t : a) =
      match ax, t with
      | Comonadic ax, Meet_const t ->
        Value.Comonadic.Const.Per_axis.print ax ppf t
      | Monadic ax, Join_const t -> Value.Monadic.Const.Per_axis.print ax ppf t
  end

  type error = Error : 'a Axis.t * 'a simple_error -> error

  type equate_error = equate_step * error

  module Const = struct
    module Monadic = Monadic.Const
    module Comonadic = Comonadic.Const

    type t = (Monadic.t, Comonadic.t) monadic_comonadic

    let id = { monadic = Monadic.id; comonadic = Comonadic.id }

    let is_id { monadic; comonadic } =
      Monadic.is_id monadic && Comonadic.is_id comonadic

    let sub t1 t2 : (unit, error) Result.t =
      match Monadic.sub t1.monadic t2.monadic with
      | Error (Error (ax, e)) -> Error (Error (Monadic ax, e))
      | Ok () -> (
        match Comonadic.sub t1.comonadic t2.comonadic with
        | Ok () -> Ok ()
        | Error (Error (ax, e)) -> Error (Error (Comonadic ax, e)))

    let equate = equate_from_submode' sub

    let apply_left ?hint t { monadic; comonadic } =
      let monadic =
        Monadic.apply_left
          ?hint:(Option.map (fun { monadic; _ } -> monadic) hint)
          t.monadic monadic
      in
      let comonadic =
        Comonadic.apply_left
          ?hint:(Option.map (fun { comonadic; _ } -> comonadic) hint)
          t.comonadic comonadic
      in
      { monadic; comonadic }

    let apply_right ?hint t { monadic; comonadic } =
      let monadic =
        Monadic.apply_right
          ?hint:(Option.map (fun { monadic; _ } -> monadic) hint)
          t.monadic monadic
      in
      let comonadic =
        Comonadic.apply_right
          ?hint:(Option.map (fun { comonadic; _ } -> comonadic) hint)
          t.comonadic comonadic
      in
      { monadic; comonadic }

    let concat ~then_ t =
      let monadic = Monadic.concat ~then_:then_.monadic t.monadic in
      let comonadic = Comonadic.concat ~then_:then_.comonadic t.comonadic in
      { monadic; comonadic }

    let proj (type a) (ax : a Axis.t) { monadic; comonadic } : a =
      match ax with
      | Monadic ax -> Monadic.proj ax monadic
      | Comonadic ax -> Comonadic.proj ax comonadic

    let set (type a) (ax : a Axis.t) (a : a) { monadic; comonadic } : t =
      match ax with
      | Monadic ax -> { monadic = Monadic.set ax a monadic; comonadic }
      | Comonadic ax -> { monadic; comonadic = Comonadic.set ax a comonadic }

    let diff t1 t2 =
      List.filter_map
        (fun ax : atom option ->
          let (P ax) = Axis.of_value ax in
          let a1 = proj ax t1 in
          let a2 = proj ax t2 in
          if a1 = a2 then None else Some (Atom (ax, a2)))
        Value.Axis.all

    let print ppf { monadic; comonadic } =
      Fmt.fprintf ppf "%a;%a" Monadic.print monadic Comonadic.print comonadic
  end

  type t = (Monadic.t, Comonadic.t) monadic_comonadic

  let undefined : t = { monadic = Undefined; comonadic = Undefined }

  let is_undefined : t -> bool = function
    | { monadic = Undefined; comonadic = Undefined } -> true
    | _ -> false
  [@@ocaml.warning "-4"]

  let apply_left ?hint t { monadic; comonadic } =
    let monadic =
      Monadic.apply_left
        ?hint:(Option.map (fun { monadic; _ } -> monadic) hint)
        t.monadic monadic
    in
    let comonadic =
      Comonadic.apply_left
        ?hint:(Option.map (fun { comonadic; _ } -> comonadic) hint)
        t.comonadic comonadic
    in
    { monadic; comonadic }

  let sub_log t1 t2 ~log : (unit, error) Result.t =
    match Monadic.sub_log t1.monadic t2.monadic ~log with
    | Error (Error (ax, e)) -> Error (Error (Monadic ax, e))
    | Ok () -> (
      match Comonadic.sub_log t1.comonadic t2.comonadic ~log with
      | Ok () -> Ok ()
      | Error (Error (ax, e)) -> Error (Error (Comonadic ax, e)))

  let sub l r = try_with_log (sub_log l r)

  let equate m1 m2 = try_with_log (equate_from_submode sub_log m1 m2)

  let print ppf ({ monadic; comonadic } : t) =
    Fmt.fprintf ppf "%a;%a" Monadic.print monadic Comonadic.print comonadic

  let infer ~md_mode ~mode : t =
    let comonadic =
      Comonadic.infer ~md_mode:md_mode.comonadic ~mode:mode.comonadic
    in
    let monadic = Monadic.infer ~md_mode:md_mode.monadic ~mode:mode.monadic in
    { monadic; comonadic }

  let zap_to_id t =
    let { monadic; comonadic } = t in
    let comonadic = Comonadic.zap_to_id comonadic in
    let monadic = Monadic.zap_to_id monadic in
    { monadic; comonadic }

  let zap_to_floor t =
    let { monadic; comonadic } = t in
    let comonadic = Comonadic.zap_to_floor comonadic in
    let monadic = Monadic.zap_to_floor monadic in
    { monadic; comonadic }

  let to_const_opt t =
    let { monadic; comonadic } = t in
    Option.bind (Comonadic.to_const_opt comonadic) (fun comonadic ->
        Option.bind (Monadic.to_const_opt monadic) (fun monadic ->
            Some { monadic; comonadic }))

  let to_const_exn t = t |> to_const_opt |> Option.get

  let of_const { monadic; comonadic } =
    let comonadic = Comonadic.of_const comonadic in
    let monadic = Monadic.of_const monadic in
    { monadic; comonadic }

  let max =
    let monadic = Monadic.max in
    let comonadic = Comonadic.max in
    { monadic; comonadic }
end

module Crossing = struct
  (* The mode crossing capability of a type [t] is characterized by a monotone
     function [f] from modes to some lattice [L], in the following way:

     To check [e : t @ m1 <= m2], we should instead check [f m1 <= f m2] to
     allow more programs.

     For example, if [f] is the identity function, then [t] does not cross modes
     at all. If [f] maps to the unit lattice (containing only one element), [f
     m1 <= f m2] always succeeds, which means [t] crosses modes fully.

     In practice, during mode checking we usually have either [m1] or [m2], but
     not both. In order to perform mode crossing one-sided, we require [f] to
     have left adjoint [fl] and right adjoint [fr], which gives:

     [f m1 <= f m2] is equivalent to [fl (f m1) <= m2] is equivalent to [m1 <=
     fr (f m2)]

     Therefore, we can perform any of the following for mode crossing:
     - Apply [f] on both [m1] and [m2]
     - Apply [fl ∘ f] on [m1]
     - Apply [fr ∘ f] on [m2]

     Mode crossing forms a lattice: [f1 <= f2] iff [f1] allows more mode
     crossing than [f2]. Concretely:

     [f1 <= f2] iff, for any [m1, m2], if [f2 m1 <= f2 m2],
     then [f1 m1 <= f1 m2].
  *)

  module Monadic = struct
    module Modality = Modality.Monadic
    module Mode = Value.Monadic

    module Atom = struct
      type 'a t = Modality of 'a Modality.Atom.t [@@unboxed]

      (* By the ordering of crossings (see comments above) [join_c1 <= join_c2]
         iff the following holds:
         For all [a,b], if [join_c2 a <= join_c2 b](E1), then [join_c1 a <=
         join_c1 b](E2)

         Case analysis by the relation between [c1] and [c2]:
         - If [c1 >= c2], then [c1] can be written as [join c2 k] for some [k].
           Then apply [join k] to E1 and we get E2 (by monotonicity of join).
         - If [c1 <= c2], take [a := c2] and [b := c1]. E1 holds but E2 doesn't.
         - If neither, then we take [a := c2] and [b := meet a c1]. E1 is
           satisfied:
           [join_c2 a = c2 <= c2 = join_c2 (meet a c1) = join_c2 b]. But E2 is
           not satisfied:
           [join_c1 a = join c1 c2 </= c1 = join_c1 (meet c1 c2) = join_c1 b]

         Therefore, [join_c1 <= join_c2] iff [c1 >= c2]. *)

      let min ax =
        Modality (Join_const ((Mode.Const.Per_axis.max [@inlined hint]) ax))

      let max ax =
        Modality (Join_const ((Mode.Const.Per_axis.min [@inlined hint]) ax))

      let le ax (Modality (Join_const c1)) (Modality (Join_const c2)) =
        (Mode.Const.Per_axis.le [@inlined hint]) ax c2 c1

      let equal ax (Modality (Join_const c1)) (Modality (Join_const c2)) =
        (Mode.Const.Per_axis.equal [@inlined hint]) ax c2 c1

      let join ax (Modality (Join_const c1)) (Modality (Join_const c2)) =
        Modality
          (Join_const ((Mode.Const.Per_axis.meet [@inlined hint]) ax c1 c2))

      let meet ax (Modality (Join_const c1)) (Modality (Join_const c2)) =
        Modality (Join_const (Mode.Const.Per_axis.join ax c1 c2))

      let print ax ppf (Modality (Join_const c)) =
        Mode.Const.Per_axis.print ax ppf c
    end

    type t = Modality of Modality.Const.t [@@unboxed]

    let create ~uniqueness:(Atom.Modality (Join_const uniqueness))
        ~contention:(Atom.Modality (Join_const contention))
        ~visibility:(Atom.Modality (Join_const visibility))
        ~staticity:(Atom.Modality (Join_const staticity)) =
      Modality (Join_const { uniqueness; contention; visibility; staticity })

    let modality m (Modality t) = Modality (Modality.Const.concat ~then_:t m)

    let apply_left (Modality (Join_const c)) m =
      Mode.subtract_const_unhint c
        (Mode.unhint (Mode.join [Mode.of_const c; m]))

    let apply_right (Modality (Join_const c)) m =
      (* The right adjoint of join is a restriction of identity *)
      Mode.join_const_unhint c m

    let proj (type a) (ax : a Mode.Axis.t) (Modality (Join_const c)) : a Atom.t
        =
      Modality (Join_const ((Axis.proj [@inlined hint]) ax c))

    let set (type a) (ax : a Mode.Axis.t) (Modality (Join_const a) : a Atom.t)
        (Modality (Join_const c)) =
      Modality (Join_const ((Axis.set [@inlined hint]) ax a c))

    let le (Modality (Join_const c1)) (Modality (Join_const c2)) =
      Mode.Const.le c2 c1

    let equal (Modality (Join_const c1)) (Modality (Join_const c2)) =
      Mode.Const.equal c1 c2

    let max = Modality (Join_const Mode.Const.min)

    let min = Modality (Join_const Mode.Const.max)

    let join (Modality (Join_const c1)) (Modality (Join_const c2)) =
      Modality (Join_const (Mode.Const.meet c1 c2))

    let meet (Modality (Join_const c1)) (Modality (Join_const c2)) =
      Modality (Join_const (Mode.Const.join c1 c2))

    let print ppf (Modality m) =
      Fmt.fprintf ppf "Modality %a" Modality.Const.print m
  end

  module Comonadic = struct
    module Modality = Modality.Comonadic
    module Mode = Value.Comonadic

    module Atom = struct
      type 'a t = Modality of 'a Modality.Atom.t [@@unboxed]

      (* The ordering of crossing here is derived similarly to the monadic
         fragment. See comments there. *)

      let min ax =
        Modality (Meet_const ((Mode.Const.Per_axis.min [@inlined hint]) ax))

      let max ax =
        Modality (Meet_const ((Mode.Const.Per_axis.max [@inlined hint]) ax))

      let le ax (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
        (Mode.Const.Per_axis.le [@inlined hint]) ax c1 c2

      let equal ax (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
        (Mode.Const.Per_axis.equal [@inlined hint]) ax c1 c2

      let join ax (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
        Modality
          (Meet_const ((Mode.Const.Per_axis.join [@inlined hint]) ax c1 c2))

      let meet ax (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
        Modality (Meet_const (Mode.Const.Per_axis.meet ax c1 c2))

      let print ax ppf (Modality (Meet_const c)) =
        Mode.Const.Per_axis.print ax ppf c
    end

    type t = Modality of Modality.Const.t [@@unboxed]

    let create ~regionality:(Atom.Modality (Meet_const areality))
        ~linearity:(Atom.Modality (Meet_const linearity))
        ~portability:(Atom.Modality (Meet_const portability))
        ~forkable:(Atom.Modality (Meet_const forkable))
        ~yielding:(Atom.Modality (Meet_const yielding))
        ~statefulness:(Atom.Modality (Meet_const statefulness)) =
      Modality
        (Meet_const
           { areality;
             linearity;
             portability;
             statefulness;
             forkable;
             yielding
           })

    let always_constructed_at c = Modality (Meet_const c)

    let proj (type a) (ax : a Mode.Axis.t) (Modality (Meet_const c)) : a Atom.t
        =
      Modality (Meet_const ((Axis.proj [@inlined hint]) ax c))

    let set (type a) (ax : a Mode.Axis.t) (Modality (Meet_const a) : a Atom.t)
        (Modality (Meet_const c)) =
      Modality (Meet_const ((Axis.set [@inlined hint]) ax a c))

    let modality m (Modality t) = Modality (Modality.Const.concat ~then_:t m)

    let apply_left (Modality (Meet_const c)) m =
      (* The left adjoint of meet is a restriction of identity *)
      Mode.meet_const_unhint c m

    let apply_right (Modality (Meet_const c)) m =
      Mode.imply_const_unhint c
        (Mode.unhint (Mode.meet [Mode.of_const c; m]))

    let le (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
      Mode.Const.le c1 c2

    let equal (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
      Mode.Const.equal c1 c2

    let max = Modality (Meet_const Mode.Const.max)

    let min = Modality (Meet_const Mode.Const.min)

    let join (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
      Modality (Meet_const (Mode.Const.join c1 c2))

    let meet (Modality (Meet_const c1)) (Modality (Meet_const c2)) =
      Modality (Meet_const (Mode.Const.meet c1 c2))

    let print ppf (Modality m) =
      Fmt.fprintf ppf "Modality %a" Modality.Const.print m
  end

  module Axis = struct
    type 'a t =
      | Monadic : 'a Value.Monadic.Axis.t -> 'a Monadic.Atom.t t
      | Comonadic : 'a Value.Comonadic.Axis.t -> 'a Comonadic.Atom.t t

    type packed = P : 'a t -> packed

    let of_modality : Modality.Axis.packed -> packed = function
      | P (Monadic ax) -> P (Monadic ax)
      | P (Comonadic ax) -> P (Comonadic ax)

    let to_modality : packed -> Modality.Axis.packed = function
      | P (Monadic ax) -> P (Monadic ax)
      | P (Comonadic ax) -> P (Comonadic ax)

    let equal : type a b. a t -> b t -> (a, b) equality =
     fun ax1 ax2 ->
      match ax1, ax2 with
      | Monadic ax1, Monadic ax2 -> (
        match Axis.equal ax1 ax2 with Equal -> Equal | Not_equal -> Not_equal)
      | Comonadic ax1, Comonadic ax2 -> (
        match Axis.equal ax1 ax2 with Equal -> Equal | Not_equal -> Not_equal)
      | Monadic _, Comonadic _
      | Comonadic _, Monadic _ -> Not_equal

    let compare : type a b. a t -> b t -> (a, b) comparison =
     fun ax1 ax2 ->
      match ax1, ax2 with
      | Monadic ax1, Monadic ax2 -> begin
          match Axis.compare ax1 ax2 with
          | Less_than -> Less_than
          | Equal -> Equal
          | Greater_than -> Greater_than
        end
      | Monadic _, _ -> Less_than
      | _, Monadic _ -> Greater_than
      | Comonadic ax1, Comonadic ax2 -> begin
          match Axis.compare ax1 ax2 with
          | Less_than -> Less_than
          | Equal -> Equal
          | Greater_than -> Greater_than
        end

    let print : type a. Fmt.formatter -> a t -> unit =
     fun ppf -> function
      | Monadic ax -> Axis.print ppf ax
      | Comonadic ax -> Axis.print ppf ax
  end

  module Per_axis = struct
    open Axis

    let le : type a. a t -> a -> a -> bool =
     fun[@inline available] ax a b ->
      match ax with
      | Monadic ax -> (Monadic.Atom.le [@inlined hint]) ax a b
      | Comonadic ax -> (Comonadic.Atom.le [@inlined hint]) ax a b

    let equal : type a. a t -> a -> a -> bool =
     fun[@inline available] ax a b ->
      match ax with
      | Monadic ax -> (Monadic.Atom.equal [@inlined hint]) ax a b
      | Comonadic ax -> (Comonadic.Atom.equal [@inlined hint]) ax a b

    let min : type a. a t -> a = function[@inline available]
      | Monadic ax -> (Monadic.Atom.min [@inlined hint]) ax
      | Comonadic ax -> (Comonadic.Atom.min [@inlined hint]) ax

    let max : type a. a t -> a = function[@inline available]
      | Monadic ax -> (Monadic.Atom.max [@inlined hint]) ax
      | Comonadic ax -> (Comonadic.Atom.max [@inlined hint]) ax

    let meet : type a. a t -> a -> a -> a =
     fun[@inline available] ax a b ->
      match ax with
      | Monadic ax -> (Monadic.Atom.meet [@inlined hint]) ax a b
      | Comonadic ax -> (Comonadic.Atom.meet [@inlined hint]) ax a b

    let join : type a. a t -> a -> a -> a =
     fun[@inline available] ax a b ->
      match ax with
      | Monadic ax -> (Monadic.Atom.join [@inlined hint]) ax a b
      | Comonadic ax -> (Comonadic.Atom.join [@inlined hint]) ax a b

    let print : type a. a t -> Fmt.formatter -> a -> unit = function
      | Monadic ax -> Monadic.Atom.print ax
      | Comonadic ax -> Comonadic.Atom.print ax

    let print_obj = Axis.print

    let equal_obj = Axis.equal

    let compare_obj = Axis.compare
  end

  type t = (Monadic.t, Comonadic.t) monadic_comonadic

  let modality m { monadic; comonadic } =
    let monadic = Monadic.modality m.monadic monadic in
    let comonadic = Comonadic.modality m.comonadic comonadic in
    { monadic; comonadic }

  let apply_left_unhint t { monadic; comonadic } =
    let monadic = Monadic.apply_left t.monadic monadic in
    let comonadic =
      Comonadic.apply_left t.comonadic (S.Unhint.unhint comonadic)
    in
    { monadic; comonadic }

  let apply_left t m =
    Value.hint ~monadic:Crossing ~comonadic:Crossing
      (apply_left_unhint t (Value.disallow_right m))

  let apply_right_unhint t { monadic; comonadic } =
    let monadic = Monadic.apply_right t.monadic (S.Unhint.unhint monadic) in
    let comonadic = Comonadic.apply_right t.comonadic comonadic in
    { monadic; comonadic }

  let apply_right t m =
    Value.hint ~monadic:Crossing ~comonadic:Crossing
      (apply_right_unhint t (Value.disallow_left m))

  (* Our mode crossing is for [Value] modes, but can be extended to [Alloc]
     modes via [alloc_as_value], defined as follows:

     Given a mode crossing [f] for [Value], and we are to check [Alloc] submoding
     [m1 <= m2], we will instead check
     [f (alloc_as_value m1) <= f (alloc_as_value m2)].

     By adjunction tricks, this is equivalent to
     - [ m1 <= regional_to_global ∘ fr ∘ f ∘ alloc_as_value m2 ]
     - [ regional_to_local ∘ fl ∘ f ∘ alloc_as_value m1 <= m2 ]
     where [regional_to_global] is the right adjoint of [alloc_as_value], and
     [regional_to_local] the left adjoint. *)

  let apply_left_alloc t m =
    m
    |> alloc_as_value
    |> apply_left_unhint t
    |> value_to_alloc_r2l_unhint
    |> Alloc.hint ~comonadic:Crossing ~monadic:Crossing

  let apply_right_alloc t m =
    m
    |> alloc_as_value
    |> apply_right_unhint t
    |> value_to_alloc_r2g_unhint
    |> Alloc.hint ~comonadic:Crossing ~monadic:Crossing

  let apply_left_right_alloc t m =
    let { monadic; comonadic } = Alloc.unhint m in
    let monadic = Monadic.apply_right t.monadic monadic in
    let comonadic =
      comonadic |> comonadic_locality_as_regionality
      |> Comonadic.apply_left t.comonadic
      |> comonadic_regional_to_local
      (* the left adjoint of [locality_as_regionality]*)
    in
    Alloc.hint ~monadic:Crossing ~comonadic:Crossing { monadic; comonadic }

  let le t1 t2 =
    Monadic.le t1.monadic t2.monadic && Comonadic.le t1.comonadic t2.comonadic

  let max = { monadic = Monadic.max; comonadic = Comonadic.max }

  let min = { monadic = Monadic.min; comonadic = Comonadic.min }

  let join t1 t2 =
    { monadic = Monadic.join t1.monadic t2.monadic;
      comonadic = Comonadic.join t1.comonadic t2.comonadic
    }

  let meet t1 t2 =
    { monadic = Monadic.meet t1.monadic t2.monadic;
      comonadic = Comonadic.meet t1.comonadic t2.comonadic
    }

  let equal t1 t2 = le t1 t2 && le t2 t1

  let[@inline available] proj (type a) (ax : a Axis.t) { monadic; comonadic } :
      a =
    match ax with
    | Monadic ax -> (Monadic.proj [@inlined hint]) ax monadic
    | Comonadic ax -> (Comonadic.proj [@inlined hint]) ax comonadic

  let[@inline available] set (type a) (ax : a Axis.t) (a : a)
      { monadic; comonadic } : t =
    match ax with
    | Monadic ax ->
      { monadic = (Monadic.set [@inlined hint]) ax a monadic; comonadic }
    | Comonadic ax ->
      { monadic; comonadic = (Comonadic.set [@inlined hint]) ax a comonadic }

  let create ~regionality ~linearity ~uniqueness ~portability ~contention
      ~forkable ~yielding ~statefulness ~visibility ~staticity =
    let comonadic b ax =
      if b then Per_axis.min (Comonadic ax) else Per_axis.max (Comonadic ax)
    in
    let monadic b ax =
      if b then Per_axis.min (Monadic ax) else Per_axis.max (Monadic ax)
    in
    let regionality = comonadic regionality Areality in
    let linearity = comonadic linearity Linearity in
    let uniqueness = monadic uniqueness Uniqueness in
    let portability = comonadic portability Portability in
    let contention = monadic contention Contention in
    let forkable = comonadic forkable Forkable in
    let yielding = comonadic yielding Yielding in
    let statefulness = comonadic statefulness Statefulness in
    let visibility = monadic visibility Visibility in
    let staticity = monadic staticity Staticity in
    let monadic =
      Monadic.create ~uniqueness ~contention ~visibility ~staticity
    in
    let comonadic =
      Comonadic.create ~regionality ~linearity ~portability ~yielding ~forkable
        ~statefulness
    in
    { monadic; comonadic }

  let print ppf t =
    let l =
      List.filter_map
        (fun ax ->
          let (P ax) = ax |> Modality.Axis.of_value |> Axis.of_modality in
          let a = proj ax t in
          if Per_axis.(le ax (max ax) a)
          then None
          else Some (Fmt.asprintf "%a" (Per_axis.print ax) a))
        Value.Axis.all
    in
    Fmt.(pp_print_list ~pp_sep:pp_print_space pp_print_string ppf l)

  let to_modality
      { monadic = Monadic.Modality monadic;
        comonadic = Comonadic.Modality comonadic
      } =
    { monadic; comonadic }
end
