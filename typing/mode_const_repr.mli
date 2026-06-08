(** Low-level mode and modality constants that must be available before
    [Mode_hint].

    [Mode] aliases these types as its canonical constant representation and
    adds the lattice operations and printing around them. Keeping only the
    representation here lets hints carry concrete constant modalities without
    making [Mode_hint] depend on [Mode]. *)

module Locality : sig
  type t =
    | Global
    | Local
end

module Regionality : sig
  type t =
    | Global
    | Regional
    | Local
end

module Uniqueness : sig
  type t =
    | Unique
    | Aliased
end

module Linearity : sig
  type t =
    | Many
    | Once
end

module Portability : sig
  (** Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Portable
    | Shareable
    | Corruptible
    | Nonportable
end

module Contention : sig
  (** Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Uncontended
    | Corrupted
    | Shared
    | Contended
end

module Forkable : sig
  type t =
    | Forkable
    | Unforkable
end

module Yielding : sig
  type t =
    | Unyielding
    | Yielding
end

module Statefulness : sig
  (** Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Stateless
    | Writing
    | Reading
    | Stateful
end

module Visibility : sig
  (** Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Read_write
    | Read
    | Write
    | Immutable
end

module Staticity : sig
  type t =
    | Static
    | Dynamic
end

type monadic =
  { uniqueness : Uniqueness.t;
    contention : Contention.t;
    visibility : Visibility.t;
    staticity : Staticity.t
  }

type 'areality comonadic_with =
  { areality : 'areality;
    linearity : Linearity.t;
    portability : Portability.t;
    forkable : Forkable.t;
    yielding : Yielding.t;
    statefulness : Statefulness.t
  }

module Modality : sig
  module Monadic : sig
    type t = Join_const of monadic [@@unboxed]
  end

  module Comonadic : sig
    type t = Meet_const of Regionality.t comonadic_with [@@unboxed]
  end

  type t =
    { monadic : Monadic.t;
      comonadic : Comonadic.t
    }
end

type t = Modality.t =
  { monadic : Modality.Monadic.t;
    comonadic : Modality.Comonadic.t
  }
