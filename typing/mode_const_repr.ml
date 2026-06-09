module Locality = struct
  type t =
    | Global
    | Local
end

module Regionality = struct
  type t =
    | Global
    | Regional
    | Local
end

module Uniqueness = struct
  type t =
    | Unique
    | Aliased
end

module Linearity = struct
  type t =
    | Many
    | Once
end

module Portability = struct
  (* Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Portable
    | Shareable
    | Corruptible
    | Nonportable
end

module Contention = struct
  (* Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Uncontended
    | Corrupted
    | Shared
    | Contended
end

module Forkable = struct
  type t =
    | Forkable
    | Unforkable
end

module Yielding = struct
  type t =
    | Unyielding
    | Yielding
end

module Statefulness = struct
  (* Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Stateless
    | Writing
    | Reading
    | Stateful
end

module Visibility = struct
  (* Constructor order is used by [Diamond] in [Mode]. *)
  type t =
    | Read_write
    | Read
    | Write
    | Immutable
end

module Staticity = struct
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

module Modality = struct
  module Monadic = struct
    type t = Join_const of monadic [@@unboxed]
  end

  module Comonadic = struct
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
