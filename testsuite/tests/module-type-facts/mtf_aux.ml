module type S = sig type t end

module type T = S with type t = int

module M : S = struct type t = int end

module F (X : S) = struct type u = X.t end

module Wrapper = struct
  module type Inner = S

  module N : Inner = struct type t = char end
end

module type Container = sig
  module type Local = S

  module Member : S
end

(* An application, whose instance context is recorded as an equality, and a
   snapshot of a structure that is not a path, whose subject cannot be named
   and is therefore recorded as an omission.  Together with the ascriptions
   and the interface above they make all four lists of facts non-empty. *)
module FM = F (M)

module type Snapshot = module type of struct type t = int end

module Snapshotted : Snapshot = struct type t = int end
