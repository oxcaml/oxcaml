module Exposed : sig
  type u
end

type 'a opaque_arg
type 'a tracked : immutable_data with 'a
type public_u
type from_hidden
type from_exposed
type from_alias
type partial
type 'a both
type 'a from_functor
type 'a from_functor_immutable
type 'a box
type ('a, 'b) pair
type 'a masked
type 'a residue
type 'a in_mutable
