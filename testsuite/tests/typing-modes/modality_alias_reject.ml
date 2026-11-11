(* Unlike [modality_alias_crash.ml], the aliased library here contains a
   function, which does not cross portability, so the [@@ portable] claim
   on [Outer_alias] in the interface must be rejected. *)

module Outer = struct
  module Inner = Modality_alias_np_lib
end

module Outer_alias = Outer
