(* Unlike [modality_alias_crash.ml], the aliased library here contains a
   function, which does not cross portability. This is still accepted: a
   module alias carries no modality, so the interface's default
   [@@ portable] does not apply to [Outer_alias]. *)

module Outer = struct
  module Inner = Modality_alias_np_lib
end

module Outer_alias = Outer
