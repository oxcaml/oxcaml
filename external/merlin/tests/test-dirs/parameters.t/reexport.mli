(** An alias to the parameter [P] *)
module As_alias : sig include module type of struct include P end end

module Included : sig
  include module type of struct
    include P
  end
end

(* CR-someday: At the moment, [As_alias] is not actually an alias. That's because the
   compiler has temporarily disabled aliasing parameterized modules (see
   https://github.com/oxcaml/oxcaml/pull/4948). Rather than deleting the tests that use
   aliasing, for now we just switch [As_alias] to being defined via an include statement.
   This should be switched back when aliasing is allowed again. (This should also be
   changed in the .ml file.) *)
