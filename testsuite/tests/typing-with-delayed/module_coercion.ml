(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

module Big = struct
  let padding = 17
  let answer = 42
end

module type S = sig
  module M : sig val answer : int end
end

module type Refined = S with module M = Big

module X : Refined = struct
  module M = Big
end

module Use (X : S) = struct
  let answer = X.M.answer
end

module Y = Use (X)

let () = assert (Y.answer = 42)


module type With_type = sig
  type t
  module M : sig val answer : int end
end

module type Chained =
  With_type with module M = Big with type t = unit

module XC : Chained = struct
  type t = unit
  module M = Big
end

module Use_chained (X : With_type) = struct
  let answer = X.M.answer
end

module YC = Use_chained (XC)

module Big_functor (_ : sig end) = Big

module type With_functor = sig
  module F : functor (_ : sig end) -> sig val answer : int end
end

module XF : With_functor with module F = Big_functor = struct
  module F = Big_functor
end

module Use_functor (X : With_functor) = struct
  module R = X.F (struct end)
  let answer = R.answer
end

module YF = Use_functor (XF)

let () =
  assert (YC.answer = 42);
  assert (YF.answer = 42)


module type Type_only = sig
  type t
  val value : t
  module M : sig val identity : t -> t end
end

module XT : Type_only with type t = int = struct
  type t = int
  let value = 42
  module M = struct let identity x = x end
end

module Use_type (X : Type_only) = struct
  let value = X.M.identity X.value
end

module YT = Use_type (XT)

let () = assert (YT.value = 42)

module Type_member = struct
  type t = int
  let answer = 42
end

module type Aliased_member = sig
  module M = Type_member
  val tail : int
end

module Alias_value : Aliased_member with type M.t = int = struct
  module M = Type_member
  let tail = 17
end

module Use_alias (X : Aliased_member) = struct
  let answer = X.M.answer
  let tail = X.tail
end

module Alias_result = Use_alias (Alias_value)

module type Present_member = sig
  module M : sig type t val answer : int end
  val tail : int
end

module type Strengthened_member = Present_member with Alias_value

module Strengthened_value : Strengthened_member with type M.t = int =
  Alias_value

module Use_present (X : Present_member) = struct
  let answer = X.M.answer
  let tail = X.tail
end

module Strengthened_result = Use_present (Strengthened_value)

let () =
  assert (Alias_result.answer = 42);
  assert (Alias_result.tail = 17);
  assert (Strengthened_result.answer = 42);
  assert (Strengthened_result.tail = 17)
