(* TEST
   flags = " -w +A ";
   expect;
*)

module type U = sig end

[%%expect {|
module type U = sig end
|}]

module M : sig
  module F2 (_ : U) : U
end = struct
  module X = struct
    let x = 13
  end

  module F1 (_ : U) = X
  module F2 (M : U) = F1 (M)
end

[%%expect
{|
Line 5, characters 8-9:
5 |     let x = 13
            ^
Warning 32 [unused-value-declaration]: unused value x.

module M : sig module F2 : U -> U end
|}]

module N : sig
  module F2 (_ : U) : U
end = struct
  module X = struct
    let x = 13
  end

  module F1 (_ : U) = X

  module F2 (_ : U) = F1 ()
end

[%%expect
{|
Line 10, characters 22-27:
10 |   module F2 (_ : U) = F1 ()
                           ^^^^^
Error: The functor was expected to be applicative at this position
|}]

module F (X : sig
  type t

  type s
end) =
struct
  type t = X.t
end

[%%expect
{|
Line 4, characters 2-8:
4 |   type s
      ^^^^^^
Warning 34 [unused-type-declaration]: unused type s.

module F : functor (X : sig type t type s end) -> sig type t = X.t end
|}]
