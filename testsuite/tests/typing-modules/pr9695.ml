(* TEST
   expect;
*)

module Test (S : sig
  module type S
end)
(M : S.S) =
struct
  open M (* should not succeed silently *)
end

[%%expect
{|
Line 6, characters 7-8:
6 |   open M (* should not succeed silently *)
           ^
Error: The module "M" is of abstract type "S.S", it cannot be opend
|}]
