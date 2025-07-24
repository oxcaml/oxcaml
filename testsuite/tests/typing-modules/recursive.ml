(* TEST
   expect;
*)

(* PR#7324 *)

module rec T : sig
  type t = T.t
end =
  T

[%%expect
{|
Lines 1-4, characters 0-3:
1 | module rec T : sig
2 |   type t = T.t
3 | end =
4 |   T
Error: The type abbreviation "T.t" is cyclic:
         "T.t" = "T.t"
|}]

(* Cyclic module type definitions should throw an error *)
module rec X : sig
  module type A = X.A
end = struct
  module type A
end

[%%expect
{|
Line 2, characters 18-21:
2 |   module type A = X.A
                      ^^^
Error: This module type is recursive. This use of the recursive module "X"
       within its own definition makes the module type of "X" depend on itself.
       Such recursive definitions of module types are not allowed.
|}]

(* Cyclic module type definitions should throw an error *)
module rec X : sig
  module type A := X.A
end = struct end

[%%expect
{|
Line 2, characters 19-22:
2 |   module type A := X.A
                       ^^^
Error: This module type is recursive. This use of the recursive module "X"
       within its own definition makes the module type of "X" depend on itself.
       Such recursive definitions of module types are not allowed.
|}]
