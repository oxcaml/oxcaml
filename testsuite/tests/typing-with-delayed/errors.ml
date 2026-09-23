(* TEST
 expect;
*)

module type S = sig
  class type c = object method m : int end
end with type c = < m : int >;;
[%%expect {|
Lines 1-3, characters 16-29:
1 | ................sig
2 |   class type c = object method m : int end
3 | end with type c = < m : int >..
Error: The signature constrained by "with" has no component named "c"
|}]

module type S = sig
  type t
end with type missing = int;;
[%%expect {|
Lines 1-3, characters 16-27:
1 | ................sig
2 |   type t
3 | end with type missing = int..
Error: The signature constrained by "with" has no component named "missing"
|}]

module type S = sig
  module M : sig type t end
end with type M.missing = int;;
[%%expect {|
Lines 1-3, characters 16-29:
1 | ................sig
2 |   module M : sig type t end
3 | end with type M.missing = int..
Error: The signature constrained by "with" has no component named "M.missing"
|}]
