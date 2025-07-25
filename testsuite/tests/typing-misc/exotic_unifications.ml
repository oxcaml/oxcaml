(* TEST
   expect;
*)

class virtual t =
  object
    method virtual x : float
  end

class x =
  object (self : < x : int ; .. >)
    inherit t
  end

[%%expect
{|
class virtual t : object method virtual x : float end
Line 8, characters 4-13:
8 |     inherit t
        ^^^^^^^^^
Error: The method "x" has type "int" but is expected to have type "float"
       Type "int" is not compatible with type "float"
|}]

let x =
  let module M = struct
    module type t = sig end
  end in
  (module struct end : M.t)

[%%expect
{|
Line 5, characters 2-27:
5 |   (module struct end : M.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(module M.t)"
       but an expression was expected of type "'a"
       The module type "M.t" would escape its scope
|}]
