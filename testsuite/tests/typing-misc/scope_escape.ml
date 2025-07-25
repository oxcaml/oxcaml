(* TEST
   expect;
*)

let x = ref []

module M = struct
  type t

  let _ = (x : t list ref)
end

[%%expect
{|
val x : '_weak1 list ref = {contents = []}
Line 6, characters 11-12:
6 |   let _ = (x : t list ref)
               ^
Error: This expression has type "'weak1 list ref"
       but an expression was expected of type "t list ref"
       The type constructor "t" would escape its scope
|}]
