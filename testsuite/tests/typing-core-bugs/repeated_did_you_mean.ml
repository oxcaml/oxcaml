(* TEST
   expect;
*)

(* MPR 7864 *)

let foo = 12

module M = struct
  let foo = 13
end

open M

let _ = fox

[%%expect
{|
val foo : int = 12
module M : sig val foo : int end
Line 9, characters 8-11:
9 | let _ = fox
            ^^^
Error: Unbound value "fox"
Hint: Did you mean "foo"?
|}]
