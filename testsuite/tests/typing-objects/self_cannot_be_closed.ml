(* TEST
   expect;
*)
let is_empty (x : < >) = ()

[%%expect {|
val is_empty : <  > -> unit = <fun>
|}]

class c =
  object (self)
    method private foo = is_empty self
  end

[%%expect
{|
Line 3, characters 34-38:
3 |     method private foo = is_empty self
                                      ^^^^
Error: This expression has type "< .. >" but an expression was expected of type
         "<  >"
       Self type cannot be unified with a closed object type
|}]
