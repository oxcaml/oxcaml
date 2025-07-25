(* TEST
   expect;
*)

class type c = object end

[%%expect {|
class type c = object  end
|}]

module type S = sig
  class c : c
end

[%%expect
{|
Line 2, characters 12-13:
2 |   class c : c
                ^
Error: The class type "c" is not yet completely defined
|}]
