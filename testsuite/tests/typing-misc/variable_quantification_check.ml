(* TEST
 expect;
*)

module type Test1 = sig
    val unbound_variable : 'a . 'a -> 'b
end

[%%expect{|
module type Test1 = sig val unbound_variable : 'a -> 'b end
|}]

module type Test2 = sig
    val wildcard : 'a . _ option -> 'a
end

[%%expect{|
module type Test2 = sig val wildcard : 'b option -> 'a end
|}]
