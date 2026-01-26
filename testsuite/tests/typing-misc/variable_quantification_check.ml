(* TEST
 {
   native;
 }{
   bytecode;
 }
*)

module type Test1 = sig
    val unbound_variable : 'a . 'a -> 'b
end

module type Test2 = sig
    val wildcard : 'a . _ option -> 'a
end

