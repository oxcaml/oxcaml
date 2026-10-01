(* Parameters whose type variables have a kind. *)
val f : ('a : immutable_data) -> bool
law? kinded (x : ('a : immutable_data)) : f x
law? kinded_unused (x : ('a : immutable_data)) : true
law? kinded_shared (x : ('a : immutable_data)) (y : 'a) : f x && f y
law? nullable (x : ('a : value_or_null)) : true
law? plain (x : 'a) : true
