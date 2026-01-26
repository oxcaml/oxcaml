(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

module type Test1 = sig
    val unbound_variable : 'a . 'a -> 'b
end

[%%expect{|
Line 2, characters 38-40:
2 |     val unbound_variable : 'a . 'a -> 'b
                                          ^^
Error: The type variable "'b" is unbound in this type declaration.
Hint: Explicit quantification requires quantifying all type variables for compatibility with upstream OCaml.
Enable non-erasable extensions to disable this check.
|}]

module type Test2 = sig
    val wildcard : 'a . _ option -> 'a
end

[%%expect{|
Line 2, characters 24-25:
2 |     val wildcard : 'a . _ option -> 'a
                            ^
Error: A type wildcard "_" is not allowed in this type declaration.
Hint: Explicit quantification requires quantifying all type variables for compatibility with upstream OCaml.
Enable non-erasable extensions to disable this check.
|}]
