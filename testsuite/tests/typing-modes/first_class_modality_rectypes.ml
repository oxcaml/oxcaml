(* TEST
 flags = "-extension mode_alpha -rectypes -w -220";
 { expect; }
 { flags += " -no-ikinds"; expect; }
*)

let self_equation (xs : 'a list) = (xs : ('a @@ global) list)
[%%expect{|
Line 1, characters 36-38:
1 | let self_equation (xs : 'a list) = (xs : ('a @@ global) list)
                                        ^^
Error: The value "xs" has type "'a list" but an expression was expected of type
         "('a @@ global) list"
       The type variable "'a" occurs inside "('a @@ global)"
|}]

type recursive_alias = (recursive_alias @@ global)
[%%expect{|
Line 1, characters 0-50:
1 | type recursive_alias = (recursive_alias @@ global)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "recursive_alias" is recursive without boxing:
         "recursive_alias" = "(recursive_alias @@ global)",
         "(recursive_alias @@ global)" contains "recursive_alias"
|}]

type guarded = Node of (guarded @@ global)
[%%expect{|
type guarded = Node of (guarded @@ global)
|}]
