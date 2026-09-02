The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file>] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.1.0 directly
2. parsing <file> on 5.1.0, migrating down to 5.0.0 and migrating back to 5.1.0

------------------

Tests for the Parsetree change for type constraints in value bindings

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let (x) : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let _ : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]


Here might be a problem in the upward migration: the 5.1.0 parser parses the constraint as a pattern constraint.
However, the upward migration makes a value binding constraint out of it.
  $ echo "let ((x,y) : (int*int)) = (assert false: int * int)" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -6,20 +6,18 @@
  -        pattern (file.ml[1,0+4]..[1,0+23])
  -          Ppat_constraint
  -          pattern (file.ml[1,0+5]..[1,0+10])
  -            Ppat_tuple
  -            [
  -              pattern (file.ml[1,0+6]..[1,0+7])
  -                Ppat_var "x" (file.ml[1,0+6]..[1,0+7])
  -              pattern (file.ml[1,0+8]..[1,0+9])
  -                Ppat_var "y" (file.ml[1,0+8]..[1,0+9])
  -            ]
  -          core_type (file.ml[1,0+14]..[1,0+21])
  -            Ptyp_tuple
  -            [
  -              core_type (file.ml[1,0+14]..[1,0+17])
  -                Ptyp_constr "int" (file.ml[1,0+14]..[1,0+17])
  -                []
  -              core_type (file.ml[1,0+18]..[1,0+21])
  -                Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  -                []
  -            ]
  +        pattern (file.ml[1,0+5]..[1,0+10])
  +          Ppat_tuple
  +          [
  +            pattern (file.ml[1,0+6]..[1,0+7])
  +              Ppat_var "x" (file.ml[1,0+6]..[1,0+7])
  +            pattern (file.ml[1,0+8]..[1,0+9])
  +              Ppat_var "y" (file.ml[1,0+8]..[1,0+9])
  +          ]
  +        core_type (file.ml[1,0+14]..[1,0+21])
  +          Ptyp_tuple
  +          [
  +            core_type (file.ml[1,0+14]..[1,0+17])
  +              Ptyp_constr "int" (file.ml[1,0+14]..[1,0+17])
  +              []
  +            core_type (file.ml[1,0+18]..[1,0+21])
  +              Ptyp_constr "int" (file.ml[1,0+18]..[1,0+21])
  +              []
  +          ]

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]


Here we may expect a diff (downwards migrating should yield the same as in the example right above).
However, those case are recoverable.

First, both

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

and
  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

are translated to the same 5.0 AST tree. But the locations on the expression
constraint and pattern constraint are only the same in the second case.
Thus, we can distinguish between the two.

Similarly, the syntactic translation for

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

and

  $ echo 'let x : [`A | `B] = (`A :> [ `A | `B ] )' > file.ml
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

are pretty close: The former is translated to "let (x: ø .  [`A | `B]) = (`A :> [`A | `B])"
whereas the latter is mapped to "let (x: ø .  [`A | `B]) = ((`A :> [`A | `B]): [`A | `B]) ".
However, the two case can be distingued by the fact that we have either an outward coercion
or an outward constraint associated to a `Ptyp_poly([],...)` pattern constraint.

Let's make sure that in the examples with diffs,
the location invariants are still fulfilled.
  $ echo "let ((x,y) : (int*int)) = (assert false: int * int)" > file.ml
  $ ./identity_driver.exe -check -locations-check file.ml > /dev/null

------------------

Tests for the Parsetree change for generative functor applications


  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F ()
  > EOF
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F [@attr1] () [@attr2]
  > EOF
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F(struct end)
  > EOF
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ cat > file.ml << EOF
  > module F (N : sig end) = struct end
  > module M = F (struct end)
  > EOF
  $ ./compare_on.exe file.ml ./identity_driver.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]
