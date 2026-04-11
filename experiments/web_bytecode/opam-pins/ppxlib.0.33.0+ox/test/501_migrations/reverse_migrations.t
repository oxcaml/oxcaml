The 501 parsetree contains a parsing modificacion.
[compare_on.exe <file> ./reverse_migrations.exe] checks if there's a diff between the
AST's resulting from
1. parsing <file> on 5.0.0 directly
2. parsing <file> on 5.0.0, migrating up to 5.1.0 and migrating back to 5.0.0

------------------

Tests for the Parsetree change for type constraints in value bindings

  $ echo "let x : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let _ : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe   | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

The downward migration isn't able to recover the whole pattern location range,
since it doesn't track the location of the closing brackets.
  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -6 +6 @@
  -        pattern (file.ml[1,0+4]..[1,0+24]) ghost
  +        pattern (file.ml[1,0+4]..[1,0+23]) ghost

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -6 +6 @@
  -        pattern (file.ml[1,0+4]..[1,0+24]) ghost
  +        pattern (file.ml[1,0+4]..[1,0+23]) ghost

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo "let f : type a . a = assert false" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x : <m:int; n:int> :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe  | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

The diffs are on locations. If the location modification
at least preserved the location invariants, it might be acceptable.
However, in several cases it doesn't.

  $ echo "let x : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x as y) : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ cat > file.ml << EOF
  > type t = {a : int}
  > let {a} = {a = 5}
  > EOF
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null


  $ echo "let _ : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let _ = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

Here we're expecting a similar location diff as above. However, the downward
migration is faulty: it turns [let (x) : int = 5] (constraint only on pattern)
into [let x : int = 5] (contraint on both pattern an expression).
  $ echo "let (x) : int = 5" > file.ml
  $ ./compare_on.exe file.ml ./reverse_migrations.exe | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -9,0 +10,9 @@
  +          core_type (file.ml[1,0+10]..[1,0+13]) ghost
  +            Ptyp_poly
  +            core_type (file.ml[1,0+10]..[1,0+13])
  +              Ptyp_constr "int" (file.ml[1,0+10]..[1,0+13])
  +              []
  +        expression (file.ml[1,0+4]..[1,0+17])
  +          Pexp_constraint
  +          expression (file.ml[1,0+16]..[1,0+17])
  +            Pexp_constant PConst_int (5,None)
  @@ -13,2 +21,0 @@
  -        expression (file.ml[1,0+16]..[1,0+17])
  -          Pexp_constant PConst_int (5,None)

Let's make sure that in the examples with diffs,
the location invariants are still fulfilled.

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null

  $ echo "let (x) : int = 5" > file.ml
  $ ./reverse_migrations.exe -check -locations-check file.ml > /dev/null


------------------

Tests for the Parsetree change for generative functor applications

  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F ()
  > EOF
  $ ./compare_on.exe file.ml ./reverse_migrations.exe | grep -v "without_migrations" | grep -v "with_migrations"
  [1]

When going up, F(struct end) is turned into F(), which makes the location be lost.
It could be stored in an attribute, or turned into F(struct end [@warning "-73"]).

  $ cat > file.ml << EOF
  > module F () = struct end
  > module M = F(struct end)
  > EOF
  $ ./compare_on.exe file.ml ./reverse_migrations.exe | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -17 +17 @@
  -        module_expr (file.ml[2,25+13]..[2,25+23])
  +        module_expr (file.ml[2,25+11]..[2,25+24])

  $ cat > file.ml << EOF
  > module F (N : sig end) = struct end
  > module M = F (struct end)
  > EOF
  $ ./compare_on.exe file.ml ./reverse_migrations.exe | grep -v "without_migrations" | grep -v "with_migrations"
  @@ -20 +20 @@
  -        module_expr (file.ml[2,36+14]..[2,36+24])
  +        module_expr (file.ml[2,36+11]..[2,36+25])
