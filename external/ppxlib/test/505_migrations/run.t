OCaml 5.5 Migrations
--------------------

1. Pexp_struct_item AST node

We first check that the changes to various let-bindings are suitably migrated.
[let module M = T in], [let open M in] and [let exception C in] are now all
represented with the same AST node.

We have a custom driver that will force migration of the AST down to 5.2 and back to
the compiler's version and print it as source code using the compiler's printer,
regardless of ppxlib's internal AST version.

If we run the driver on the following source file:

  $ cat > test.ml << EOF
  > module T = struct let x = 1 end
  > let f =
  >   let exception E of int in
  >   let module X = T in
  >   let open X in
  >   x
  > EOF

it should successfully roundtrip to 5.2 and print the source code unchanged:

  $ ./driver.exe test.ml --use-compiler-pp
  module T = struct let x = 1 end
  let f = let exception E of int  in let module X = T in let open X in x

In addition to these items, the 5.5 AST can now encode pretty much any structure item
locally, except for a few (e.g. [let let ...]). We have to handle these also during migration.

  $ cat > extra.ml << EOF
  > type e = ..
  > 
  > let f =
  >   let type e += Hello in
  >   let external id : 'a -> 'a = "identity" in 
  >   let type t = int in
  >   ()
  > EOF

  $ ./driver.exe extra.ml --use-compiler-pp
  type e = ..
  let f =
    let type e +=  
          | Hello  in
      let external id : 'a -> 'a = "identity" in let type t = int in ()

2. Ptyp_extension

A new feature of OCaml 5.5 are external types (e.g. [type t = external "t"]).

  $ cat > test.ml << EOF
  > type t = external "t"
  > EOF

For now, we do not support these and raise an error. In the future we may wish
to encode this feature into attributes and this test, along with this comment,
will need updated.

  $ ./driver.exe test.ml --use-compiler-pp
  type t = external "t"

3. Ptyp_functor

Finally, OCaml 5.5 comes with modular explicits (a.k.a module-dependent
arrows). Users should be able to migrate code without losing the modular
explicit.

  $ cat > test.ml << EOF
  > module type Add = sig
  >   type t
  >   val add : t -> t -> t
  > end
  > let add : (module A : Add) -> A.t -> A.t -> A.t =
  >   fun (module A : Add) a b -> A.add a b
  > EOF

  $ ./driver.exe test.ml --use-compiler-pp
  module type Add  = sig type t val add : t -> t -> t end
  let add : (module A : Add) -> A.t -> A.t -> A.t =
    fun (module A : Add) a b -> A.add a b

This brings up another semantic change to the syntax. In functions, we now have
either [fun (module X : T) ...] or [fun ((module X) : (module T))...] which are
two different things w.r.t to modular explicits.

  $ cat > test.ml << EOF
  > module type T = sig
  >   type t
  > end
  > 
  > let foo = fun ((module X) : (module T)) () -> ()
  > let foo = fun ((module _) : (module T)) () -> ()
  > let foo = fun (module X : T) () -> ()
  > let foo = fun (module _ : T) () -> ()
  > EOF

  $ ./driver.exe test.ml --use-compiler-pp
  module type T  = sig type t end
  let foo ((module X)  : (module T)) () = ()
  let foo ((module _)  : (module T)) () = ()
  let foo (module X : T) () = ()
  let foo (module _ : T) () = ()
