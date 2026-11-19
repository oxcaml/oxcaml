These tests are for a bug in ppxlib where doc comments are not removed/updated when 
registering corrections.

For example, before the fix, the following:

| [@@@expand_inline
|   (** i am foo *)
|   let foo = ()
| ]
| (** i am bar *) 
| let bar = ()
| [@@@end]

Would get corrected to:

| [@@@expand_inline
|   (** i am foo *)
|   let foo = ()
| ]
| (** i am bar *) 
| let foo = () [@@ocaml.doc {| i am foo |}]
| [@@@end]

When the correct thing to do would be to remove the doc comment:

| [@@@expand_inline
|   (** i am foo *)
|   let foo = ()
| ]
| let foo = () [@@ocaml.doc {| i am foo |}]
| [@@@end]

This bug occurs because the OCaml parser doesn't update the location of 
structure/signature items to include the location of attached doc comments. These
locations are used by ppxlib to determine where to update the source code in corrected 
files, thus without any intervention, doc comments will not get removed.

  $ run() {
  >   cat > program.ml
  >   ./ppx.exe -no-color -null -diff-cmd 'diff -u --label "" --label ""' "$@" program.ml
  > }

Basic tests.

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > let foo = ()
  > ]
  > (** bar *)
  > let bar = ()
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -1,6 +1,5 @@
   [@@@expand_inline
   let foo = ()
   ]
  -(** bar *)
  -let bar = ()
  +let foo = ()
   [@@@end]
  [1]

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > let foo = ()
  > ]
  > let bar = ()
  > (** bar *)
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -1,6 +1,5 @@
   [@@@expand_inline
   let foo = ()
   ]
  -let bar = ()
  -(** bar *)
  +let foo = ()
   [@@@end]
  [1]

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > let foo = ()
  > ]
  > (** bar *)
  > let bar = ()
  > 
  > (** qux *)
  > and qux = ()
  > and baz = ()
  > (** baz *)
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -1,11 +1,5 @@
   [@@@expand_inline
   let foo = ()
   ]
  -(** bar *)
  -let bar = ()
  -
  -(** qux *)
  -and qux = ()
  -and baz = ()
  -(** baz *)
  +let foo = ()
   [@@@end]
  [1]
  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  >   (** foo *) 
  >   let foo = 10
  > ]
  > (** bar *)
  > let bar = 20
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -2,6 +2,5 @@
     (** foo *) 
     let foo = 10
   ]
  -(** bar *)
  -let bar = 20
  +let foo = 10[@@ocaml.doc {| foo |}]
   [@@@end]
  [1]

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > (** i am foo *)
  > let foo = ()
  > 
  > (** i am bar *)
  > let bar = ()
  > ]
  > (** i am bar *)
  > let bar = ()
  > 
  > (** i am foo *)
  > let foo = ()
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -5,9 +5,6 @@
   (** i am bar *)
   let bar = ()
   ]
  -(** i am bar *)
  -let bar = ()
  -
  -(** i am foo *)
  -let foo = ()
  +let foo = ()[@@ocaml.doc {| i am foo |}]
  +let bar = ()[@@ocaml.doc {| i am bar |}]
   [@@@end]
  [1]

Doc comments on [val] declerations.

  $ run -raise-embedded-errors <<EOF
  > module type S = sig 
  >   [@@@expand_inline: 
  >     (** foo *)
  >     val foo : unit
  >   ] 
  >   (** bar *)
  >   val bar : unit 
  >   [@@@end]
  > end
  > EOF
  --- 
  +++ 
  @@ -3,7 +3,7 @@
       (** foo *)
       val foo : unit
     ] 
  -  (** bar *)
  -  val bar : unit 
  +  
  +val foo : unit[@@ocaml.doc {| foo |}]
     [@@@end]
   end
  [1]

Doc comments on [module]s.

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > (** foo *)
  > module Foo = struct end
  > ] 
  > (** bar *)
  > module Foo = struct end
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -2,6 +2,5 @@
   (** foo *)
   module Foo = struct end
   ] 
  -(** bar *)
  -module Foo = struct end
  +module Foo = struct  end[@@ocaml.doc {| foo |}]
   [@@@end]
  [1]

Doc comments on [module type]s.

  $ run -raise-embedded-errors <<EOF
  > [@@@expand_inline
  > (** foo *)
  > module type Foo = sig end
  > ] 
  > (** bar *)
  > module type Foo = sig end
  > [@@@end]
  > EOF
  --- 
  +++ 
  @@ -2,6 +2,5 @@
   (** foo *)
   module type Foo = sig end
   ] 
  -(** bar *)
  -module type Foo = sig end
  +module type Foo  = sig  end[@@ocaml.doc {| foo |}]
   [@@@end]
  [1]
