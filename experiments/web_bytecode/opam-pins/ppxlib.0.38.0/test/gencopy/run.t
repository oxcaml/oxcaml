gencopy is a dev utility tool that we use to generate migration functions
skeletons when adding support for new compiler versions.

We can run it on two empty .ml files:

  $ touch in.ml
  $ touch out.ml

  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  
It then simply generates the headers which are a global open to stdlib0 and a
default copy function for location that one could overwrite if needed.

================= Simple types =================

We can test it on a pair of simple types, note that all types are expected to be
defined in submodules and not at the root as the ast_X.ml files are laid out that
way:

  $ cat > in.ml << EOF
  > module X = struct
  >   type r = {a: int; b: string}
  > end
  > 
  > module Y = struct
  >   type s = A of int | B of string
  > end
  > EOF

  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  let rec copy_r : In.X.r -> Out.X.r =
    fun { In.X.a = a; In.X.b = b } -> { Out.X.a = a; Out.X.b = b }
  and copy_s : In.Y.s -> Out.Y.s =
    function | In.Y.A x0 -> Out.Y.A x0 | In.Y.B x0 -> Out.Y.B x0

Note that it just copies the base types (string, int, etc...) directly and does
not expect a copy_string function.

================= Recursive types ===============

Recursive types should be handled properly:

  $ cat > in.ml << EOF
  > module X = struct
  >   type a = b list
  >   and b = A of a | B
  > end
  > EOF

  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  let rec copy_a : In.X.a -> Out.X.a = fun x -> List.map copy_b x
  and copy_b : In.X.b -> Out.X.b =
    function | In.X.A x0 -> Out.X.A (copy_a x0) | In.X.B -> Out.X.B

Any type it encounters a non base type, it assumes that a copy_ function
for this type will be defined. If the type is defined in the input file, it
should generate it itself, otherwise it will have to be added manually.

================= Parametrized types ===============

A copy_function for a parametrized type should be polymorphic and take
one extra argument per type parameter. It should also properly pass that
argument when copying instances of a parametrized type.

  $ cat > in.ml << EOF
  > module X = struct
  >   type 'a loc = { loc: location; txt: 'a }
  >   and u = A | B
  >   and v = { u : u loc; flags : int }
  > end
  > EOF
 
  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  let rec copy_loc : 'f0 'g0 . ('f0 -> 'g0) -> 'f0 In.X.loc -> 'g0 Out.X.loc =
    fun f0 { In.X.loc = loc; In.X.txt = txt } ->
      { Out.X.loc = (copy_location loc); Out.X.txt = (f0 txt) }
  and copy_u : In.X.u -> Out.X.u =
    function | In.X.A -> Out.X.A | In.X.B -> Out.X.B
  and copy_v : In.X.v -> Out.X.v =
    fun { In.X.u = u; In.X.flags = flags } ->
      { Out.X.u = (copy_loc copy_u u); Out.X.flags = flags }

=================== Inline records =================

Inline record fields should not be qualified (e.g. Modname.fiel_name) as this
would trigger a compile error. gencopy should properly handle this:

  $ cat > in.ml << EOF
  > module X = struct
  >   type x =
  >     | A of {i: int; s: string}
  > end
  > EOF

  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  let rec copy_x : In.X.x -> Out.X.x =
    function | In.X.A { i; s } -> Out.X.A { i; s }

====================== X.t as x ====================

For convenience, gencopy the copy function for X.t as copy_x:

  $ cat > in.ml << EOF
  > module X = struct
  >   type t = int
  > end
  > EOF

  $ ../../dev/gencopy/gencopy.exe in.ml out.ml
  open Stdlib0
  let copy_location x = x
  let rec copy_x : In.X.t -> Out.X.t = fun x -> x

