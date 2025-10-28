# OCaml JIT compiler

This library provides a way to execute the native toplevel on x86 platforms without
running external processes such as `as` and `ln` as the regular native toplevel does.
Everything is handled in process which results in much better performances

At the moment, it builds on top of 4.11 and requires a few simple modification to the
compiler which can be found [here](https://github.com/NathanReb/ocaml/tree/jit-hook-411).

If you just want to give it a quick spin you can run:
```
git clone git@github.com:NathanReb/ocaml-jit.git
cd ocaml-jit
opam compiler create --switch=. NathanReb:jit-hook-411
opam install --deps-only ./
dune exec jittop
```

To use it in your project you will need to use the above mentioned compiler fork,
you can create a fresh local switch on top of it using the `opam-compiler` plugin as
follows:
```
opam compiler create --switch=. NathanReb:jit-hook-411
```

To make the native toplevel accessible by dune and ocamlfind you will also need to use
[`compiler-libs-opttoplevel`](https://github.com/NathanReb/compiler-libs-opttoplevel).
You can pin it using:
```
opam pin add compiler-libs-opttoplevel.0.1.0 git+https://github.com/NathanReb/compiler-libs-opttoplevel.git#0.1.0
```

And pin `jit` using:
```
opam pin add jit.0.1.2 git+https://github.com/NathanReb/ocaml-jit.git#0.1.2
```

If you want to use `#require` directives in your JIT based toplevel, you will also need
to use [`opttopfind`](https://github.com/NathanReb/opttopfind):
```
opam pin opttopfind.0.1.0 git+https://github.com/NathanReb/opttopfind.git#0.1.0
```

## Running MDX with ocaml-jit

We won't be able to release `ocaml-jit` before some hooks and improvements have been merged into
the compiler. In the meantime, we maintain a branch of MDX built on top of this, you can find it
[here](https://github.com/realworldocaml/mdx/tree/mdx-with-jit).

If you want to use that branch you will need to pin the above mentioned packages along with `jit`
because opam doesn't handle `pin-depends` recursively.
You can also add the following to your project's opam file:
```
pin-depends: [
  [
    "compiler-libs-opttoplevel.0.1.0"
    "git+https://github.com/NathanReb/compiler-libs-opttoplevel.git#0.1.0"
  ]
  [
    "opttopfind.0.1.0"
    "git+https://github.com/NathanReb/opttopfind.git#0.1.0"
  ]
  [
    "jit.0.1.2"
    "git+https://github.com/NathanReb/ocaml-jit.git#0.1.1"
  ]
  [
    "mdx.dev"
    "git+https://github.com/realworldocaml/mdx.git#mdx-with-jit"
  ]
]
```
