'`ppxlib_jane`'
===============

A library for use in ppxes for constructing and matching on ASTs corresponding to the
augmented parsetree that is recognized by the [Jane Street OCaml compiler][JaneStreetOCaml].

ASTs constructed using this library are compatible with the standard OCaml compiler. Any
syntax change known to this library is encoded as attributes, and the standard OCaml
compiler's interpretation of the ASTs constructed by these library (which amounts to
ignoring the attributes) is reasonable. That is, we only expose "unsurprising" things in
this library. For example, if you construct an *n*-ary function using this library, the
standard OCaml compiler will interpret it as *n* nested unary functions in the normal way.

Likewise, ppxes that use this library to match on Jane Street ASTs can also be used with
the standard OCaml compiler. (The Jane Street AST cases of the match will just never be
triggered when using the standard OCaml compiler.)

This is how we intend this library to fit into the broader ppx ecosystem:

```
+-------------+       +--------------+      +------------------+
|             |       |              |      |                  |
|    ppxes    +------>|    ppxlib    +----->+ compiler libs,   |
|             |       |              |      | ppxlib_ast, etc. |
+------+------+       +--------------+      |                  |
       |                                    +---------+--------+
       |              +---------------+               ^
       |              |               |               |
       +------------->+  ppxlib_jane  +---------------+
                      |               |
                      +---------------+
```

That is, there is no dependency between `ppxlib` and `ppxlib_jane`, and ppx authors
are free to use `ppxlib_jane` if they want to construct AST nodes recognized by the
Jane Street OCaml compiler.

[JaneStreetOCaml]: https://github.com/ocaml-flambda/flambda-backend
