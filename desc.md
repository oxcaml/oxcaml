Title: Fix unsound function argument/return sorts in partial GADT matches

When a parameter pattern of a function matches a GADT constructor, this can refine the sorts of later parameters (say, from `any` to `value`) when typechecking the function. However, if that first parameter was a partial match, then this refinement is not valid as all parameters are "passed at the same time," i.e., one could pass a different GADT constructor which refines the later argument to a different sort (say, from `any` to `bits64`), and the mismatch in calling convention can be observed before a partial match error is raised.

For example, the below program segfaults on native, as `x` gets sort `value` from partial match on `V`, yet can actually be passed as a `bits64`.
```ocaml
type ('a : any) t = V : 'a t | B : ('a : bits64) t
let f : type (a : any). a t -> a -> unit -> a = fun V x () -> x
let i = (Sys.opaque_identity f) B #2L
let () = Gc.full_major ()
```

Instead, `x` should not be considered to be representable because it depends on information from the partial match.

The same issue occurs with optional arguments, as their pattern can introduce equations, yet the caller may omit the argument.

We also must not let the result type's sort be refined, even though a call with a missing constructor raises before ever returning, to avoid a flambda error.

To fix this, we simply drop the equations introduced by a parameter pattern that a caller can bypass — a partial match's, or an optional argument's — before typechecking the rest of the function. Nothing can then depend on them: sorts that relied on them now fail the ordinary representability checks, with the ordinary errors. Equations from total matches are kept, as every caller must establish them.

This is approximate in two ways. The function's body could soundly use a partial match's equations (it only runs once the match succeeds), but sees them dropped like everything else. And exhaustiveness must be decided at the parameter, before typechecking the rest of the function can refute more constructors (the usual exhaustiveness warning is unaffected).

Dropping the equations at the source, rather than checking uses after the fact, also means a later pattern's equations cannot capture what a partial match implied and pass it along (see the `f_eq` test, which used to segfault this way).

This is the jkind/sort analogue of the bug fixed by #6356. H/t @Ekdohibs for the report of both bugs.
