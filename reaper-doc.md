The reaper is a pass that performs inter-procedural dead code removal and unboxing, running after the flambda2 simplify pass.

It works in two parts: first an analysis of all the code, then performing some transformations depending on the result of that analysis.

The analysis
------------
The analysis considers all the variables, and examines what their content could be, and how they can be used in the future. It is control-flow insensitive: in `let x = if b then y else z`, it considers that any value that `y` or `z` could have is a value that `x` could have, forgetting the condition `b`.

We compute, for each variable, its set of *sources* (the points where the value can have been computed), and its set of *usages* (the points where it is used). Furthermore, when a value is a block or a set of closure, we track, for each of its fields/value_slots the possible sources or usages of those fields, and recursively if those fields are themselves blocks.

When a value is exposed beyond the current compilation unit (giving it to a function defined in a different compilation unit, or exporting it in the module of the current compilation unit), or used by an untracked primitive, *XXX tracked primitives* it is considered to escape. An escaping value which is a block has all of its fields which escape as well, since what is happening untracked outside the tracked realm is unknown and could read those fields of the value *XXX ref local slots*.

Likewise, a value that could come from beyond the compilation unit (for instance, a value defined outside the current compilation unit, or a parameter of a function that is exported), or is produced by an untracked primitive, is considered to have unknown sources.

Transformations
---------------
Once the analysis is done, the reaper performs several transformations:

- All values which have no possible sources are considered to occur only in dead code, and code mentionning them is replaced by `Invalid`. For instance, in the following code, we can prove that `x` has no possible source, and replace the whole branch by `Invalid`.
```ocaml
let f () =
  let[@inline never][@local never] g = function
    | None -> 0
    | Some x -> x
  in g None
```

- All values which have no usage are replaced by a poison value (`0` in practice). This poison value can end up being stored in a block, if we know we are never going to read it back. For instance, in the following code, `y` will be replaced with the poison value when building `b`.
```ocaml
let f x y =
  let b = (x, y) in
  let[@inline never][@local never] g a = fst a in
  g b
```

- Some functions might have their calling convention changed. For that, the closure for the function must not escape, and there must only be direct calls to that function. (Note: actually the condition is a bit less strict, as there may be [Indirect_known_arity] calls to the function as well if we are able to identify the closure as the only possible callee.)

- Finally, we perform unboxing and representation changes on the values where we can do it. For that, we require the following:
  - The value is a block or a function, with known usages,
  - Each of these usages only has a single source, which is the value itself,
  - For unboxing, there are a few additional restrictions:
    - Each time the value is stored somewhere, the block in which is it stored must itself be unboxed or have its representation changed,
    - Each time the value is given as parameter to a function, or returned from a function, that function must have its calling convention changed,
    - Symbols may not be unboxed,
    - Functions that are called indirectly may not be unboxed.
  - Besides, blocks that are not closures may currently not have their representation changed, even though they may be unboxed.
