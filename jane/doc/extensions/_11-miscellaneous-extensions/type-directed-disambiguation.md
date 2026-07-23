---
layout: documentation-page
collectionName: Miscellaneous extensions
title: Type-directed disambiguation
---

# Type-directed disambiguation

OCaml already has some type-directed disambiguation. For example, array literals
are disambiguated among `array`s, `iarray`s, and `floatarray`s. More
disambiguation can be enabled with `-extension type_directed_disambiguation`.

## Type-directed disambiguation of integer literals

Integer literals with no suffix and no `#` prefix can be disambiguated if their
type can be inferred exactly. Consider the following two expressions:
```
let four = Int32.add 2 2;;
let my_array = [| #10L; 20; 30 |];;
```
Both of them are accepted, but `my_array` will emit warnings with `-principal`.

A typical pitfall of non-principal type-inference applies: Whether a bare
integer literal is disambiguated or inferred to be `int` can depend on the order
of type-inference.
```
# let my_array = [| 10; #20L; 30 |];;
                        ^^^^
Error: This constant has type "int64#" but an expression was expected of type
         "int"
```

Disambiguation also works in patterns.
