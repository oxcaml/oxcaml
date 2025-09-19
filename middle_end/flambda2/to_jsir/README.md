# Flambda 2 to js_of_ocaml IR translation pass

The `to_jsir` pass is responsible for translating the Flambda 2 IR to `js_of_ocaml`'s IR ([JSIR](jsoo_imports/code.mli)). This translation enables OCaml code compiled with Flambda 2 optimisations to be executed in JavaScript environments via `js_of_ocaml`, instead of compiling through bytecode. The entry point is [`To_jsir.unit`](to_jsir.mli).

## Number representations
| Flambda kind       | JSIR representation                                                                                                                                                                                                |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `Tagged_immediate` | Untagged 32-bit integer (via `Number`)                                                                                                                                                                             |
| `Naked_immediate`  | Untagged 32-bit integer (via `Number`)                                                                                                                                                                             |
| `Naked_nativeint`  | Untagged 32-bit integer (via `Number`)                                                                                                                                                                             |
| `Naked_int32`      | Untagged 32-bit integer (via `Number`)                                                                                                                                                                             |
| `Naked_int64`      | Untagged 64-bit integer (via [`MlInt64`](https://github.com/oxcaml/js_of_ocaml/blob/master/runtime/js/int64.js) )                                                                                                  |
| `Naked_float32`    | Untagged 64-bit float (via `Number`), where operations are performed in 64-bit precesion then [rounded down](https://github.com/oxcaml/js_of_ocaml/blob/master/runtime/js/float32.js) to the nearest 32-bit result |
| `Naked_float`      | Untagged 64-bit float (via `Number`)                                                                                                                                                                               |
Hence `Tag_immediate` and `Untag_immediate` are identities in JSIR: all integers are tagged. Similarly, `Box_number` and `Unbox_number` are also identities, since everything is represented as the `Number` type (with the exception of `Naked_int64`, which are just three `Number`s under the hood).

Beware - immediates are 32 bits in JavaScript, instead of 63 or 31!

## Known quirks and limitations

### Unsupported Flambda features
The following are not yet supported in JSIR translation, and will raise an exception in the compiler when encountered:

1. Smallints: `int8` and `int16`
2. SIMD/vector types: `vec128`, `vec256` and `vec512`
3. Block indices: `Read_offset` and `Write_offset`

### WASM
The produced JSIR is not suitable for WASM compilation via `wasm_of_ocaml`, despite the two sharing the same IR.
In particular, there are places within the `to_jsir` code that assumes JavaScript - these are marked with comments.

## External stub behaviour
For `external` declarations containing both bytecode and native names, the JSIR pass uses the **bytecode** name for most cases, and such JavaScript stubs are compatible with the existing `js_of_ocaml` stubs.
However, when **unboxed products** are involved (as arguments or return type), we instead use the **native** name. In this case, we use a slightly different convention to that of both existing JS/bytecode stubs and native C stubs:
- Suppose an `external` takes an unboxed product, e.g. [#(int * int)]. In bytecode, this is passed as a single argument, containing a pointer to a pair; however, in JSIR, this will be passed as two arguments, in the same way as for native compilation.
- If the return type for an external is a nested unboxed product such as [#(#(int * int) * int)], bytecode stubs need to return a nested tuple, while JSIR stubs need to return a single flat (i.e. non-nested) tuple containing the unarised arguments. Unlike the parameter passing case above, this is different from native code compilation, where multiple return values are supported to some extent.
- `[@untagged]` and `[@unboxed]` on externals are irrelevant for what JS stubs should look like, since there is no tagging in JSIR and naked integers look just like boxed ones.
