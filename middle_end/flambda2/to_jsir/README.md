# Flambda 2 to js_of_ocaml IR translation pass

The `to_jsir` pass is responsible for translating the Flambda 2 IR to `js_of_ocaml`'s IR ([JSIR](../../../external/js_of_ocaml/compiler/lib/code.mli), compiled into
the compiler by [jsoo_imports/dune](jsoo_imports/dune)). This translation enables OCaml code compiled with Flambda 2 optimisations to be executed in JavaScript environments via `js_of_ocaml`, instead of compiling through bytecode. The entry point is [`To_jsir.unit`](to_jsir.mli).

## Usage

The pass is the code generator of `ocamlopt -target js_of_ocaml` (see [`Jscomp`](../../../optcomp/jscomp.mli)), which works like regular `ocamlopt`:

```
ocamlopt -target js_of_ocaml -c foo.ml                           # foo.cmi foo.cmjx foo.cmjo
ocamlopt -target js_of_ocaml -a -o lib.cmjxa foo.cmjx stubs.js   # lib.cmjxa lib.cmja
ocamlopt -target js_of_ocaml -o prog.js lib.cmjxa main.cmjx      # or a.out.js without -o
```

| Native | js_of_ocaml | Contents |
|--------|-------------|----------|
| `.cmx` / `.cmxa` | `.cmjx` / `.cmjxa` | Flambda 2 export information |
| `.o` / `.a` | `.cmjo` / `.cmja` | JavaScript, produced by `js_of_ocaml compile` / `js_of_ocaml link -a` |
| `.s` | `.cmj` | The JSIR of a unit, marshaled; deleted unless `-S` is given |
| C stubs | `.js` stubs | Recorded in `.cmjxa` files and passed to `js_of_ocaml build-runtime` when linking |

The compiler runs the `js_of_ocaml` executable found in its `bin` directory, or in the `PATH`, unless the `OXCAML_JS_OF_OCAML` environment variable is populated.
We can print JSIR with `-djsir`, `-g` makes `js_of_ocaml` keep debug information, and `-verbose` shows the `js_of_ocaml` commands.
It is disallowed to pass in C sources and objects, `-ccopt`/`-cclib`, `-shared` and `-output-obj`.

Options passed in through `-jsoo-opt` are passed through to all `js_of_ocaml` commands. The flags `-jsoo-opt-compile`, `-jsoo-opt-archive`, `-jsoo-opt-runtime` and `-jsoo-opt-link` pass in arguments to one of them only: `js_of_ocaml compile` (for `-c`), `js_of_ocaml link -a` (for `-a` and `-pack`), `js_of_ocaml build-runtime` (the runtime and the `.js` stubs of an executable) and the `js_of_ocaml link` of an executable, respectively. `OCAMLPARAM` accepts them as `jsoo-opt=`, `jsoo-opt-compile=`, etc.


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

Hence `Tag_immediate` and `Untag_immediate` are identities in JSIR: all integers are untagged. Similarly, `Box_number` and `Unbox_number` are also identities, since everything is represented as the `Number` type (with the exception of `Naked_int64`, which are just three `Number`s under the hood).

Beware - immediates are 32 bits in JavaScript, instead of 63 or 31!

## Known limitations

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
- Suppose an `external` takes an unboxed product, e.g. `[#(int * int)]`. In bytecode, this is passed as a single argument, containing a pointer to a pair; however, in JSIR, this will be passed as two arguments, in the same way as for native compilation.
- If the return type for an external is a nested unboxed product such as `[#(#(int * int) * int)]`, bytecode stubs need to return a nested tuple, while JSIR stubs need to return a single flat (i.e. non-nested) tuple containing the unarised arguments. Unlike the parameter passing case above, this is different from native code compilation, where multiple return values are supported to some extent.
- `[@untagged]` and `[@unboxed]` on externals are irrelevant for what JS stubs should look like, since there is no tagging in JSIR and naked integers look just like boxed ones.
