# Flambda 2 to js_of_ocaml IR translation pass

The `to_jsir` pass is responsible for translating the Flambda 2 IR to `js_of_ocaml`'s IR ([JSIR](../../../external/js_of_ocaml/compiler/lib/code.mli), compiled into
the compiler by [jsoo_imports/dune](jsoo_imports/dune)). This translation enables OCaml code compiled with Flambda 2 optimisations to be executed in JavaScript environments via `js_of_ocaml`, instead of compiling through bytecode. The entry point is [`To_jsir.unit`](to_jsir.mli).

## Usage

The pass is the code generator of `ocamlopt -target js_of_ocaml` and `ocamlopt -target wasm_of_ocaml` (see [`Jscomp`](../../../optcomp/jscomp.mli)), which work like regular `ocamlopt`:

```
ocamlopt -target js_of_ocaml -c foo.ml                               # foo.cmi foo.cmx-js foo.cmo-js
ocamlopt -target js_of_ocaml -a -o lib.cmxa-js foo.cmx-js stubs.js   # lib.cmxa-js lib.cma-js
ocamlopt -target js_of_ocaml -o prog.js lib.cmxa-js main.cmx-js      # or a.out.js without -o

ocamlopt -target wasm_of_ocaml -c foo.ml                                    # foo.cmi foo.cmx-wasm foo.cmo-wasm
ocamlopt -target wasm_of_ocaml -a -o lib.cmxa-wasm foo.cmx-wasm stubs.wat   # lib.cmxa-wasm lib.cma-wasm
ocamlopt -target wasm_of_ocaml -o prog.js lib.cmxa-wasm main.cmx-wasm       # prog.js prog.assets/
```

This compiles each unit to JavaScript (WebAssembly) separately, which is the fastest way to build. With `-link-jsir`, the JSIR of the units is linked into one program first, which `js_of_ocaml` optimises as a whole (see [JSIR linking](#jsir-linking)):

```
ocamlopt -target js_of_ocaml -link-jsir -c foo.ml                              # foo.cmi foo.cmx-js foo.cm-jsir-js
ocamlopt -target js_of_ocaml -link-jsir -a -o lib.cmxa-js foo.cmx-js stubs.js  # lib.cmxa-js lib.cma-jsir-js
ocamlopt -target js_of_ocaml -link-jsir -o prog.js lib.cmxa-js main.cmx-js     # prog.js
```

| Native | js_of_ocaml | wasm_of_ocaml | Contents |
|--------|-------------|---------------|----------|
| `.cmx` / `.cmxa` | `.cmx-js` / `.cmxa-js` | `.cmx-wasm` / `.cmxa-wasm` | Flambda 2 export information |
| `.o` / `.a` | `.cmo-js` / `.cma-js` | `.cmo-wasm` / `.cma-wasm` | JavaScript or WebAssembly, produced by `compile` / `link -a` |
| `.s` | `.cm-jsir-js` | `.cm-jsir-wasm` | The JSIR of a unit, marshaled (the same format, recording the target); deleted unless `-S` is given. With `-link-jsir`, this is the object file of a unit instead of `.cmo-js` (`.cmo-wasm`), and `<exe>.cm-jsir-js` (`<exe>.cm-jsir-wasm`) is the JSIR of the linked program, deleted unless `-S` is given |
| | `.cma-jsir-js` | `.cma-jsir-wasm` | The JSIR of the units of a library (a marshaled list), which `-link-jsir` produces and reads instead of `.cma-js` (`.cma-wasm`) |
| startup file | `<exe>.runtime.js` | `<exe>.runtime.wasma` | The output of `build-runtime`; deleted unless `-dstartup` is given |
| C stubs | `.js` stubs | `.js`, `.wat`, `.wast` and `.wasm` stubs | Recorded in library files and passed to `build-runtime` when linking |
| executable | `prog.js` | `prog.js` and `prog.assets/` | The output of `link`; the directory holds the WebAssembly code that `prog.js` loads |

The compiler runs the `js_of_ocaml` (`wasm_of_ocaml`) executable found in its `bin` directory, or in the `PATH`, unless the `OXCAML_JS_OF_OCAML` (`OXCAML_WASM_OF_OCAML`) environment variable is populated.
Every `wasm_of_ocaml` command is given `--enable=portable-int`, as we use 63-bit integers.
We can print JSIR with `-djsir`, and `-verbose` shows the `js_of_ocaml` commands.
C sources and objects and `-ccopt`/`-cclib` are ignored (with a warning under `-verbose`).

`-jsoo-effects` selects the implementation of effect handlers, passed as `--effects` to `compile` and `build-runtime`: `disabled`, `cps` or `double-translation` (the default) for `js_of_ocaml`, and `disabled`, `cps`, `jspi` (the default) or `native` for `wasm_of_ocaml`.
Only `jspi` (and `disabled`, which uses the same runtime) has a precompiled `wasm_of_ocaml` runtime for 63-bit integers; with `cps` or `native`, `build-runtime` compiles the runtime from its sources every time an executable is linked, which is slow.

Options passed in through `-jsoo-opt` are passed through to all `js_of_ocaml` commands. The flags `-jsoo-opt-compile`, `-jsoo-opt-archive`, `-jsoo-opt-runtime` and `-jsoo-opt-link` pass in arguments to one of them only: `js_of_ocaml compile` (for `-c`), `js_of_ocaml link -a` (for `-a`), `js_of_ocaml build-runtime` (the runtime and the stubs of an executable) and the `js_of_ocaml link` of an executable, respectively. They apply to the same `wasm_of_ocaml` commands. `OCAMLPARAM` accepts them as `jsoo-opt=`, `jsoo-opt-compile=`, etc., and also `target=` and `jsoo-effects=`.

## Debug information
With `-g`, the JSIR of a unit carries the debug information that `js_of_ocaml` otherwise reads from bytecode:
- debug events (source locations), at the start of functions, before calls (and after those that are not tail calls), before raises, and before the registration of closure symbols;
- the names of the variables and functions of the source, which JSIR files record as `var_names` in [`Code.jsir_file`](../../../external/js_of_ocaml/compiler/lib/code.mli), and which the JSIR linker keeps.

The debug location of code inlined by Flambda 2 is the one it had in the function it comes from.
With `-jsir-inlined-frames`, we additionally record the call sites that the function was inlined through as a single string.
For example, we might record `list.ml<-foo.ml:12:4<-bar.ml:40:2` for code of `list.ml` inlined at line 12, column 4 of `foo.ml`, itself inlined at line 40 of `bar.ml`.
This is a specific convention to JSIR, and as such normal debuggers will not be able to find such source files.

`-g` does not change the `js_of_ocaml` commands: what `js_of_ocaml` does with this information is chosen with `-jsoo-opt`, for example `--source-map`, `--pretty`, or `--debuginfo` (which puts the locations in comments in the generated JavaScript).

For source maps, pass `-jsoo-opt --source-map-inline`.
`--source-map` (a separate `.map` file) only works for the link of an executable, as `js_of_ocaml link` ignores source maps that are not inline in the files it is given. To get the source map of an executable in a separate file:

```
-jsoo-opt-compile --source-map-inline -jsoo-opt-archive --source-map-inline -jsoo-opt-runtime --source-map-inline -jsoo-opt-link --source-map
```

## JSIR linking
By default, `js_of_ocaml compile` runs on each unit when it is compiled, and `js_of_ocaml link` links the results: units can be compiled in parallel and are only recompiled when they change, but `js_of_ocaml` cannot optimise across units, and symbols of other units are looked up at runtime.

`-link-jsir` trades link time for faster and smaller code, as `js_of_ocaml` does for bytecode executables. It must be given consistently:
- when compiling a unit (`-c`), `js_of_ocaml` is not run: the object file is the JSIR of the unit (`.cm-jsir-js`, `.cm-jsir-wasm`);
- when creating a library (`-a`), the JSIR of its units is put in a `.cma-jsir-js` (`.cma-jsir-wasm`) file;
- when linking an executable, the JSIR of all its units is linked into the JSIR of one program by [`Jsir_linker`](jsir_linker.mli), on which the compiler runs `js_of_ocaml compile` (to which `-jsoo-opt-compile` and `-jsoo-effects` now apply), followed by `build-runtime` and `link` as usual.

The units are linked in initialisation order, as determined by the compiler's linker (`Optlink`) from the `.cmx-js` and `.cmxa-js` files, so each unit comes after the units it imports.
The JSIR linker renumbers the variables and block addresses of each unit so that they are disjoint, and replaces the `Stop`s of the toplevel code of each unit (reachable from its entry block without entering closures) with a branch to the entry block of the next unit.

The JSIR of a unit refers to the symbols of other units through the runtime's symbol table: a unit binds `caml_get_symbol` (or `caml_get_global`, for a module block) to a fresh variable at the start of its entry block, and registers its own symbols with `caml_register_symbol` (`caml_register_global` for its module block) where they are defined.
JSIR files record these variables (`symbol_imports` and `symbol_exports` in [`Code.jsir_file`](../../../external/js_of_ocaml/compiler/lib/code.mli)), and the linker substitutes the importing unit's variable with the exporting unit's, deleting the `caml_get_symbol` binding.
The `caml_register_global` calls are deleted too unless `-linkall` is given (as `js_of_ocaml` does for bytecode executables), so that `js_of_ocaml` can remove the fields of module blocks that are not used.

A unit whose toplevel code never exits normally (it always raises) makes the units after it unreachable, and the linker leaves them out.

## Number representations
The JSIR produced for `-target js_of_ocaml` and `-target wasm_of_ocaml` differs in a few places, chosen by [`To_jsir_target`](to_jsir_target.mli).
For WebAssembly, we use wasm_of_ocaml's portable integer representation, where integers have the same widths as in native code: Flambda compiles for a 64-bit machine, and `Targetint` and `Targetnativeint` are 63 and 64 bits wide.

| Flambda kind       | JavaScript                                                                                                                                                                                       | WebAssembly                                            |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------|
| `Tagged_immediate` | Untagged 32-bit integer (via `Number`)                                                                                                                                                           | Untagged 63-bit integer                                |
| `Naked_immediate`  | Untagged 32-bit integer (via `Number`)                                                                                                                                                           | Untagged 63-bit integer                                |
| `Naked_int8`       | Untagged 32-bit integer (via `Number`), always kept sign-extended to 8 bits                                                                                                                      | Untagged 63-bit integer, always kept sign-extended to 8 bits  |
| `Naked_int16`      | Untagged 32-bit integer (via `Number`), always kept sign-extended to 16 bits                                                                                                                     | Untagged 63-bit integer, always kept sign-extended to 16 bits |
| `Naked_nativeint`  | Untagged 32-bit integer (via `Number`)                                                                                                                                                           | Boxed 64-bit integer                                   |
| `Naked_int32`      | Untagged 32-bit integer (via `Number`)                                                                                                                                                           | Boxed 32-bit integer                                   |
| `Naked_int64`      | Untagged 64-bit integer (via [`MlInt64`](https://github.com/oxcaml/js_of_ocaml/blob/master/runtime/js/int64.js) )                                                                                | Boxed 64-bit integer                                   |
| `Naked_float32`    | Untagged 64-bit float (via `Number`), where operations are performed in 64-bit precesion then [rounded down](https://github.com/oxcaml/js_of_ocaml/blob/master/runtime/js/float32.js) to the nearest 32-bit result | Boxed 32-bit float                                     |
| `Naked_float`      | Untagged 64-bit float (via `Number`)                                                                                                                                                             | Boxed 64-bit float                                     |

Hence `Tag_immediate` and `Untag_immediate` are identities in JSIR: all integers are untagged. Similarly, `Box_number` and `Unbox_number` are also identities: in JavaScript, everything is represented as the `Number` type (with the exception of `Naked_int64`, which are just three `Number`s under the hood), and in WebAssembly, boxed numbers are structs at the level of JSIR, which wasm_of_ocaml unboxes where it can.

Beware - immediates are 32 bits in JavaScript, instead of 63 or 31!

Smallints (`Naked_int8` and `Naked_int16`) follow the same convention as bytecode and Cmm, where they are stored sign-extended.
This means that JavaScript stubs receive exactly the same values for `int8#`/`int16#` arguments as they do in bytecode, and must likewise return sign-extended values.
Conversions between smallints and other integers go through ints (e.g. `caml_int32_of_int`), which are identities in JavaScript.

## Block indices
JSIR blocks have the same flattened and reordered layout as native blocks. However, these blocks
are represented as JavaScript arrays, and thus each (unarised) element always occupies one slot.
In particular, arrays are never packed, and so always occupy the same number of slots as their length.

We treat every slot as being 8 bytes wide. This means that the indexing behaviour differs
compared to native code: for example, accessing the second element of a packed `int32# array` will
require offset 4 in native code, but offset 8 in JSIR.

External pointers (block index accesses with a null base) are unsupported as in bytecode.
The JS runtime primitive `caml_check_idx_base` raises `Failure` when such an access is attempted.

## Effect handlers
The effects backend of `js_of_ocaml` (`--effects`) must be the same for every unit of a program (including the standard library) and for its runtime,
as `js_of_ocaml link` fails otherwise.
The backend can be chosen with `-jsoo-effects {disabled|cps|double-translation}` (`jsoo-effects=` in `OCAMLPARAM`), which the compiler passes to `js_of_ocaml compile` and `js_of_ocaml build-runtime`.
The default is `double-translation`, which is what the installed standard library is built with. Selecting another effects backend thus requires a standard
library built with the same backend.
This does not apply with `-link-jsir`: the JSIR of a unit does not depend on the effects backend, which is only chosen when linking, so the installed standard library can be used with any of them.
With `-jsoo-effects disabled`, performing an effect or installing a handler fails at runtime with `Failure "Effect handlers are not supported"`.

## Known limitations

### Unsupported Flambda features
The following are not yet supported in JSIR translation, and will raise an exception in the compiler when encountered:

1. SIMD/vector types: `vec128`, `vec256` and `vec512`

### Partial applications of functions of void parameters
Void parameters are erased in JSIR, so a function all of whose parameters are void is a closure of arity 0.
When the callee is unknown, the generic application of `js_of_ocaml` and `wasm_of_ocaml` (`caml_call_gen`, `caml_apply0`) cannot tell a partial application supplying only some of its void arguments from a full one, and evaluates the function immediately (native and bytecode defer it).
For example, given `let[@inline never] g (v : void) (w : void) = print_string "g"; 1` and `let[@inline never] partial2 (k : void -> void -> int) = let p = k (void ()) in print_string "a"; p (void ())`, `print_int (partial2 g)` prints `ga1` instead of `ag1` (and fails at runtime with the JavaScript target).
Functions with at least one non-void parameter are unaffected.

## External stub behaviour
For `external` declarations containing both bytecode and native names, the JSIR pass uses the **bytecode** name for most cases, and such JavaScript stubs are compatible with the existing `js_of_ocaml` stubs.
However, when **unboxed products** are involved (as arguments or return type), we instead use the **native** name. In this case, we use a slightly different convention to that of both existing JS/bytecode stubs and native C stubs:
- Suppose an `external` takes an unboxed product, e.g. `[#(int * int)]`. In bytecode, this is passed as a single argument, containing a pointer to a pair; however, in JSIR, this will be passed as two arguments, in the same way as for native compilation.
- If the return type for an external is a nested unboxed product such as `[#(#(int * int) * int)]`, bytecode stubs need to return a nested tuple, while JSIR stubs need to return a single flat (i.e. non-nested) tuple containing the unarised arguments. Unlike the parameter passing case above, this is different from native code compilation, where multiple return values are supported to some extent.
- `[@untagged]` and `[@unboxed]` on externals are irrelevant for what JS stubs should look like, since there is no tagging in JSIR and naked integers look just like boxed ones.

## Optimisation hints
The bytecode compiler (from OCaml 5.6) records optimisation hints alongside the bytecode, which JSOO reads in `parse_bytecode.ml` and uses to specialise the generated code: on external calls (`Extern`), closures, array lengths (`Vectlength`) and block allocations (`mutability`).
The JSIR pass attaches the same hints, computed from Flambda instead (see [`Optimization_hint`](../../../external/js_of_ocaml/compiler/lib/optimization_hint.mli)):

| Hint | Attached to |
|------|-------------|
| `Vectlength` kind | `Array_length`: `Float` for flat float arrays, `Value` for every other array (an ordinary block in JSIR), `Generic` when Flambda does not know. |
| Closure hints | Every `Closure`, from the metadata of its code: the representation of its parameters and result (from the arities), its `inline` attribute (`Available_inline` is `Hint_inline`) and whether it is a functor. |
| `Hint_unsafe` | String, bytes and bigstring accesses of 16 bits or more (`String_or_bigstring_load`, `Bytes_or_bigstring_set`). Flambda inserts the bounds checks itself, so these accesses are always unsafe and call the unchecked runtime primitives (`caml_string_get16u`, ...), as `js_of_ocaml` does for bytecode accesses with this hint. Signed accesses load the unsigned value and sign-extend it. The runtime only provides the unchecked 16- and 64-bit bigstring accessors for OCaml >= 5.6, so those still call the checked ones. |
| `Hint_bigarray` | `Bigarray_load`, `Bigarray_set` and byte accesses to bigstrings (`caml_ba_get_raw_unsafe`, `caml_ba_set_raw_unsafe`), giving the kind and layout of the bigarray. |
| `Hint_int` | Comparisons of unboxed `int64`s, `int32`s and `nativeint`s. |
| `Hint_primitive` | Not emitted yet. Bytecode attaches it to the calls of externals whose native name differs from the bytecode one, with that name and the representations of its arguments and result. Flambda does not keep the native name right now, and JSOO does not use this hint yet, so we defer this until later. |
