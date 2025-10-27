# ocamlj (js_of_ocaml) Integration TODO List

## Correctness Issues

1. **Put code IDs into global symbol table (for classic mode)**
   - Classic mode can make direct calls to functions in different compilation units without the callee
   - Currently only symbols are saved, not code IDs
   - Test case: Compile hello world with ocamlj-compiled stdlib visible through -I flag → triggers Not_found in To_jsir_env when print_endline is called directly
   - Solution needed: Store the exact function closure that uses this code_id (registration point is far from where code_id is added to To_jsir_env)
   - Interim solution: classic mode is always disabled in ocamlj!

2. **Write tests + user-facing docs for unboxed/untagged types**
   - Document external behaviour when unboxed tuples/untagged/etc are involved
   - Add comprehensive test coverage

3. **CI tests**
   - Requires integrating js_of_ocaml into CI pipeline
   - Currently blocked on internal js_of_ocaml availability

4. **Fix float array runtime check issue**
   - Location: `external/js_of_ocaml/compiler/lib/mlvalue.ml`
   - Problem: When constructing an `any array`, Flambda checks at runtime if the value is a boxed float to determine array type
   - Issue: The value can be `undefined` in JavaScript, causing errors when checking field 0
   - Currently using optional chaining as workaround

## Usability and Performance

1. **Improve tail call optimization**
   - Many base tests fail due to stack overflow
   - JavaScript doesn't have native TCO, needs trampolining or other techniques

2. **Reduce code bloat** (see detailed analysis below)

3. **Implement source maps**
   - Currently Flambda2 offsets and reachable names are ignored in JSIR translation
   - Need to propagate debugging information through the pipeline

4. **Performance benchmarking**
   - Run comprehensive benchmarks comparing ocamlj vs legacy js_of_ocaml
   - Identify optimization opportunities

5. **Stack traces for fatal errors**
   - Stack traces don't appear when using ocamlj-compiled stdlib
   - It seems this is because Printexc sets a handler that doesn’t print the trace, not sure why it works in native code...

6. **Enable more Flambda2 inlining**
   - Currently disabled due to closure projection limitations
   - Cannot project value/function slots from closures other than "itself"
   - See `check_my_closure` in `to_jsir_primitive.ml` - causes fatal error if attempted
   - Solution needed: Implement cross-closure slot projection in js_of_ocaml

## Miscellaneous

1. **Fix Flambda scoping in environment**
   - Location: CR in `to_jsir.ml`
   - Issue: Environment keeps symbols but doesn't properly flush variables to respect Flambda scoping invariants
   - Impact: Potential incorrect variable scoping in generated code

2. **Prune excessive events**
   - Many Flambda events become JavaScript comments
   - No runtime performance impact but increases file size
   - Makes generated code harder to read

3. **Refactor closure and code ID handling**
   - Current: Single code instance shared across uses
   - Needed: Each use of code should result in a new copy
   - Would enable better optimization opportunities

4. Replace `caml_register_symbol` and `caml_get_symbol` with just adding the symbol (without compilation unit) to the `runtime` object.

## Low Priority

1. **SIMD emulation**

2. **WASM support**
   - Leverage JSIR for WebAssembly compilation
   - Could provide better performance than JavaScript

3. **Better Flambda2 intrinsics support**
   - Use JavaScript [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) for int64 operations
   - Map more Flambda2 intrinsics to efficient JavaScript equivalents

## Code Bloat Analysis

Currently JSIR-compiled binaries are huge. We believe significant reasons are how we handle the linking of symbols and something about the JSOO linker (see “I lied” below).

**Why care about symbols?**
The ocamlj compiler produces .cmjx artifacts, which are very similar to (and in fact have the same format as) .cmx files. These contain information that the Flambda simplifier can use to, e.g., perform cross-file inlining. However this also allows the simplifier to generate code containing symbols that were defined outside the current compilation unit. This is problematic, because the flambda \-\> JSIR pass has no idea what the variable corresponding to this symbol is\!

**What is being done right now?**
We maintain a global symbol table `caml_symbols`, defined in `ocamlj.js` within the JSOO runtime. Every time we define a symbol, we [register it to the global table](https://github.com/oxcaml/oxcaml/blob/630d749d3abcaebc3445c939a3a474a709f769ec/middle_end/flambda2/to_jsir/to_jsir_env.ml#L126). Dually, when we attempt to look up a symbol defined outside the current compilation unit, [we fetch it from this table](https://github.com/oxcaml/oxcaml/blob/630d749d3abcaebc3445c939a3a474a709f769ec/middle_end/flambda2/to_jsir/to_jsir_env.ml#L200).

**Why is this bad?**
This means we need string constants for every single symbol ever defined, each of which can get pretty long\! For a given compilation unit, only a small portion of the defined symbols will actually be used, and an even smaller number will be used by something outside this compilation unit. This means there is no reason to add everything here to the symbol table \- this is bad for both code size and performance.

I used to think this might influence DCE as well, but I’m not so sure anymore since cross-module DCE can’t work on already-compiled .[cmj.js](http://cmj.js) files, and within a single module, every symbol I believe should be used (either directly or by inclusion to the exported first-class module).

**How should we fix this?**
Before we added `caml_symbols`, js\_of\_ocaml actually already had an existing table keeping track of first-class modules. However Jerome is smarter than I am, and made sure to keep track of the module dependencies required for each compilation unit. This materialises as a header on top of each (pre-linking) javascript file, which looks like:

```javascript
//# unitInfo: Provides: Random
//# unitInfo: Requires: Nativeint, Sys
```

This is parsed into a Unit\_info.t, which is used by the js\_of\_ocaml linker to only link in used modules. We could have a similar stanza `//\#unitInfo: ProvidesSymbols:` or something that tracks this information, and arrange for the linker to prune out unneeded inserts. We could also maybe collapse the symbol name strings into something shorter when not in pretty-printing mode.

### Linker Over-inclusion Problem

The js_of_ocaml linker is including the entire standard library even for simple programs.

- **Expected**: Hello world should only depend on Stdlib and CamlinternalFormatBasics
- **Actual**: Entire standard library is linked (verify with `--debug=link`)
- **Impact**: Massive unnecessary code bloat for small programs
- **Investigation needed**: Understand why the linker's dead code elimination isn't working

This contradicts the design where Unit_info.t tracks dependencies and the linker should only include used modules. The dependency tracking exists but appears to be broken or bypassed.
