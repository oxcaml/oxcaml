# Using JavaScript Stubs with OxCaml's New Pipeline

## File Types
- `.cmj` - Intermediate IR files (automatically cleaned up)
- `.cmjo` - JavaScript object files
- `.cmja` - JavaScript archive files (linked .cmjo files)
- `.js` - JavaScript stub files (must be provided separately)

## Creating Libraries with External Stubs

When creating a library that uses external JavaScript functions:

1. **Compile the OCaml module:**
   ```bash
   ocamlj -c mylib.ml
   ```
   This creates `mylib.cmjo`

2. **Create the archive:**
   ```bash
   ocamlj -a -o mylib.cmja mylib.cmjo
   ```
   Note: The archive contains only the compiled OCaml code, NOT the stubs

3. **Provide stubs when linking:**
   ```bash
   ocamlj -o program.js mystubs.js mylib.cmja main.ml
   ```
   JavaScript stub files must be provided at link time

## Why Stubs Are Separate

JavaScript stubs are included in the runtime, not in archives. This is because:
- Stubs need to be available globally in the JavaScript runtime
- js_of_ocaml's `build-runtime` command handles stub integration
- Archives (.cmja) contain only linked JavaScript code from .cmjo files

## Example Stub File

```javascript
//Provides: caml_my_function
//Requires: caml_jsstring_of_string
function caml_my_function(ocaml_str) {
  var js_str = caml_jsstring_of_string(ocaml_str);
  console.log("From stub: " + js_str);
  return 0;
}
```

## Best Practices

1. Keep stub files with their corresponding libraries
2. Document which stub files are required for each library
3. Consider providing a Makefile or dune rules that include the stubs
4. Test that your library fails gracefully if stubs are missing