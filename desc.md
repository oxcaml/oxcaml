# Fix bytecode aliasing bug for unboxed products in mixed blocks

## Problem

In bytecode, reading or writing an unboxed product from/to a mutable mixed block field causes observable aliasing. This makes bytecode behave differently from native code.

### Example (read aliasing)

```ocaml
type r = #{ i : int; j : unit }
type t = { mutable r : r }

let () =
  let t = { r = #{ i = 1; j = () } } in
  let r = t.r in
  set_idx t (.r.#i) 2;
  Printf.printf "%d\n" r.#i
  (* native: prints 1 *)
  (* bytecode (before fix): prints 2 *)
```

### Example (write aliasing)

```ocaml
let () =
  let r = #{ i = 1; j = () } in
  let t = { r = #{ i = 0; j = () } } in
  t.r <- r;
  set_idx t (.r.#i) 2;
  Printf.printf "%d\n" r.#i
  (* native: prints 1 *)
  (* bytecode (before fix): prints 2 *)
```

### Root Cause

In native code, unboxed products are truly unboxed—flattened into registers or stored inline. In bytecode, unboxed products are represented as heap-allocated blocks for simplicity.

When `Pmixedfield` reads an unboxed product, it returns a pointer to the nested block. When `Psetmixedfield` writes an unboxed product, it stores the pointer directly. This causes aliasing: mutations via `set_idx` affect both the field and any copies.

## Solution

Deep copy unboxed products when reading from or writing to mixed block fields. The `copy_unboxed_product` function generates Blambda code that:
- For non-product layouts: returns the expression unchanged
- For product layouts: allocates a fresh block and recursively copies each field

### Files Modified

- `lambda/lambda.mli` — Export `layout_of_mixed_block_shape`
- `bytecomp/blambda_of_lambda.ml` — Add `copy_unboxed_product` helper and use it in `Pmixedfield`/`Psetmixedfield` handling

### Test

Added `testsuite/tests/mixed-blocks/unboxed_product_no_alias.ml` with test cases for:
- Simple unboxed records
- Nested unboxed records
- Deeply nested unboxed records (3 levels)
- Mixed blocks with non-value types (float#)
- All of the above with multi-field outer records
- Both read and write aliasing scenarios
