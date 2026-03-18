# CamlinternalQuote Code Review

## Summary

| # | File | Line(s) | Bug |
|---|------|---------|-----|
| 1 | camlinternalQuote.ml | 1463 | `Int32` missing `l` suffix |
| 2 | camlinternalQuote.ml | 1464 | `Int64` missing `L` suffix |
| 3 | camlinternalQuote.ml | 1465 | `Nativeint` missing `n` suffix |
| 4 | camlinternalQuote.ml | 1582 | Guard keyword `with` instead of `when` |
| 5 | camlinternalQuote.ml | 1490, 1566 | Negative constants not parenthesized |
| 6 | camlinternalQuote.ml | 1701, 2408 | `TypeAlias` missing `'` on type variable |
| 7 | camlinternalQuote.ml | 1667-1678 | `TypeUnboxedTuple` missing `#` prefix |
| 8 | camlinternalQuote.ml | 1704-1711 | `VClosed` variant drops "present" tags |
| 9 | camlinternalQuote.ml | 1849-1864 | `Fun` printing has mismatched format boxes |
| 10 | camlinternalQuote.ml | 1996 | `Src_pos` prints as `.` |
| 11 | camlinternalQuote.ml | 140-148 | `base_name` skips keyword parenthesization |
| 12 | camlinternalQuote.ml | 145 | `base_name` uses `Int32` not `int` for suffix detection |
| 13 | translquote.ml | 3822-3839 | `env_poly` never cleared between quotations |
| 14 | translquote.ml | multiple | `with`/`without` pairs not exception-safe |
| 15 | translquote.ml | 2331, 2380, etc. | `raise Exit` escapes with no diagnostic |
| 16 | translquote.ml | 3370 | Mode annotations silently dropped (has FIXME) |

## camlinternalQuote.ml

### 1. Boxed `Int32` printed without `l` suffix (line 1463)

```ocaml
| Int32 n -> pp fmt "%ld" n
```

`%ld` prints the digits only. OCaml literal syntax requires `42l`. Compare with the
correct unboxed version at line 1468: `pp fmt "#%ldl" n`.

**Fix:** `pp fmt "%ldl" n`

### 2. Boxed `Int64` printed without `L` suffix (line 1464)

```ocaml
| Int64 n -> pp fmt "%Ld" n
```

Same issue. OCaml requires `42L`. The unboxed version at line 1469 is correct:
`pp fmt "#%LdL" n`.

**Fix:** `pp fmt "%LdL" n`

### 3. Boxed `Nativeint` printed without `n` suffix (line 1465)

```ocaml
| Nativeint n -> pp fmt "%nd" n
```

OCaml requires `42n`. The unboxed version at line 1470 is correct: `pp fmt "#%ndn" n`.

**Fix:** `pp fmt "%ndn" n`

### 4. Guard keyword printed as `with` instead of `when` (line 1582)

```ocaml
| Some guard -> pp fmt "@ with@ %a" (print_exp_with_parens env) guard
```

OCaml match syntax is `| pat when guard -> expr`. Every guarded case in every printed
quotation produces unparseable output.

**Fix:** `pp fmt "@ when@ %a"`

### 5. Negative constants not parenthesized in argument positions (lines 1490, 1566)

`print_pat_with_parens` lists `PatConstant _` as never needing parentheses (line 1490).
`print_exp_with_parens` lists `Constant _` the same way (line 1566). So:

- Pattern `Foo (-42)` prints as `Foo -42` (parsed as `Foo` minus `42`)
- Expression `f (-42)` prints as `f -42` (parsed as `f` minus `42`)
- Similarly affects negative `Int32`, `Int64`, `Nativeint`, and negative floats

**Fix:** These `_with_parens` functions need to check whether the constant is negative
and parenthesize if so.

### 6. `TypeAlias` printed without `'` on the type variable (lines 1701-1702, 2408-2410)

The `.mli` says `alias : t -> Var.Type_var.t -> t`, but the construction converts the
variable to a bare `Name.t`:

```ocaml
let alias typ tv =
  let+ t = typ in
  Ast.TypeAlias (t, Var.Type_var.name tv)  (* drops the var, keeps just the string *)
```

And printing uses `Name.print`, which prints just the string:

```ocaml
| TypeAlias (ty, tv) ->
  pp fmt "%a@ as@ %a" (print_core_type env) ty Name.print tv
```

This prints `int as a` instead of `int as 'a`. The `'` prefix is lost because
`Var.Type_var.name` returns the raw name and `Name.print` doesn't add it.

**Fix:** Either store `Var.Type_var.t` in the AST (and use `Var.Type_var.print`), or
prepend `'` in the printer.

### 7. `TypeUnboxedTuple` printed identically to `TypeTuple` (lines 1667-1678)

The code is copy-pasted from `TypeTuple` with a comment
`(* possibly incorrect way of displaying unboxed tuples *)`. In OxCaml, unboxed tuples
use `#(...)` syntax. Compare with `PatUnboxedTuple` (line 1506) which correctly prints
`#(...)`, and `Unboxed_tuple` expressions (line 1981) which also print `#(...)`.

**Fix:** Print with `#` prefix, matching the pattern and expression printers.

### 8. `VClosed` variant type loses "present" tags (lines 1704-1711)

```ocaml
| VClosed _ -> pp fmt "[< "
```

The `VClosed of string list` form carries a list of tags that must be present (OCaml
syntax: `` [< `A | `B > `A ] ``). The `> tags` part is never printed. So
`` [< `A | `B > `A ] `` prints as `` [< `A | `B ] ``, which has different semantics
(any subset vs. must include `` `A ``).

**Fix:** Print the `> tag1 tag2...` portion when the string list is non-empty.

### 9. `Fun` printing has mismatched Format boxes (lines 1849-1864)

```ocaml
| Fun { params; constraint_; body } -> (
  (match params with
  | _::_ ->
    pp fmt "@[<2>fun";    (* opens box A *)
    ...
  | [] -> ());            (* box A never opened *)
  match body with
  | Pfunction_body exp ->
    ...
    pp fmt "%a@]" ...     (* closes box A -- or nothing if params=[] *)
  | Pfunction_cases cases ->
    pp fmt "function@[";  (* opens box B *)
    ...
    pp fmt "@]")          (* closes box B -- box A leaked if params<>[] *)
```

Two of four cases have mismatched `@[`/`@]`:
- `params=[]` + `Pfunction_body`: `@]` closes a box that was never opened
- `params<>[]` + `Pfunction_cases`: box A is opened but never closed

This produces garbled formatting output.

### 10. `Src_pos` prints as `.` (line 1996)

```ocaml
| Unreachable | Src_pos -> pp fmt "."
```

`.` is correct for `Unreachable` (refutation case syntax) but wrong for `Src_pos`,
which represents `[%src_pos]`. In expression position, `.` is not valid OCaml and
doesn't convey the meaning.

### 11. `Name.base_name` skips `let`-parenthesization for names with internal underscores (lines 140-148)

```ocaml
match String.rindex_opt s '_' with
| None -> whole_possibly_parens s
| Some i -> begin
    if i = 0 then whole_possibly_parens s
    else if s.[i - 1] = '_'
    then ... (* strip __N suffix *)
    else s   (* <-- no parens check! *)
  end
```

A name like `let_x` (starts with `"let"`, has an underscore at position 3) hits the
`else s` branch and is returned without the `whole_possibly_parens` check. It should be
parenthesized as `(let_x)` since it starts with a keyword prefix. The `None` and `i = 0`
branches correctly apply the check, but the `else` branch on line 148 forgets to.

### 12. `Name.base_name` uses `Int32.of_string_opt` instead of `int_of_string_opt` (line 145)

```ocaml
match Int32.of_string_opt (String.sub s (i + 1) (n - i - 1)) with
```

This is used to detect compiler-generated `__N` numeric suffixes on variable names.
`Int32.of_string_opt` rejects values outside `[-2^31, 2^31-1]`. On 64-bit systems where
`int` is 63 bits, the compiler can generate stamps larger than `Int32.max_int`, causing
`base_name` to fail to strip the suffix, printing the raw internal name
(e.g., `x__3000000000`) instead of `x`.

**Fix:** Use `int_of_string_opt`.

## translquote.ml

### 13. `vars_env.env_poly` never cleared after `transl_quote` (lines 3822-3839)

Free polymorphic type variables are auto-created during `quote_core_type` (lines
2890-2893) and accumulated in `vars_env.env_poly`. At `transl_quote`, they're read and
used to build the `Code.of_exp_with_type_vars`. But they are never removed after the
function returns.

If a compilation unit has two quotations -- `<[ fun (x : 'a) -> x ]>` then `<[ 42 ]>` --
the second quotation would see a stale `'a` in `env_poly` and incorrectly generate
`Code.of_exp_with_type_vars` with a spurious type parameter.

**Fix:** Clear `vars_env.env_poly` at the end (or start) of `transl_quote`.

### 14. `with`/`without` env pairs not exception-safe (multiple locations)

Throughout `translquote.ml`, bindings are added to the global `vars_env` and removed
after use, but without `try`/`finally` protection. If the code between
`with_new_idents_*` and `without_idents_*` raises, bindings leak into subsequent
translations. Examples:

- `case_binding` lines 3066/3085, 3092/3098, 3108/3118
- `quote_expression_desc` for `Texp_let Recursive` lines 3500/3511
- `quote_expression_desc` for `Texp_for` lines 3670/3672
- `quote_expression_desc` for `Texp_letmodule` lines 3690/3692

### 15. `raise Exit` escapes with no diagnostic (multiple locations)

`module_for_path` (line 2331), `type_for_path` (line 2380), `value_for_path` (line
2394), `module_type_for_path` (line 2341) all use `raise Exit` for unsupported `Path`
cases (`Papply`, `Pextra_ty`). Some callers catch it -- `value_for_path_opt` (lines
2396-2397) wraps in `match ... with exception Exit -> None`. But most callers (e.g.,
`module_for_path` is called extensively from `quote_constructor`, `quote_record_field`,
`quote_module_path`, `type_for_path`, etc.) do not catch `Exit`. If a quoted expression
references a module via `Papply` (functor application in a path), the compiler crashes
with an uncaught `Exit` exception and zero diagnostic.

**Fix:** Replace `raise Exit` with `fatal_errorf` providing the location and path that
couldn't be translated.

### 16. Mode annotations silently dropped from quotations (line 3370)

```ocaml
| Texp_mode _ -> lambda (* FIXME: add modes to quotation representation *)
```

When a quoted expression has mode annotations (e.g., `local_`, `unique_`), they are
silently discarded. The resulting quotation AST represents a different program than what
was written. Has an existing FIXME.
