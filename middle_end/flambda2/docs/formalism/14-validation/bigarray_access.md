# bigarray_access: bigarray indexing, element access, dims, to_cmm layout

Targets the bigarray rules added when the scope-ledger row was promoted to
"formalized": `P.Bigarray.Indexing`, `P.Binary.BigarrayLoad`,
`P.Ternary.BigarraySet`, `P.Unary.BigarrayLength` (ch.
[06](../06-primitives-memory.md)); `R.Obj.Bigarray` (ch.
[17](../17-representation.md)); `TC.Prim.BigarrayAccess`,
`TC.Prim.BigarrayLength` (ch. [18](../18-to-cmm-data.md)).

**Scratch compile** (no testsuite file): `ocamlopt.opt -c -O3 -dfexpr -dcmm
-dcanonical-ids bigarray_access.ml` with the in-tree compiler.

## Source

```ocaml
open Bigarray

let[@inline never] get (a : (float, float64_elt, c_layout) Array2.t) i j =
  a.{i, j}

let[@inline never] get_unsafe (a : (float, float64_elt, c_layout) Array2.t) i j =
  Array2.unsafe_get a i j

let[@inline never] set_unsafe (a : (float, float64_elt, c_layout) Array2.t) i j v =
  Array2.unsafe_set a i j v

let[@inline never] dim1 (a : (float, float64_elt, c_layout) Array2.t) =
  Array2.dim1 a

let[@inline never] get_fortran (a : (int32, int32_elt, fortran_layout) Array1.t) i =
  Array1.unsafe_get a i
```

## Prediction (before reading the dumps)

1. `get` (safe 2-d C float64): per-dimension bounds checks (unsigned-`Lt`
   conditions, cold raise branch) with bounds from `Bigarray_length {1}` and
   `Bigarray_length {2}`; flat offset `i·d₂ + j` computed in *tagged*
   arithmetic with `Bigarray_length {2}` read a *second* time for the multiply
   (the duplicated-read CR cited in `P.Bigarray.Indexing`); then
   `Bigarray_load(2, Float64, C)` yielding naked_float, boxed with
   `Box_number Naked_float`.
2. `get_unsafe`: same minus the checks; `Bigarray_length {2}` read once.
3. `set_unsafe`: `Unbox_number Naked_float` of `v`, same flat offset,
   `Bigarray_set(2, Float64, C)`, unit result.
4. `dim1`: `Bigarray_length {1}` then `Tag_immediate`. Cmm: `Word_int` load at
   `ba + 40` (field 4+1 = 5, stride 8, `R.Obj.Bigarray`), then tag.
5. `get_fortran` (unsafe 1-d Fortran int32): index decremented by 1 (tagged
   `Sub`), no multiply (single dimension), `Bigarray_load(1, Int32, Fortran)`
   → naked_int32, boxed as int32 custom block.
6. Cmm element access (`TC.Prim.BigarrayAccess`): data pointer =
   `load Word_int` at `ba + 8` (field 1); element address = data +
   tagged-offset scaled with the untag folded into the scale
   (float64: `4·off − 4`; int32: `2·off − 2`); chunks `float64` resp.
   `signed int32`; all loads `Mutable`.

## Actual (excerpts)

`-dfexpr` (after simplify), `get`:

```
(let prim = %bigarray_length.[`1`] (a) in
 let prim_1 = %num_conv.[`imm`].[`nativeint`] (prim) in
 let prim_2 = %num_conv.[tagged_imm].[`nativeint`] (i) in
 let prim_3 = %int_comp.`nativeint`.unsigned.lt (prim_2, prim_1) in
 switch prim_3 | 0 -> k2 | 1 -> k3)
  where k3 =
    ((let prim = %bigarray_length.[`2`] (a) in
      … same shape for j …)
       where k3 =
         let prim = %bigarray_length.[`2`] (a) in
         let prim_1 = %tag_imm (prim) in
         let prim_2 = %int_barith.mul (i, prim_1) in
         let prim_3 = %int_barith.add (prim_2, j) in
         let prim_4 = %bigarray_load.[`2`].float64 (a, prim_3) in
         let Pbigarrayref = %box_num.`float` (prim_4) in
         cont k (Pbigarrayref))
  where k2 = cont k1 pop(regular k1) ($camlBigarray_access__block21)
```

The others, condensed:

```
set_unsafe:  %unbox_num.`float` (v) … %bigarray_set.[`2`].float64 (a, prim_4, prim) … cont k (0)
dim1:        %bigarray_length.[`1`] (a) … %tag_imm … cont k (Pbigarraydim)
get_fortran: %int_barith.sub (i, 1) … %bigarray_load.[`1`].`int32`.fortran (a, prim)
             … %box_num.`int32` … cont k (Pbigarrayref)
```

`-dcmm`, `get_unsafe`:

```
(alloc 1277
  (load_mut float64
    (+a
      (+a (load_mut int (+a a/1 8))
        (<< (+ (* (+ i/1 -1) (>>s (<< (load_mut int (+a a/1 48)) 1) 1)) j/1)
          2))
      -4)))
```

`dim1`: `(+ (<< (load_mut int (+a a/3 40)) 1) 1)`. `set_unsafe`: `store
float64` at the same address form, result `1`. `get_fortran`:

```
(alloc 2303 G:"caml_int32_ops"
  (load_mut signed int32 (+a (+a (load_mut int (+a a/4 8)) (<< i/3 1)) -6)))
```

## Verdict

MATCH (with one detail refined).

- **Checks** (`P.Bigarray.Indexing`): one per dimension, unsigned `Lt`, cold
  raise; dim-1 check first, then dim-2, then the access. *Refinement*: both
  index and bound are `num_conv`ed to **nativeint** before the compare
  (check_bound at `bound_kind:Naked_immediate` widens through nativeint),
  which the prediction did not pin down.
- **Duplicated length read**: the dim-2 `%bigarray_length` appears twice in `get`
  (check + offset multiply), once in `get_unsafe` — exactly the CR'd behaviour.
- **Offset arithmetic**: tagged `Mul`/`Add` after `%tag_imm` of the bound. The
  Cmm residue `(>>s (<< dim 1) 1)` is the tag-then-untag of the bound that the
  CR in `bigarray_indexing` notes is not cancelled.
- **Element access** (`TC.Prim.BigarrayAccess`, `R.Obj.Bigarray`): data
  pointer from field 1 (`+a a 8`); float64 address `data + 4·off_tagged − 4`
  (printed `(<< … 2)` then `-4`), int32 `data + 2·off_tagged − 2` (with the
  tagged `−1` folded: `2·i − 6`); chunks `float64` / `signed int32`; all loads
  `load_mut`.
- **Dims** (`TC.Prim.BigarrayLength`): fields 5 and 6 → byte offsets 40 and 48
  = 8·(4+d) for d = 1, 2. `dim1` tags the result.
- **Fortran** decrement and `.fortran` layout on the fexpr primitive; boxing
  `alloc 1277` (double) / `alloc 2303 caml_int32_ops` (boxed int32,
  `R.Obj.Boxed`).

## Diagnosis

No discrepancy. The flat-offset convention (tagged element-unit offset), the
descriptor layout (field 1 data, field 4+d dims), the per-kind chunks, and the
check placement all match the promoted rules. `P.Binary.BigarrayGetAlignment`
and the complex-kind two-part access were not exercised here and keep their
statuses on code inspection alone.
