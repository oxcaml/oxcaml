---
layout: documentation-page
collectionName: Uniqueness
title: Borrowing
---

# Borrowing

Borrowing is a mechanism that lets you temporarily use a unique value as an
`aliased`, `local` reference without permanently giving up its uniqueness. After the
borrow ends, the value can be used uniquely again. This is useful when you want
to call a function that takes an `aliased` argument but still want to uniquely own
the value afterwards.

See also the [introduction to uniqueness](../intro) and the [reference](../reference).

## The `borrow_` operator

The `borrow_` operator creates a borrowed reference to a value:

```ocaml
let foo () =
  let x @ unique = "hello" in
  local_aliased_use (borrow_ x);  (* x is borrowed here *)
  unique_use x                    (* x is used uniquely here *)
```

The borrowed value `borrow_ x` has two key properties:

- It is `aliased`: it cannot be passed where a `unique` value is expected.
- It is `local`: it cannot escape the current borrow region (it cannot become
  global or be stored in a heap-allocated value).

## Borrow regions

A _borrow region_ is the scope during which a value is considered borrowed. The
borrow region begins at the `borrow_` expression and extends to the end of the
enclosing `let` binding, function application, or `match` expression. For
example:

```ocaml
let foo () =
  let x = "hello" in
  (let y = borrow_ x in    (* borrow region starts here *)
  local_aliased_use y);    (* borrow region ends at the closing paren *)
  unique_use x             (* safe: borrow region has ended *)
```

During the borrow region, the original value `x` may not be used uniquely:

```ocaml
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  unique_use x;            (* Error: x is being borrowed *)
  ()
```

```
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used as "unique" here, but it is being borrowed.
Line 3, characters 10-19:
3 |   let y = borrow_ x in
              ^^^^^^^^^
  The value is being borrowed
Lines 3-5, characters 2-4:
3 | ..let y = borrow_ x in
4 |   unique_use x;
5 |   ()
  during this borrow context
```

The borrow region ends when the borrowed binding goes out of scope. Note that
borrowing is still tracked even if the borrowed value is not bound to a name:

```ocaml
let foo () =
  let x = "hello" in
  let _ = borrow_ x in    (* x is still borrowed despite _ binding *)
  unique_use x;           (* Error: x is still being borrowed *)
  ()
```

## Borrowing requires `many`

The original value must be `many` (not `once`) to be borrowed. A `once` value
cannot be borrowed:

```ocaml
let foo (x @ once) =
  let y = borrow_ x in    (* Error: x is "once" but must be "many" *)
  ()
```

## Typical usage pattern

The main use case for borrowing is to call functions that accept `aliased`
arguments while keeping the ability to use the value uniquely afterwards:

```ocaml
let foo () =
  let x @ unique = "hello" in
  local_aliased_use (borrow_ x);  (* temporarily alias x *)
  unique_use x                    (* x is still uniquely owned *)
```

Without `borrow_`, `aliased` use of a unique value permanently forfeits its
uniqueness:

```ocaml
let foo () =
  let x @ unique = "hello" in
  global_aliased_use x;   (* permanently aliased, uniqueness lost *)
  unique_use x;           (* Error: x has already been used aliased *)
  ()
```

## `aliased` use during borrowing

Using the original value `aliased` _within_ the borrow region produces a warning
(Warning 216 [use-during-borrowing]):

```ocaml
let foo () =
  let x = "hello" in
  let _y = borrow_ x in
  global_aliased_use x;   (* Warning 216: used while being borrowed *)
  ()
```

Such `aliased` use during borrowing invalidates the value's uniqueness permanently,
so unique use is not allowed even after the borrow region ends:

```ocaml
let foo () =
  let x = "hello" in
  (let y = borrow_ x in
  global_aliased_use x);  (* Warning 216: used while being borrowed *)
  unique_use x;           (* Error: already used while borrowed *)
  ()
```

To avoid this, pass the original value through `borrow_` whenever you need to
use it `aliased` within the borrow region, rather than using it directly.

## Borrowed values cannot escape the borrow region

Because borrowed values are `local`, they cannot be returned from or stored beyond
their borrow region:

```ocaml
let foo () =
  let _ =
    let x = "hello" in
    let y = borrow_ x in
    y                     (* Error: y is local and cannot escape *)
  in
  ()
```

Borrowed values also cannot be captured by heap-allocated closures:

```ocaml
let foo () =
  let _ =
    let y = borrow_ "hello" in
    (fun () -> y)         (* Error: y cannot be captured in a global closure *)
  in
  ()
```

## Multiple borrows

Multiple borrows of the same value can appear in a single function call. All
such borrows form one borrow region for that application, and unique use
afterwards is still permitted:

```ocaml
let aliased_aliased_use (local_ x) (local_ y) = ()

let foo () =
  let x @ unique = "hello" in
  aliased_aliased_use (borrow_ x) (borrow_ x);  (* two borrows, one region *)
  unique_use x;                                  (* safe *)
  ()
```

If any argument uses `x` without `borrow_` within the same application, that
aliased use triggers Warning 216 and prevents subsequent unique use:

```ocaml
let foo () =
  let x @ unique = "hello" in
  aliased_aliased_use (borrow_ x) x;  (* Warning 216: x used without borrow_ *)
  unique_use x;                       (* Error *)
  ()
```

You can also borrow two different values in the same expression:

```ocaml
let foo () =
  let x = "foo" in
  let y = "bar" in
  let z1 = borrow_ x in
  (let z2 = borrow_ y in aliased_aliased_use z1 z2);
  unique_aliased_use y z1   (* safe: y's borrow ended, x's borrow is still alive *)
```

## Borrowing in `match` expressions

`borrow_` can appear as the scrutinee of a `match`:

```ocaml
let foo () =
  let x = "hello" in
  match borrow_ x with
  | _y -> ignore (local_aliased_use _y)
```

The borrow region spans the entire match expression. Unique use of the original
within any branch is an error:

```ocaml
let foo () =
  let x = "hello" in
  match borrow_ x with
  | _y -> ignore (unique_use x)  (* Error: x is being borrowed *)
```

## Closures closing over borrowed values

A closure that contains a `borrow_` expression is itself considered borrowing.
If such a closure may be called after the original value's owner performs a
unique use, the compiler rejects the program:

```ocaml
let foo () =
  let x = "hello" in
  let bar () =
    let bar' () =
      local_aliased_use (borrow_ x);
      ()
    in
    ()
  in
  unique_use x   (* Error: x has already been borrowed in a closure *)
```

## Valid positions for `borrow_`

The `borrow_` operator must appear directly in one of these positions:

- As an argument in a function application
- On the right-hand side of a `let` binding
- As the scrutinee of a `match`

Using `borrow_` in other positions, such as inside a tuple literal, is not
currently supported:

```ocaml
let foo () =
  let y @ unique = "hello" in
  let y0, y1 = borrow_ y, borrow_ y in  (* Error: invalid borrowing context *)
  ()
```

## Nested borrowing

You can borrow a borrowed value. The result is local to the inner borrow region
and cannot escape it:

```ocaml
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ y in z)  (* Error: z cannot escape the inner borrow region *)
```

Two parallel borrows of the same value in separate regions are fine:

```ocaml
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ x in ());  (* inner borrow region ends here *)
  local_aliased_use y         (* outer borrow region still active *)
```
