---
layout: documentation-page
collectionName: Uniqueness
title: Borrowing
---

# Borrowing

A common situation when working with `unique` values is wanting to pass one
to a function that takes an `aliased` argument, while still retaining the
ability to use the value uniquely afterwards. This is not directly possible:
once a `unique` value has been used as `aliased`, its uniqueness is
permanently forfeited:

```ocaml
let foo () =
  let x @ unique = "hello" in
  global_aliased_use x;   (* x used as aliased — uniqueness lost *)
  unique_use x;           (* Error: x has already been used aliased *)
  ()
```

Borrowing solves this. The `borrow_` operator lets you temporarily use a
`unique` value as `aliased` and `local`, without permanently giving up its
uniqueness. After the borrow ends, the value can be used uniquely again:

```ocaml
let foo () =
  let x @ unique = "hello" in
  local_aliased_use (borrow_ x);  (* temporarily alias x *)
  unique_use x                    (* x is still uniquely owned *)
```

The borrow itself comes with two restrictions:

- It is `aliased`: it cannot be passed where a `unique` value is expected.
- It is `local`: it cannot escape the current borrow region (it cannot become
  global or be stored in a heap-allocated value).

Also, the original value must be `many` (not `once`) to be borrowed:

```ocaml
let foo (x @ once) =
  let y = borrow_ x in    (* Error: x is "once" but must be "many" *)
  ()
```

## Valid positions and borrow regions

The `borrow_` operator must appear directly in one of three positions. Each
position determines an implicit _borrow region_ — the scope during which the
value is considered borrowed:

- **On the right-hand side of a `let` binding.** The borrow region spans the
  body of the `let`:

  ```ocaml
  let foo () =
    let x = "hello" in
    (let y = borrow_ x in    (* borrow region starts here *)
    local_aliased_use y);    (* borrow region ends at the closing paren *)
    unique_use x             (* safe: borrow region has ended *)
  ```

- **As an argument to a function application.** The borrow region spans the
  application:

  ```ocaml
  let foo () =
    let x @ unique = "hello" in
    local_aliased_use (borrow_ x);  (* borrow region = this application *)
    unique_use x                    (* safe *)
  ```

- **As the scrutinee of a `match`.** The borrow region spans the entire match:

  ```ocaml
  let foo () =
    let x = "hello" in
    match borrow_ x with             (* borrow region = the whole match *)
    | _y -> local_aliased_use _y
  ```

`borrow_` in any other position is not currently supported:

```ocaml
let foo () =
  let y @ unique = "hello" in
  let y0, y1 = borrow_ y, borrow_ y in  (* Error: invalid borrowing context *)
  ()
```

## Using the original value during borrowing

Within the borrow region, the original value cannot be used as `unique`:

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

`aliased` use of the original is allowed but produces a warning
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

## Multiple borrows

Multiple borrows of the same value can appear in a single function call. All
such borrows form one borrow region for that application, and unique use
afterwards is still permitted:

```ocaml
let f (_ @ local) (_ @ local) = ()

let foo () =
  let x @ unique = "hello" in
  f (borrow_ x) (borrow_ x);  (* two borrows, one region *)
  unique_use x                (* safe *)
```

## Closures closing over borrowed values

Consider the following, where `_bar` is defined inside an inner scope and
goes out of scope before `unique_use x`:

```ocaml
let foo () =
  let x = "hello" in
  let _z =
    (let _bar () =
      local_aliased_use (borrow_ x);
      ()
     in
    42)
  in
  unique_use x   (* Error: x has already been borrowed in a closure *)
```

You might expect that constructing the closure starts a borrow region of `x`
that lasts for the lifetime of the closure, with each `borrow_ x` in the body
simply borrowing that already-borrowed value. Under that model, `x` would be
usable uniquely once `_bar` goes out of scope, so this example would
typecheck.

The actual behavior is more conservative: constructing a closure that
captures `x` is treated as an aliased use of `x`, not a borrow. Any
`borrow_ x` in the body is then a borrow of that aliased value. So defining
`_bar` immediately counts as an aliased use of `x`, and the later
`unique_use x` is rejected — just as it would be if you had used `x` aliased
directly. Unlike a borrow, this aliased use is permanent: `x` cannot be used
uniquely even after `_bar` goes out of scope.

We plan to switch to the expected behavior in the near future.

## Nested borrowing

`borrow_` can be applied to an `aliased` value, not just a `unique` one.
Since a borrow is itself `aliased`, this means you can also borrow a borrowed
value. The result is local to the inner borrow region and cannot escape it:

```ocaml
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ y in z)  (* Error: z cannot escape the inner borrow region *)
```

When the inner region ends, the value it borrowed becomes usable uniquely,
even while the outer borrow is still alive:

```ocaml
let foo () =
  let x = "foo" in
  let y = "bar" in
  let z1 = borrow_ x in
  (let z2 = borrow_ y in
   local_aliased_use z1;     (* z1 usable in inner region *)
   local_aliased_use z2);
  unique_use y;              (* safe: y's borrow ended *)
  local_aliased_use z1       (* safe: x's borrow region still alive *)
```

## Interactions with stack allocation

A borrow region only allows `global` values to escape; anything `local` to
it is trapped inside. A value can also be `local` because it was
stack-allocated, and the compiler does not distinguish these two reasons
for being `local`. As a result, a stack-allocated value cannot escape a
borrow region, even when no borrowed value is involved:

```ocaml
let bar (x @ local) =
  let _ = x in
  exclave_ (42, 24)

let foo x =
  let _ = bar (borrow_ x) in   (* Error: bar's result escapes the borrow region *)
  ()
```

Here `bar`'s result is `local` because it is stack-allocated via `exclave_`,
not because it is borrowed. But the compiler treats both kinds of `local`
uniformly, so the result is forbidden from escaping the borrow region.

We plan to split the locality axis into two — one for stack allocation, one
for borrowing — at which point the example above will typecheck.
