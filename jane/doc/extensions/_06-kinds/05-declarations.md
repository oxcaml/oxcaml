---
layout: documentation-page
collectionName: Kinds
title: Kind declarations
---

# Kind declarations

Just as module signatures and structures may contain (possibly abstract) `type`
declarations, they may contain (possibly abstract) `kind_` declarations as well:

```ocaml
module type Example = sig
  kind_ k1
  kind_ k2 = value mod portable

  type t1 : k1
  type t2 : k2
  val make1 : unit -> t1
end
```

Here, `k1` is an _abstract kind_ and `k2` is a _kind abbreviation_.
Once declared, the kinds `k1` and `k2` may be used wherever kinds may appear,
as described in [the syntax overview](../syntax).
Due to the lack of information known to clients about abstract kinds (as well as
limitations with the kind system), there are some caveats on their usage,
detailed in the [limitations section](#limitations-on-abstract-kinds) below.

As with type declarations in module types, kind declarations in module types may
also be (destructively) substituted using `with`.

To implement a module type with a declared `kind_`, include a `kind_` in the
implementing `struct`, just as is done for types. Kind abbreviations in
implementations must match the abbreviations in the signature (again, just as
for type abbreviations).

```ocaml
module Float_example : Example with kind_ k1 = float64 = struct
  kind_ k1 = float64
  kind_ k2 = value mod portable

  type t1 = float#
  type t2 = int
  let make1 () = #42.0
end
```

We give this implementation the module type `Example with kind_ k1 = float64` to
allow clients to call `make`. Without the substitution, clients would not know
how to represent values of type `t`, preventing them from being used. For
details, see the [limitations section](#limitations-on-abstract-kinds) below.

The following example showcases more complicated ways in which abstract kinds
may be used:

```ocaml
module type Container = sig
  kind_ k
  type ('a : k) t

  val length : ('a : k). 'a t -> int
  val of_array : ('a : k separable). 'a array -> 'a t
end
```

Abstract kinds can be particularly useful as the kind of a type parameter, like
`('a : k) t` above, allowing it to be substituted for various concrete kinds
later. We may add [scannable axes](../../unboxed-types/intro#value-layouts) and
[(non-)modal bounds](../intro) on an abstract kind, like `k separable` above,
to make the kind more restrictive.

```ocaml
module List_container : Container with kind_ k = value_or_null = struct
  kind_ k = value_or_null
  type ('a : k) t = 'a list

  let length = List.length
  let of_array = Array.to_list
end

module Array_container : Container with kind_ k := float64 = struct
  type ('a : float64) t = 'a array

  let length = Array.length
  let of_array a = a
end
```

The second instantiation here uses destructive substitution `:=` and thus does
not need to include a `kind_ k = float64` declaration. This destructive
substitution of kinds works the same way as it does for types.


## Limitations on abstract kinds

As briefly seen above, abstract kinds are _not representable_.
It is not known how to represent in memory the values of a type `t : M.k` with
an abstract kind `k`, preventing such values from being used. This is why
implementations typically expose their kinds using substitution.

Types with abstract kinds are still useful wherever only `any` is required, as
in the following (contrived) example:

```ocaml
module Container_with_elt (C : Container) = struct
  (* Recall [('a : any) option] *)
  type ('a : C.k) t = { c : 'a C.t; elt : 'a option }
end
```

Abstract kinds cannot appear in products. Currently, products are composed of
_layouts_, not kinds. Product types whose components have abstract kinds are
allowed, and non-abstract kind abbreviations can still appear in products.

```ocaml
module type Products = sig
  kind_ k1
  kind_ k2 = value mod portable

  (* Allowed and equivalent to [(value mod portable) & (value mod portable)]
     AKA [(value & value) mod portable]. *)
  kind_ k2_pair = k2 & k2

  (* Error: Abstract kinds are not yet supported in products. *)
  kind_ k1_pair = k1 & k1

  type t : k1
  (* Allowed, but we cannot ascribe the more precise kind [k1 & k1]
     exactly because abstract kinds are not yet supported in products. *)
  type t_pair : any & any = #(t * t)
end
```

[Templates](../../templates/intro) cannot look through abstract kinds, and will
only work if templates and instantiations use the exact same (unqualified) name.
