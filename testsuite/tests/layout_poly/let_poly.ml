(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* CR-soon zqian: Layout poly currently raises in lambda and middle-end.
Therefore, in the following typing tests, we intentionally write the wrong
signature such that the inferred signature can be printed and inspected, and we
never go to lambda. We should add the corresponding positive tests once they can
go through lambda and middle-end. *)

let poly_ id x = x
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]

(* Simple let poly_ with a polymorphic function *)
module _ : sig
  val id : int
end = struct
  let poly_ id x = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ id x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val id : int end
       Values do not match:
         val id : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val id : int
       The type "'a -> 'a" is not compatible with the type "int"
|}]

module _ : sig
  val id : int
end = struct
  let poly_ id =
    let f x = x in
    f
end
[%%expect{|
Lines 5-6, characters 4-5:
5 | ....let f x = x in
6 |     f
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

module _ : sig
  val id : int
end = struct
  let poly_ id = fun x -> x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ id = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val id : int end
       Values do not match:
         val id : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val id : int
       The type "'a -> 'a" is not compatible with the type "int"
|}]


(* Let poly_ with multiple bindings - all must be poly_ *)
module _ : sig
  val const : int
  val apply : int
end = struct
  let poly_ const x y = x
  and poly_ apply f x = f x
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   let poly_ const x y = x
6 |   and poly_ apply f x = f x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val const : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a
           val apply :
             layout_ l l0. ('a : l) ('b : l0). ('a -> 'b) -> 'a -> 'b
         end
       is not included in
         sig val const : int val apply : int end
       Values do not match:
         val const : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a
       is not included in
         val const : int
       The type "'a -> 'b -> 'a" is not compatible with the type "int"
|}]

(* Tuple pattern - both bindings have separate univars *)
module _ : sig
  val f : int
  val g : int
end = struct
  let poly_ (f, g) = ((fun a b -> a), (fun c d -> d))
end
[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   let poly_ (f, g) = ((fun a b -> a), (fun c d -> d))
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a
           val g : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'b
         end
       is not included in
         sig val f : int val g : int end
       Values do not match:
         val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a
       is not included in
         val f : int
       The type "'a -> 'b -> 'a" is not compatible with the type "int"
|}]

(* Regular let cannot be given a layout_ type *)
module _ : sig
  val regular_id : layout_ x. ('a : x). 'a -> 'a
end = struct
  let regular_id x = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let regular_id x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val regular_id : 'a -> 'a end
       is not included in
         sig val regular_id : layout_ l. ('a : l). 'a -> 'a end
       Values do not match:
         val regular_id : 'a -> 'a
       is not included in
         val regular_id : layout_ l. ('a : l). 'a -> 'a
       the second has 1 more layout parameter that is not used,
       which is not supported yet.
|}]

(* a [let poly_] binding of a tuple. The middle-end won't support this in the
   foreseeable future *)
module _ : sig
  val foo : layout_ x. ('a : x). 'a -> 'a
  val bar : layout_ p q. ('a : p) ('b : q). 'a -> 'b -> 'a
end = struct
  let poly_ foo, bar =
    (fun x -> x, fun x _ -> x)
end
[%%expect{|
Line 6, characters 4-30:
6 |     (fun x -> x, fun x _ -> x)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is "'a * 'b"
|}]

(* CR-someday zqian: Mixing poly_ and non-poly_ in let ... and ... is a type
   error for now, but we may want to allow this in the future. *)
module _ = struct
  let poly_ f x = x
  and g x = x
end
[%%expect{|
Line 3, characters 2-13:
3 |   and g x = x
      ^^^^^^^^^^^
Error: All bindings in a "let" must be either all "poly_" or all non-"poly_"
|}]

(* Warning when poly_ binding generalizes no layout variables *)
module M = struct
  let poly_ f = 42
end
[%%expect{|
Line 2, characters 12-13:
2 |   let poly_ f = 42
                ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.
>> Fatal error: Matching: layout-poly patterns not yet supported (0 sort var(s))
Uncaught exception: Misc.Fatal_error

|}]

(* layout-polymorphic id is not included in regular id,
   even though the former can be instantiated to the latter *)
module _ : sig
  val id : 'a -> 'a
end = struct
  let poly_ id x = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ id x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val id : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val id : 'a -> 'a end
       Values do not match:
         val id : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val id : 'a -> 'a
       the first has 1 more layout parameter that is not used,
       which is not supported yet.
|}]

(* The RHS has to be a syntactic value *)
module M : sig
  val pair : int
end = struct
  let poly_ pair = let y = 42 in fun x -> #(x, y)
end
[%%expect{|
Line 4, characters 19-49:
4 |   let poly_ pair = let y = 42 in fun x -> #(x, y)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* constructor: passing when all args are syntactic values *)
let poly_ f = Some (fun x -> x)
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]

(* constructor: failing when an arg is not a syntactic value *)
let poly_ f = Some (let x = ref 0 in x)
[%%expect{|
Line 1, characters 19-39:
1 | let poly_ f = Some (let x = ref 0 in x)
                       ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* variant: passing - no payload *)
let poly_ f = `A
[%%expect{|
Line 1, characters 10-11:
1 | let poly_ f = `A
              ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.
>> Fatal error: Matching: layout-poly patterns not yet supported (0 sort var(s))
Uncaught exception: Misc.Fatal_error

|}]

(* variant: passing - payload is a syntactic value *)
let poly_ f = `A (fun x -> x)
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]

(* variant: failing - payload is not a syntactic value *)
let poly_ f = `A (let x = ref 0 in x)
[%%expect{|
Line 1, characters 17-37:
1 | let poly_ f = `A (let x = ref 0 in x)
                     ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* tuple: passing when all components are syntactic values *)
let poly_ f = (42, fun x -> x)
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]

(* tuple: failing when a component is not a syntactic value *)
let poly_ f = (let x = ref 0 in x, fun x -> x)
[%%expect{|
Line 1, characters 14-46:
1 | let poly_ f = (let x = ref 0 in x, fun x -> x)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* unboxed tuple: passing when all components are syntactic values *)
let poly_ f = #(42, fun x -> x)
[%%expect{|
>> Fatal error: layout: unexpected genvar
Uncaught exception: Misc.Fatal_error

|}]

(* unboxed tuple: failing when a component is not a syntactic value *)
let poly_ f = #((let x = ref 0 in x), fun x -> x)
[%%expect{|
Line 1, characters 16-36:
1 | let poly_ f = #((let x = ref 0 in x), fun x -> x)
                    ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* record: passing when all fields are syntactic values *)
type r = { a : int; b : int -> int }
let poly_ f = { a = 42; b = fun x -> x }
[%%expect{|
type r = { a : int; b : int -> int; }
Line 2, characters 10-11:
2 | let poly_ f = { a = 42; b = fun x -> x }
              ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.
>> Fatal error: Matching: layout-poly patterns not yet supported (0 sort var(s))
Uncaught exception: Misc.Fatal_error

|}]

(* record: failing when a field is not a syntactic value *)
let poly_ f = { a = (let x = ref 0 in !x); b = fun x -> x }
[%%expect{|
Line 1, characters 20-41:
1 | let poly_ f = { a = (let x = ref 0 in !x); b = fun x -> x }
                        ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* unboxed product record: passing when all fields are syntactic values *)
type ur = #{ a : int; b : int }
let poly_ f = #{ a = 42; b = 0 }
[%%expect{|
type ur = #{ a : int; b : int; }
Line 2, characters 10-11:
2 | let poly_ f = #{ a = 42; b = 0 }
              ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.
>> Fatal error: Matching: layout-poly patterns not yet supported (0 sort var(s))
Uncaught exception: Misc.Fatal_error

|}]

(* unboxed product record: failing when a field is not a syntactic value *)
let poly_ f = #{ a = (let x = ref 0 in !x); b = 0 }
[%%expect{|
Line 1, characters 21-42:
1 | let poly_ f = #{ a = (let x = ref 0 in !x); b = 0 }
                         ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition; it must be a function, constructor, tuple, record, or constant.
|}]

(* RHS might constrain a layout and makes it not polymorphic *)
module M : sig
  val f : int
end = struct
  let poly_ f x y = #(x, (y, y))
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ f x y = #(x, (y, y))
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : layout_ l. ('a : l) 'b. 'a -> 'b -> #('a * ('b * 'b))
         end
       is not included in
         sig val f : int end
       Values do not match:
         val f : layout_ l. ('a : l) 'b. 'a -> 'b -> #('a * ('b * 'b))
       is not included in
         val f : int
       The type "'a -> 'b -> #('a * ('b * 'b))" is not compatible with the type
         "int"
|}]

(* [any] doesn't really constrain the layout *)
module M : sig
  val f : int
end = struct
  let poly_ f x = (x : (_ : any))
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ f x = (x : (_ : any))
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         sig val f : int end
       Values do not match:
         val f : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val f : int
       The type "'a -> 'a" is not compatible with the type "int"
|}]

(* [value] does constrain the layout *)
module M : sig
  val f : int
end = struct
  let poly_ f x = (x : (_ : value))
end
[%%expect{|
Line 4, characters 12-13:
4 |   let poly_ f x = (x : (_ : value))
                ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ f x = (x : (_ : value))
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : int end
       Values do not match: val f : 'a -> 'a is not included in val f : int
       The type "'a -> 'a" is not compatible with the type "int"
|}]

(* [assert false] is layout poly *)
module M : sig
  val f : int
end = struct
  let poly_ f () = assert false
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let poly_ f () = assert false
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : layout_ l. ('a : l). unit -> 'a end
       is not included in
         sig val f : int end
       Values do not match:
         val f : layout_ l. ('a : l). unit -> 'a
       is not included in
         val f : int
       The type "unit -> 'a" is not compatible with the type "int"
       Hint: Did you forget to provide "()" as argument?
|}]

(* We observe that foo is polymorphic on two types sharing the same polymorphic
   layout *)
module M : sig
  val foo : int
end = struct
  let poly_ foo x y =
    let id z = z in
    let _ = id x in
    let _ = id y in
    ()
end
[%%expect{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   let poly_ foo x y =
5 |     let id z = z in
6 |     let _ = id x in
7 |     let _ = id y in
8 |     ()
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig val foo : layout_ l. ('a : l) ('b : l). 'a -> 'b -> unit end
       is not included in
         sig val foo : int end
       Values do not match:
         val foo : layout_ l. ('a : l) ('b : l). 'a -> 'b -> unit
       is not included in
         val foo : int
       The type "'a -> 'b -> unit" is not compatible with the type "int"
|}]

(* We observe that foo is polymorphic on two types NOT sharing the same polymorphic
   layout. *)
module M : sig
  val foo : int
end = struct
  let poly_ foo x y =
    let poly_ id z = z in
    let _ = id x in
    let _ = id y in
    ()
end
[%%expect{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   let poly_ foo x y =
5 |     let poly_ id z = z in
6 |     let _ = id x in
7 |     let _ = id y in
8 |     ()
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig val foo : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit end
       is not included in
         sig val foo : int end
       Values do not match:
         val foo : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit
       is not included in
         val foo : int
       The type "'a -> 'b -> unit" is not compatible with the type "int"
|}]

(* [rec] prevents layout polymorphism, even for fake recursion (no
   self-reference). *)
module M : sig
  val f : 'a -> 'a
end = struct
  let rec poly_ f x = x
end
[%%expect{|
Line 4, characters 16-17:
4 |   let rec poly_ f x = x
                    ^
Warning 218: "poly_" has no effect in recursive bindings, which do not support layout polymorphism. Consider using a regular "let rec" instead.
>> Fatal error: Translcore.transl_let
Uncaught exception: Misc.Fatal_error

|}]

(* CR-someday zqian: [rec poly_] should work with explicit user annotations. *)
module M : sig
  val f : int
end = struct
  let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> x
end
[%%expect{|
Line 4, characters 20-49:
4 |   let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> x
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* CR-soon zqian: should be layout poly, once we support instantiation. *)
module M : sig
  val f : int
end = struct
  let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> f x
end
[%%expect{|
Line 4, characters 20-49:
4 |   let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> f x
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* CR-soon zqian: should be layout poly, once we support instantiation. *)
module M : sig
  val f : int
  val g : int
end = struct
  let rec poly_ g : layout_ l. ('a : l). 'a -> 'a = fun x -> h x
  and poly_ h : layout_ l. ('a : l). 'a -> 'a = fun x -> g x
end
[%%expect{|
Line 5, characters 20-49:
5 |   let rec poly_ g : layout_ l. ('a : l). 'a -> 'a = fun x -> h x
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* either all poly, or none poly. *)
module M : sig
  val f : int
  val g : int
end = struct
  let rec poly_ f x = g x
  and g x = f x
end
[%%expect{|
Line 6, characters 2-15:
6 |   and g x = f x
      ^^^^^^^^^^^^^
Error: All bindings in a "let" must be either all "poly_" or all non-"poly_"
|}]

(* The following fails, because [f] contains a captured environment containing x which is
   regional, and that makes the captured environment to be local, which makes [f] unable
   to escape the regiohn. *)
let _bar (x @ local) =
  let poly_ f = x in
  f
[%%expect{|
Line 2, characters 12-13:
2 |   let poly_ f = x in
                ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

Line 3, characters 2-3:
3 |   f
      ^
Error: This value is "local"
         because it is defined by a layout-polymorphic expression (at line 2, characters 12-13)
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* multiple poly can be have different captured environment mode *)
let f (x @ local) =
  let poly_ f = x
  and poly_ g = () in
  g
[%%expect{|
Line 3, characters 12-13:
3 |   and poly_ g = () in
                ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

Line 2, characters 12-13:
2 |   let poly_ f = x
                ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

Line 2, characters 12-13:
2 |   let poly_ f = x
                ^
Warning 26 [unused-var]: unused variable f.
>> Fatal error: Matching: layout-poly patterns not yet supported (0 sort var(s))
Uncaught exception: Misc.Fatal_error

|}]
