(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

external to_int64 : int64# -> int64 = "%box_int64"
external to_float : float# -> float = "%box_float"
external to_int8 : int8# -> int8 = "%tag_int8"
external to_nativeint : nativeint# -> nativeint = "%box_nativeint"
[%%expect{|
external to_int64 : int64# -> int64 = "%box_int64"
external to_float : float# -> float = "%box_float"
external to_int8 : int8# -> int8 = "%tag_int8"
external to_nativeint : nativeint# -> nativeint = "%box_nativeint"
|}]

(* Simple let poly_ with a polymorphic function *)
let poly_ id x = x
[%%expect{|
val id : layout_ l. ('a : l). 'a -> 'a = <lpoly>
|}]

let (a, b, c, d) =
  let poly_ tuple x y = #(x, y) in
  let #(a, b) = tuple "a" #1L in
  let #(c, d) = tuple #42.0 "d" in
  (a, to_int64 b, to_float c, d)
[%%expect{|
val a : string = "a"
val b : int64 = 1L
val c : float = 42.
val d : string = "d"
|}]

let poly_ id =
  let f x = x in
  f
[%%expect{|
Lines 2-3, characters 2-3:
2 | ..let f x = x in
3 |   f
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* Let poly_ with multiple bindings - all must be poly_ *)
let poly_ const x y = x
and poly_ apply f x = f x
[%%expect{|
val const : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a = <lpoly>
val apply : layout_ l l0. ('a : l) ('b : l0). ('a -> 'b) -> 'a -> 'b =
  <lpoly>
|}]

(* CR-soon zqian: Tuple patterns are not yet supported by transl.
Therefore, in the following typing test, we intentionally write the wrong
signature such that the inferred signature can be printed and inspected, and we
never go to lambda. We should add the corresponding positive test once it can
go through transl. *)

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
let poly_ foo, bar = (fun x -> x, fun x _ -> x)
[%%expect{|
Line 1, characters 21-47:
1 | let poly_ foo, bar = (fun x -> x, fun x _ -> x)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is "'a * 'b"
|}]

(* CR-someday zqian: Mixing poly_ and non-poly_ in let ... and ... is a type
   error for now, but we may want to allow this in the future. *)
let poly_ f x = x
and g x = x
[%%expect{|
Line 2, characters 0-11:
2 | and g x = x
    ^^^^^^^^^^^
Error: All bindings in a "let" must be either all "poly_" or all non-"poly_"
|}]

(* Warning when poly_ binding generalizes no layout variables *)
let poly_ f = 42
[%%expect{|
Line 1, characters 10-11:
1 | let poly_ f = 42
              ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

val f : int = 42
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
let poly_ pair = let y = 42 in fun x -> #(x, y)
[%%expect{|
Line 1, characters 17-47:
1 | let poly_ pair = let y = 42 in fun x -> #(x, y)
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* constructor: passing when all args are syntactic values *)
let poly_ f = Some (fun x -> x)
[%%expect{|
val f : layout_ l. ('a : l). ('a -> 'a) option = <lpoly>
|}]

(* constructor: failing when an arg is not a syntactic value *)
let poly_ f = Some (let x = ref 0 in x)
[%%expect{|
Line 1, characters 19-39:
1 | let poly_ f = Some (let x = ref 0 in x)
                       ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* variant: passing - no payload *)
let poly_ f = `A
[%%expect{|
Line 1, characters 10-11:
1 | let poly_ f = `A
              ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

val f : [> `A ] = `A
|}]

(* variant: passing - payload is a syntactic value *)
let poly_ f = `A (fun x -> x)
[%%expect{|
val f : layout_ l. ('a : l). [> `A of 'a -> 'a ] = <lpoly>
|}]

(* variant: failing - payload is not a syntactic value *)
let poly_ f = `A (let x = ref 0 in x)
[%%expect{|
Line 1, characters 17-37:
1 | let poly_ f = `A (let x = ref 0 in x)
                     ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* tuple: passing when all components are syntactic values *)
let (x, f, a, y, g, b) =
  let poly_ p = (42, fun x -> x) in
  let (x, f) = p in
  let (y, g) = p in
  let #(a, b) = #(f #1.0, g #3L) in
  (x, f, to_float a, y, g, to_int64 b)
[%%expect{|
val x : int = 42
val f : '_weak1 -> '_weak1 = <fun>
val a : float = 1.
val y : int = 42
val g : '_weak2 -> '_weak2 = <fun>
val b : int64 = 3L
|}]

(* tuple: failing when a component is not a syntactic value *)
let poly_ f = (let x = ref 0 in x, fun x -> x)
[%%expect{|
Line 1, characters 14-46:
1 | let poly_ f = (let x = ref 0 in x, fun x -> x)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* unboxed tuple: passing when all components are syntactic values *)
let poly_ f = #(42, fun x -> x)
[%%expect{|
val f : layout_ l. ('a : l). #(int * ('a -> 'a)) = <lpoly>
|}]

(* unboxed tuple: failing when a component is not a syntactic value *)
let poly_ f = #((let x = ref 0 in x), fun x -> x)
[%%expect{|
Line 1, characters 16-36:
1 | let poly_ f = #((let x = ref 0 in x), fun x -> x)
                    ^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* record: passing when all fields are syntactic values *)
type r = { a : int; b : int -> int }
let poly_ f = { a = 42; b = fun x -> x }
[%%expect{|
type r = { a : int; b : int -> int; }
Line 2, characters 10-11:
2 | let poly_ f = { a = 42; b = fun x -> x }
              ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

val f : r = {a = 42; b = <fun>}
|}]

(* record: failing when a field is not a syntactic value *)
let poly_ f = { a = (let x = ref 0 in !x); b = fun x -> x }
[%%expect{|
Line 1, characters 20-41:
1 | let poly_ f = { a = (let x = ref 0 in !x); b = fun x -> x }
                        ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* unboxed product record: passing when all fields are syntactic values *)
type ur = #{ a : int; b : int }
let poly_ f = #{ a = 42; b = 0 }
[%%expect{|
type ur = #{ a : int; b : int; }
Line 2, characters 10-11:
2 | let poly_ f = #{ a = 42; b = 0 }
              ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

val f : ur = <abstr>
|}]

(* unboxed product record: failing when a field is not a syntactic value *)
let poly_ f = #{ a = (let x = ref 0 in !x); b = 0 }
[%%expect{|
Line 1, characters 21-42:
1 | let poly_ f = #{ a = (let x = ref 0 in !x); b = 0 }
                         ^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function, constructor, tuple, record, or constant.
|}]

(* RHS might constrain a layout and makes it not polymorphic *)
let poly_ f x y = #(x, (y, y))
[%%expect{|
val f : layout_ l. ('a : l) 'b. 'a -> 'b -> #('a * ('b * 'b)) = <lpoly>
|}]

(* [any] doesn't really constrain the layout *)
let poly_ f x = (x : (_ : any))
[%%expect{|
val f : layout_ l. ('a : l). 'a -> 'a = <lpoly>
|}]

(* [value] does constrain the layout *)
let poly_ f x = (x : (_ : value))
[%%expect{|
Line 1, characters 10-11:
1 | let poly_ f x = (x : (_ : value))
              ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

val f : 'a -> 'a = <fun>
|}]

(* [assert false] is layout poly *)
let poly_ f () = assert false
[%%expect{|
val f : layout_ l. ('a : l). unit -> 'a = <lpoly>
|}]

(* We observe that foo is polymorphic on two types sharing the same polymorphic
   layout *)
let poly_ foo x y =
  let id z = z in
  let _ = id x in
  let _ = id y in
  ()
[%%expect{|
val foo : layout_ l. ('a : l) ('b : l). 'a -> 'b -> unit = <lpoly>
|}]

(* We observe that foo is polymorphic on two types NOT sharing the same polymorphic
   layout. *)
let poly_ foo x y =
  let poly_ id z = z in
  let _ = id x in
  let _ = id y in
  ()
[%%expect{|
val foo : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit = <lpoly>
|}]

(* [rec] prevents layout polymorphism, even for fake recursion (no
   self-reference). *)
let rec poly_ f x = x
[%%expect{|
Line 1, characters 14-15:
1 | let rec poly_ f x = x
                  ^
Warning 218: poly_ has no effect in recursive bindings, which do not support layout polymorphism. Consider using a regular let rec instead.

val f : 'a -> 'a = <fun>
|}]

(* CR-someday zqian: [rec poly_] should work with explicit user annotations. *)
let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> x
[%%expect{|
Line 1, characters 18-47:
1 | let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> x
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* CR-soon zqian: should be layout poly, once we support instantiation. *)
let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> f x
[%%expect{|
Line 1, characters 18-47:
1 | let rec poly_ f : layout_ l. ('a : l). 'a -> 'a = fun x -> f x
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* CR-soon zqian: should be layout poly, once we support instantiation. *)
let rec poly_ g : layout_ l. ('a : l). 'a -> 'a = fun x -> h x
and poly_ h : layout_ l. ('a : l). 'a -> 'a = fun x -> g x
[%%expect{|
Line 1, characters 18-47:
1 | let rec poly_ g : layout_ l. ('a : l). 'a -> 'a = fun x -> h x
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported in term-level type annotations
|}]

(* either all poly, or none poly. *)
let rec poly_ f x = g x
and g x = f x
[%%expect{|
Line 2, characters 0-13:
2 | and g x = f x
    ^^^^^^^^^^^^^
Error: All bindings in a "let" must be either all "poly_" or all non-"poly_"
|}]

(* The following fails, because [f] contains a captured environment containing x which is
   regional, and that makes the captured environment to be local, which makes [f] unable
   to escape the region. *)
let _bar (x @ local) =
  let poly_ f = x in
  f
[%%expect{|
Line 2, characters 12-13:
2 |   let poly_ f = x in
                ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

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
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

Line 2, characters 12-13:
2 |   let poly_ f = x
                ^
Warning 217: This binding has no layout variables, so poly_ has no effect. Consider using a regular let instead.

Line 2, characters 12-13:
2 |   let poly_ f = x
                ^
Warning 26 [unused-var]: unused variable "f".

val f : 'a @ local -> unit = <fun>
|}]

(* let poly_ instantiation *)
let (a, b) =
  let poly_ id x = x in
  (id 42, id #43.0 |> to_float)
[%%expect{|
val a : int = 42
val b : float = 43.
|}]

(* let poly_ instantiation with multiple variables *)
let (a, b, c, d) =
  let poly_ tuple x y = #(x, y) in
  let #(a, b) = tuple #42s #43.0 in
  let #(c, d) = tuple #44L #45n in
  (to_int8 a, to_float b, to_int64 c, to_nativeint d)
[%%expect{|
val a : int8 = 42s
val b : float = 43.
val c : int64 = 44L
val d : nativeint = 45n
|}]

(* closure conversion - uniform block *)
let (a, b) =
  let x = true in
  let y = ref "first" in
  let poly_ f z = if x then #(!y, z) else #("false", z) in
  let #(a1, a2) = f 1 in
  y := "second";
  let #(b1, b2) = f #2L in
  (a1, b1)
[%%expect{|
val a : string = "first"
val b : string = "second"
|}]

(* closure conversion - mixed block *)
let a, b, c, d =
  let x = true in
  let y = #1s in
  let poly_ f z = if x then #(y, z) else #(#2s, z) in
  let #(a, b) = f 1 in
  let #(c, d) = f #2L in
  to_int8 a, b, to_int8 c, to_int64 d

[%%expect{|
val a : int8 = 1s
val b : int = 1
val c : int8 = 1s
val d : int64 = 2L
|}]

(* closure-conversion - capture lpoly function *)
let a, b, y =
  let a = #2n in
  let poly_ f x = #(a, x) in
  let poly_ g x =
    let #(b, y) = f x in
    #(a, b, y)
  in
  let #(a, b, y) = g #1.0 in
  to_nativeint a, to_nativeint b, to_float y
[%%expect {|
val a : nativeint = 2n
val b : nativeint = 2n
val y : float = 1.
|}]

(* Nested lpoly functions *)
let x, y =
  let poly_ f x =
    let poly_ g y = #(x, y) in
    g x
  in
  let #(x, y) = f #1.0 in
  to_float x, to_float y
[%%expect {|
val x : float = 1.
val y : float = 1.
|}]

(* Module containing lpoly function *)
let x =
  let module M = struct
    let poly_ id x = x
  end in
  M.id #1s |> to_int8
[%%expect {|
val x : int8 = 1s
|}]
