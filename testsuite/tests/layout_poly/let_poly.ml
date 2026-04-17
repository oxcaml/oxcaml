(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
 expect.opt;
*)

(* CR-soon zqian: Layout poly currently raises in lambda and middle-end.
Therefore, in the following typing tests, we intentionally write the wrong
signature such that the inferred signature can be printed and inspected, and we
never go to lambda. We should add the corresponding positive tests once they can
go through lambda and middle-end. *)

let poly_ id x = x
[%%expect{|
val id : layout_ l. ('a : l). 'a -> 'a = <fun>
|}]

(* Simple let poly_ with a polymorphic function *)
let poly_ id x = x
[%%expect{|
val id : layout_ l. ('a : l). 'a -> 'a = <fun>
|}]

let poly_ id =
  let f x = x in
  f
[%%expect{|
Lines 1-3, characters 0-3:
1 | let poly_ id =
2 |   let f x = x in
3 |   f
Error: The right-hand side of a "let poly_" binding must be a syntactic value.
|}]

let poly_ id = fun x -> x
[%%expect{|
val id : layout_ l. ('a : l). 'a -> 'a = <fun>
|}]


(* Let poly_ with multiple bindings - all must be poly_ *)
let poly_ const x y = x
and poly_ apply f x = f x
[%%expect{|
val const : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> 'a = <fun>
val apply : layout_ l l0. ('a : l) ('b : l0). ('a -> 'b) -> 'a -> 'b = <fun>
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
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

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
Line 1, characters 0-47:
1 | let poly_ pair = let y = 42 in fun x -> #(x, y)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The right-hand side of a "let poly_" binding must be a syntactic value.
|}]

(* RHS might constrain a layout and makes it not polymorphic *)
let poly_ f x y = #(x, (y, y))
[%%expect{|
val f : layout_ l. ('a : l) 'b. 'a -> 'b -> #('a * ('b * 'b)) = <fun>
|}]

(* [any] doesn't really constrain the layout *)
let poly_ f x = (x : (_ : any))
[%%expect{|
val f : layout_ l. ('a : l). 'a -> 'a = <fun>
|}]

(* [value] does constrain the layout *)
let poly_ f x = (x : (_ : value))
[%%expect{|
Line 1, characters 10-11:
1 | let poly_ f x = (x : (_ : value))
              ^
Warning 217: This binding has no layout variables, so "poly_" has no effect. Consider using a regular "let" instead.

val f : 'a -> 'a = <fun>
|}]

(* [assert false] is layout poly *)
let poly_ f () = assert false
[%%expect{|
val f : layout_ l. ('a : l). unit -> 'a = <fun>
|}]

(* We observe that foo is polymorphic on two types sharing the same polymorphic
   layout *)
let poly_ foo x y =
  let id z = z in
  let _ = id x in
  let _ = id y in
  ()
[%%expect{|
val foo : layout_ l. ('a : l) ('b : l). 'a -> 'b -> unit = <fun>
|}]

(* We observe that foo is polymorphic on two types NOT sharing the same polymorphic
   layout. *)
let poly_ foo x y =
  let poly_ id z = z in
  let _ = id x in
  let _ = id y in
  ()
[%%expect{|
val foo : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> unit = <fun>
|}]

(* [rec] prevents layout polymorphism, even for fake recursion (no
   self-reference). *)
let rec poly_ f x = x
[%%expect{|
Line 1, characters 14-15:
1 | let rec poly_ f x = x
                  ^
Warning 218: "poly_" has no effect in recursive bindings, which do not support layout polymorphism. Consider using a regular "let rec" instead.

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

val f : 'a @ local -> unit = <fun>
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"
external to_float : float# -> float = "%box_float" [@@warning "-187"]
let x =
  let[@inline never] poly_ f x = id x in
  let a = f 2 in
  let b = f #3.0 |> to_float in
  (a, b)

[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
external to_float : float# -> float = "%box_float"
val x : int * float = (2, 3.)
|}]
