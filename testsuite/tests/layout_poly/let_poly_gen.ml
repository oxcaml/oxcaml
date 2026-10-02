(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

external box_float : float# -> float = "%box_float"
module M = struct
  let poly_ id x = x
  let poly_ ignore _ = ()
end
[%%expect{|
external box_float : float# -> float = "%box_float"
module M : sig val poly_ id : 'a -> 'a val poly_ ignore : 'a -> unit end
|}]

(** Nested let-generalisation **)

(* Check all combinations of [poly_] on triple-nested let-bindings,
   with at least one [poly_].
   Tests basic instantiation and generalisation. *)

module Test = struct
  let poly_ id x =
    let poly_ id x =
      let poly_ id x = x in
      id x
    in
    id x
end;;
Test.id 42, box_float (Test.id #3.14)
[%%expect{|
module Test : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ id x =
    let poly_ id x =
      let id x = x in
      id x
    in
    id x
end;;
Test.id 42, box_float (Test.id #3.14)
[%%expect{|
module Test : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ id x =
    let id x =
      let poly_ id x = x in
      id x
    in
    id x
end;;
Test.id 42, box_float (Test.id #3.14)
[%%expect{|
module Test : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ id x =
    let id x =
      let id x = x in
      id x
    in
    id x
end;;
Test.id 42, box_float (Test.id #3.14)
[%%expect{|
module Test : sig val poly_ id : 'a -> 'a end
- : int * float = (42, 3.14)
|}]

(* The outermost binding is not a template,
   but an instantiation of inner templates. *)

module Test = struct
  let id x =
    let poly_ id x =
      let poly_ id x = x in
      id x
    in
    id x
end;;
Test.id 42
[%%expect{|
module Test : sig val id : 'a -> 'a end
- : int = 42
|}]

module Test = struct
  let id x =
    let poly_ id x =
      let id x = x in
      id x
    in
    id x
end;;
Test.id 42
[%%expect{|
module Test : sig val id : 'a -> 'a end
- : int = 42
|}]

module Test = struct
  let id x =
    let id x =
      let poly_ id x = x in
      id x
    in
    id x
end;;
Test.id 42
[%%expect{|
module Test : sig val id : 'a -> 'a end
- : int = 42
|}]

(* The outermost binding is not a function *)

module Test = struct
  let poly_ id = let poly_ id x = x in id
end;;
Test.id 42, box_float (Test.id #3.14)
[%%expect{|
Line 2, characters 17-41:
2 |   let poly_ id = let poly_ id x = x in id
                     ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is not allowed in a "let poly_" definition;
       it must be a function.
|}]

(* instantiates *)
module Test = struct
  let id = let poly_ id x = x in id
end;;
Test.id 42
[%%expect{|
module Test : sig val id : 'a -> 'a end
- : int = 42
|}]

(** Constrained let-generalisation **)

(* Since [id] is not generalised in its implementation,
   calling it constrains the argument's layout to be weakly-polymorphic.
   All combinations of weak-polymorphism constraints for 2 arguments.
   Tests that variables in pools are lowered when we leave a level. *)

module Test = struct
  let id x = x
  let poly_ k x y = M.ignore y; x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val id : 'a -> 'a val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let id x = x
  let poly_ k x y = M.ignore y; id x
end;;
Test.k 42 #17l, Test.k 42 #11s
[%%expect{|
module Test : sig val id : 'a -> 'a val poly_ k : 'a. 'a -> 'b -> 'a end
- : int * int = (42, 42)
|}]

module Test = struct
  let id x = x
  let poly_ k _ x y = M.ignore (id y); id x
end;;
Test.k () 42 "abc"
[%%expect{|
module Test :
  sig val id : 'a -> 'a val poly_ k : 'b 'c. 'a -> 'b -> 'c -> 'b end
- : int = 42
|}]

module Test = struct
  let id x = x
  let poly_ k x y = M.ignore (id y); x
end;;
Test.k 42 "abc", box_float (Test.k #3.14 "xyz")
[%%expect{|
module Test : sig val id : 'a -> 'a val poly_ k : 'b. 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

(* [id] is generalised, so we can freely generalise [k] *)

module Test = struct
  let poly_ k x y = M.ignore y; M.id x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let k x y = M.ignore y; M.id x
end;;
Test.k 42 17
[%%expect{|
module Test : sig val k : 'a -> 'b -> 'a end
- : int = 42
|}]

module Test = struct
  let k x y = M.ignore (M.id y); M.id x
end;;
Test.k 42 17
[%%expect{|
module Test : sig val k : 'a -> 'b -> 'a end
- : int = 42
|}]

(** Irrelevant generalisation **)

(* [app_x] and [app_y] cannot generalize over [x] and [y],
   as they are introduced in [k].
   Tests generalisation. *)

module Test = struct
  let poly_ k x y =
    let poly_ app_x f = f x in
    let poly_ app_y f = f y in
    ignore app_x; ignore app_y;
    x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ k x y =
    let poly_ app_x f = f x in
    let app_y f = f y in
    ignore app_x; ignore app_y;
    x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ k x y =
    let app_x f = f x in
    let poly_ app_y f = f y in
    ignore app_x; ignore app_y;
    x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ k x y =
    let app_x f = f x in
    let app_y f = f y in
    ignore app_x; ignore app_y;
    x
end;;
Test.k 42 #17l, box_float (Test.k #3.14 #11s)
[%%expect{|
module Test : sig val poly_ k : 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

(** Equating polymorphic layouts **)

(* [id] is instantiated to one layout at its non-polymorphic let-binding.
   Tests instantiation. *)

module Test = struct
  let poly_ k x y =
    let id = M.id in
    M.ignore #(id x, id y);
    x
end;;
Test.k 42 "abc", box_float (Test.k #3.14 #4.20)
[%%expect{|
module Test : sig val k : layout_ l. ('a : l) ('b : l). 'a -> 'b -> 'a end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ k' x y =
    let id = M.id in
    M.ignore #(id x, id y);
    y
end;;
Test.k' "abc" 42, box_float (Test.k' #4.20 #3.14)
[%%expect{|
module Test : sig val k' : layout_ l. ('a : l) ('b : l). 'a -> 'b -> 'b end
- : int * float = (42, 3.14)
|}]

(** Layouts with non-trivial structure **)

(* We construct a generic product layout by constraining against [any & any].
   Tests level computation, updates and pools for complex layouts. *)

module Test = struct
  let poly_ id (x : (_ : any & any)) = x
end;;
let #(a, b) = Test.id #(42, #3.14) in
let #(c, d) = Test.id #(#3.14, 42) in
a, box_float b, box_float c, d
[%%expect{|
module Test : sig val id : layout_ l l0. ('a : l & l0). 'a -> 'a end
- : int * float * float * int = (42, 3.14, 3.14, 42)
|}]

module Test = struct
  let id (x : (_ : any & any)) = x
  let poly_ id _ x = id x
end;;
let #(a, b) = Test.id () #(42, 3.14) in
a, b
[%%expect{|
module Test :
  sig val poly_ id : ('b : value_or_null & value_or_null). 'a -> 'b -> 'b end
- : int * float = (42, 3.14)
|}]

module Test = struct
  let poly_ id (x : (_ : any & any)) = x
  let poly_ id _ x = id x
end;;
let #(a, b) = Test.id () #(42, #3.14) in
a, box_float b
[%%expect{|
module Test :
  sig val id : layout_ l l0 l1. ('a : l) ('b : l0 & l1). 'a -> 'b -> 'b end
- : int * float = (42, 3.14)
|}]

(* Other structures *)

module Test = struct
  let poly_ id (x : (_ : any & (any & any))) = x
end;;
let #(a, #(b, c)) = Test.id #(42, #(#3.14, 17)) in
a, box_float b, c
[%%expect{|
module Test :
  sig val id : layout_ l l0 l1. ('a : l & (l0 & l1)). 'a -> 'a end
- : int * float * int = (42, 3.14, 17)
|}]

module Test = struct
  let poly_ id (x : (_ : any addressable)) = x
end;;
Test.id "abc"
[%%expect{|
module Test : sig val id : layout_ l. ('a : l addressable). 'a -> 'a end
- : string = "abc"
|}]

(** Lowering the type lowers the layout **)

(* We use type constraints, which lower to [global_level],
   and should accordingly lower the level of the layout. *)

module Test = struct
  let f () =
    let poly_ k _ x y =
      let id = M.id in
      M.ignore #(id x, id y); (* set sorts of x and y equal *)
      M.ignore (x : 'o);      (* lower [x]'s type below the scope of [poly_],
                                 lowering its and [y]'s sorts too *)
      y
    in
    k
end;;
Test.f () () 42 "abc"
[%%expect{|
module Test : sig val f : unit -> 'a -> 'o -> 'b -> 'b end
- : string = "abc"
|}]
