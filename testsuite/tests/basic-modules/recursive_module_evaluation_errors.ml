(* TEST
   expect;
*)

module rec A : sig
  val x : int
end = struct
  let x = B.x
end

and B : sig
  val x : int
end = struct
  let x = E.y
end

and C : sig
  val x : int
end = struct
  let x = B.x
end

and D : sig
  val x : int
end = struct
  let x = C.x
end

and E : sig
  val x : int

  val y : int
end = struct
  let x = D.x

  let y = 0
end

[%%expect
{|
Lines 9-11, characters 6-3:
 9 | ......struct
10 |   let x = E.y
11 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: B -> E -> D -> C -> B.
       There are no safe modules in this cycle (see manual section 12.2).
Line 8, characters 2-13:
8 |   val x : int
      ^^^^^^^^^^^
  Module "B" defines an unsafe value, "x" .
Line 26, characters 2-13:
26 |   val x : int
       ^^^^^^^^^^^
  Module "E" defines an unsafe value, "x" .
Line 20, characters 2-13:
20 |   val x : int
       ^^^^^^^^^^^
  Module "D" defines an unsafe value, "x" .
Line 14, characters 2-13:
14 |   val x : int
       ^^^^^^^^^^^
  Module "C" defines an unsafe value, "x" .
|}]

type t = ..

module rec A : sig
  type t += A
end = struct
  type t += A = B.A
end

and B : sig
  type t += A
end = struct
  type t += A = A.A
end

[%%expect
{|
type t = ..
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t += A = B.A
7 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 4, characters 12-13:
4 |   type t += A
                ^
  Module "A" defines an unsafe extension constructor, "A" .
Line 10, characters 12-13:
10 |   type t += A
                 ^
  Module "B" defines an unsafe extension constructor, "A" .
|}]

module rec A : sig
  module F : functor (X : sig end) -> sig end

  val f : unit -> unit
end = struct
  module F (X : sig end) = struct end

  let f () = B.value
end

and B : sig
  val value : unit
end = struct
  let value = A.f ()
end

[%%expect
{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   module F (X : sig end) = struct end
7 |
8 |   let f () = B.value
9 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 2-45:
2 |   module F : functor (X : sig end) -> sig end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Module "A" defines an unsafe functor, "F" .
Line 12, characters 2-18:
12 |   val value : unit
       ^^^^^^^^^^^^^^^^
  Module "B" defines an unsafe value, "value" .
|}]

module F (X : sig
  module type t

  module M : t
end) =
struct
  module rec A : sig
    module M : X.t

    val f : unit -> unit
  end = struct
    module M = X.M

    let f () = B.value
  end

  and B : sig
    val value : unit
  end = struct
    let value = A.f ()
  end
end

[%%expect
{|
Lines 11-15, characters 8-5:
11 | ........struct
12 |     module M = X.M
13 |
14 |     let f () = B.value
15 |   end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 8, characters 4-18:
8 |     module M : X.t
        ^^^^^^^^^^^^^^
  Module "A" defines an unsafe module, "M" .
Line 18, characters 4-20:
18 |     val value : unit
         ^^^^^^^^^^^^^^^^
  Module "B" defines an unsafe value, "value" .
|}]

module rec M : sig
  val f : unit -> int
end = struct
  let f () = N.x
end

and N : sig
  val x : int
end = struct
  let x = M.f ()
end

[%%expect {|
Exception: Undefined_recursive_module ("", 3, 6).
|}]
