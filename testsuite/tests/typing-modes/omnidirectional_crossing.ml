(* TEST
 flags = "-extension omnidirectionality";
 expect;
*)

(* Mode crossing waits for the type variables that a type's kind depends on. *)

(* unify3 is used due to application introducing a local region while
   typechecking the spine. *)
let unify3 (_ : 'a) (_ : 'a) (_ : 'a) = ()
let unify (_ : 'a) (_ : 'a) = ()
[%%expect{|
val unify3 : 'a -> 'a -> 'a -> unit = <fun>
val unify : 'a -> 'a -> unit = <fun>
|}]

let f (local_ x) = (x, ignore (x + 1))
[%%expect {|
val f : int @ local -> int * unit = <fun>
|}]

let f (local_ x) = (x, ignore (x ^ ""))
[%%expect{|
Line 1, characters 20-21:
1 | let f (local_ x) = (x, ignore (x ^ ""))
                        ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is an element of the tuple at line 1, characters 19-39
         which is expected to be "global" because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit = <fun>
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), unify3 x (Some None) (Some (Some 1)))
[%%expect{|
val f : int option option -> int option option * unit = <fun>
|}]

let f (x @ nonportable) y = ((x : _ @ portable), unify3 x y 1)
[%%expect{|
val f : int -> int -> int * unit = <fun>
|}]

(* CR-soon omni: this test should change once defaulting is lazier *)
let f (x @ nonportable) =
  ((x : _ @ portable), unify x (Some (fun () -> ())))
[%%expect{|
Line 2, characters 4-5:
2 |   ((x : _ @ portable), unify x (Some (fun () -> ())))
        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]


let f (x @ nonportable) =
  ((x : _ @ portable), unify x (Some None), unify x (Some (Some (fun () -> ()))))
[%%expect{|
Line 2, characters 4-5:
2 |   ((x : _ @ portable), unify x (Some None), unify x (Some (Some (fun () -> ()))))
        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), unify x (Some None), unify x (Some (Some 1)))
[%%expect{|
Line 2, characters 4-5:
2 |   ((x : _ @ portable), unify x (Some None), unify x (Some (Some 1)))
        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Reported examples *)

let hof : ('a @ local -> unit) = fun _ -> ()
let g : int -> unit = fun _ -> ()
let _ = hof g
[%%expect {|
val hof : 'a @ local -> unit = <fun>
val g : int -> unit = <fun>
- : unit = ()
|}]

type 'a t = Variant of 'a
let foo (type a : value mod portable contended) (_ : a) = ()

let variant = Variant ()

let works1 = foo variant
let works2 = foo (Variant () : _)
(* CR-soon omni: this fails due to eager defaulting *)
let doesn't_work = foo (Variant ())
[%%expect {|
type 'a t = Variant of 'a
val foo : ('a : value mod portable contended). 'a -> unit = <fun>
val variant : unit t = Variant ()
val works1 : unit = ()
val works2 : unit = ()
Line 9, characters 23-35:
9 | let doesn't_work = foo (Variant ())
                           ^^^^^^^^^^^^
Error: This constructor has type "'a t" but an expression was expected of type
         "('b : value mod portable contended)"
       The kind of 'a t is immutable_data with 'a
         because of the definition of t at line 1, characters 0-25.
       But the kind of 'a t must be a subkind of value mod portable contended
         because of the definition of foo at line 2, characters 8-60.
|}, Principal{|
type 'a t = Variant of 'a
val foo : ('a : value mod portable contended). 'a -> unit = <fun>
val variant : unit t = Variant ()
Line 6, characters 17-24:
6 | let works1 = foo variant
                     ^^^^^^^
Error: The value "variant" has type "unit t"
       but an expression was expected of type
         "('a : value mod portable contended)"
       The kind of unit t is immutable_data with unit
         because of the definition of t at line 1, characters 0-25.
       But the kind of unit t must be a subkind of
           value mod portable contended
         because of the definition of foo at line 2, characters 8-60.
|}]

let l x y = exclave_ (x, y)
let g x y = x, y

let (_ : _) =
  let x, y = l 1 2 in
  g x y
[%%expect {|
val l : 'a -> 'b -> 'a * 'b @ local = <fun>
val g : 'a -> 'b -> 'a * 'b = <fun>
- : int * int = (1, 2)
|}]

let f (local_ r) =
  let rec loop i = if i >= r then i else loop (i + 1) in
  loop 0
[%%expect {|
Line 3, characters 2-6:
3 |   loop 0
      ^^^^
Error: This value is "local"
         because it is allocated at line 2, characters 15-53 containing data
         which is "local" to the parent region
         because it closes over the value "r" at line 2, characters 27-28
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

type t = { mutable x : int }
let make () @ portable = { x = 1 }

type 'a t = { mutable x : 'a }
let make () @ portable = { x = 1 }

[%%expect {|
type t = { mutable x : int; }
val make : unit -> t @ portable = <fun>
type 'a t = { mutable x : 'a; }
Line 5, characters 25-34:
5 | let make () @ portable = { x = 1 }
                             ^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]
