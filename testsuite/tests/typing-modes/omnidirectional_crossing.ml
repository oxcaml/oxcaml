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
Line 1, characters 31-32:
1 | let f (local_ x) = (x, ignore (x ^ ""))
                                   ^
Error: This value is "local" to the parent region but is expected to be "global".
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
Line 1, characters 7-8:
1 | let f (x @ nonportable) =
           ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* CR-soon omni: this test should change once defaulting is lazier *)
let f (x @ nonportable) =
  ((x : _ @ portable), unify x (Some None), unify x (Some (Some 1)))
[%%expect{|
val f : int option option -> int option option * unit * unit = <fun>
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
(* XCR-soon omni: this fails due to eager defaulting

   aobrien: This fails due to order dependence in typing (not mode crossing).
   Essentially, the type ['a t] is unified with [foo]'s argument type,
   which results in the subkinding ['a t <= value mod portable contended],
   *before* we start typing the arguments. *)
let doesn't_work = foo (Variant ())
[%%expect {|
type 'a t = Variant of 'a
val foo : ('a : value mod portable contended). 'a -> unit = <fun>
val variant : unit t = Variant ()
val works1 : unit = ()
val works2 : unit = ()
Line 14, characters 23-35:
14 | let doesn't_work = foo (Variant ())
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
val f : int @ local -> int = <fun>
|}]

type t = { mutable x : int }
let make () @ portable = { x = 1 }

[%%expect {|
type t = { mutable x : int; }
val make : unit -> t @ portable = <fun>
|}]

type 'a t = { mutable x : 'a }
let make () @ portable = { x = 1 }

[%%expect {|
type 'a t = { mutable x : 'a; }
val make : unit -> int t @ portable = <fun>
|}]

(* Mutable fields kept by a record update are crossed at the instance of
   their type. *)
type 'a u = { mutable m : 'a; k : int }
let update r @ portable = ({ r with k = 1 }, ignore (r.m + 1))
[%%expect {|
type 'a u = { mutable m : 'a; k : int; }
val update : int u -> int u * unit @ portable = <fun>
|}]

(* Other crossing sites *)

let alias (local_ x) = match x with (_ as y) -> (y, ignore (y + 1))
[%%expect {|
val alias : int @ local -> int * unit = <fun>
|}]

let array (local_ x) = ([| x |], ignore (x + 1))
[%%expect {|
val array : int @ local -> int array * unit = <fun>
|}]

let return (local_ x) = exclave_ (fun y -> (y, ignore (y + 1))) x
[%%expect {|
val return : int @ local -> int * unit @ local = <fun>
|}]

type 'a box = { contents : 'a }
let field (local_ r) = (r.contents, ignore (r.contents + 1))
[%%expect {|
type 'a box = { contents : 'a; }
val field : int box @ local -> int * unit = <fun>
|}]

(* Pending variables copied by [Subst] *)

let exhaustive (local_ x) =
  ((match x with exception Not_found -> x | y -> y), ignore (x + 1))
[%%expect {|
val exhaustive : int @ local -> int * unit = <fun>
|}]

type _ gadt = Int : int gadt | Bool : bool gadt
[%%expect {|
type _ gadt = Int : int gadt | Bool : bool gadt
|}]

let gadt_branches (type a) (w : a gadt) (local_ x) =
  ((match w with Int -> x | Bool -> x), ignore (x + 1))
[%%expect {|
val gadt_branches : 'a gadt -> int @ local -> int * unit = <fun>
|}]

let newtype = fun (type a) (local_ x) -> (x, ignore (x + 1))
[%%expect {|
val newtype : int @ local -> int * unit = <fun>
|}]

(* Pending variables unified under a GADT equation *)

let gadt_equation : type a. a gadt -> a option = fun w ->
  let r = ref None in
  (match w with Int -> r := Some (1 : a) | Bool -> ());
  !r
[%%expect {|
val gadt_equation : 'a gadt -> 'a option = <fun>
|}]

type 'a field = { fld : 'a }
let gadt_field (type a) (w : a gadt) (y : a) =
  match w with Int -> { fld = y }.fld | Bool -> y
[%%expect {|
type 'a field = { fld : 'a; }
val gadt_field : 'a gadt -> 'a -> 'a = <fun>
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), (match x with Some 1 -> () | _ -> ()))
[%%expect {|
val f : int option -> int option * unit = <fun>
|}]

let f (x @ nonportable) =
  let p = ((x : _ @ portable), ()) in
  ignore (x = Some 1);
  p
[%%expect {|
val f : int option -> int option * unit = <fun>
|}]

(* We only default completely at the toplevel *)

let f (x @ nonportable) =
  ((x : _ @ portable), (let module M = struct let y = () end in M.y), ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit * unit = <fun>
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), (let module M = struct end in ()), ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit * unit = <fun>
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), (let open struct let y = () end in y), ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit * unit = <fun>
|}]

let f (x @ nonportable) =
  ((x : _ @ portable), (let open struct end in ()), ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit * unit = <fun>
|}]
