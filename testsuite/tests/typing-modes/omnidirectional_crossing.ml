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
Line 2, characters 4-5:
2 |   ((x : _ @ portable), unify x (Some None), unify x (Some (Some (fun () -> ()))))
        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

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
val doesn't_work : unit = ()
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

(* Defaulting waits for the end of the scope that generalizes the variable *)

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

let f (x @ nonportable) =
  ((x : _ @ portable),
   (let module M : module type of struct end = struct end in ()),
   ignore (x = Some 1))
[%%expect{|
val f : int option -> int option * unit * unit = <fun>
|}]

(* Top-level items default at their end *)

module F (X : sig end) = struct
  let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
[%%expect{|
module F :
  functor (X : sig end) -> sig val f : int option -> int option * unit end
|}]

module F (X : sig end) = struct
  let r = ref None
  let g (x @ nonportable) = r := Some (x : _ @ portable)
end
[%%expect{|
Line 3, characters 39-40:
3 |   let g (x @ nonportable) = r := Some (x : _ @ portable)
                                           ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

include struct
  let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
[%%expect{|
val f : int option -> int option * unit = <fun>
|}]

class c = object
  method m (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
[%%expect{|
class c : object method m : int option -> int option * unit end
|}]

(* Weak variables *)

(* Defaulted weak variables can be printed *)
let r = (fun x -> ref x) (Obj.magic 0)
[%%expect{|
val r : '_weak1 ref = {contents = <poly>}
|}]

(* A weak variable waited on by a later phrase *)
let r = ref None
[%%expect{|
val r : '_weak2 option ref = {contents = None}
|}]

let g (x @ portable) = r := Some x
[%%expect{|
val g : '_weak2 @ portable -> unit = <fun>
|}]

let _ = r
[%%expect{|
- : '_weak2 option ref = {contents = None}
|}]

(* A failed phrase leaves nothing pending *)
let r = ref None
let f (x @ nonportable) = r := Some (x : _ @ portable); 1 + ""
[%%expect{|
val r : '_weak3 option ref = {contents = None}
Line 2, characters 60-62:
2 | let f (x @ nonportable) = r := Some (x : _ @ portable); 1 + ""
                                                                ^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

let g (x @ portable) = r := Some x
let _ = r
[%%expect{|
val g : '_weak3 @ portable -> unit = <fun>
- : '_weak3 option ref = {contents = None}
|}]

(* [module type of] defaults its own scope before checking generalizability *)

module type S = module type of struct
  let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
[%%expect{|
module type S = sig val f : int option -> int option * unit @@ portable end
|}]

module type S = module type of struct let r = ref [] end
[%%expect{|
Line 1, characters 31-56:
1 | module type S = module type of struct let r = ref [] end
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type of this module, sig val r : '_weak4 list ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 1, characters 42-43:
1 | module type S = module type of struct let r = ref [] end
                                              ^
  The type of this value, "'_weak4 list ref",
  contains the non-generalizable type variable(s) "'_weak4".
|}]

module type S = module type of struct
  let r = ref None
  let g (x @ portable) = r := Some x
end
[%%expect{|
Lines 1-4, characters 31-3:
1 | ...............................struct
2 |   let r = ref None
3 |   let g (x @ portable) = r := Some x
4 | end
Error: The type of this module,
       sig val r : '_weak5 option ref val g : '_weak5 @ portable -> unit end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 2, characters 6-7:
2 |   let r = ref None
          ^
  The type of this value, "'_weak5 option ref",
  contains the non-generalizable type variable(s) "'_weak5".
|}]

(* Checks delayed by an inclusion check are done before it returns *)

module M : sig
  val f : 'a @ local -> 'a ref
end = struct
  let f (x : 'a) = ref x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : 'a) = ref x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a ref end
       is not included in
         sig val f : 'a @ local -> 'a ref end
       Values do not match:
         val f : 'a -> 'a ref
       is not included in
         val f : 'a @ local -> 'a ref
       The type "'a -> 'a ref" is not compatible with the type
         "'a @ local -> 'a ref"
       The argument mode was expected to be "global"
       because it crosses with something but is "local"
|}]

(* The synchronous error is preferred *)
module M : sig
  val f : ('a @ local -> 'a ref) * string
end = struct
  let f = ((fun (x : 'a) -> ref x), 1)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f = ((fun (x : 'a) -> ref x), 1)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a -> 'a ref) * int end
       is not included in
         sig val f : ('a @ local -> 'a ref) * string end
       Values do not match:
         val f : ('a -> 'a ref) * int
       is not included in
         val f : ('a @ local -> 'a ref) * string
       The type "('a -> 'a ref) * int" is not compatible with the type
         "('a @ local -> 'a ref) * string"
       Type "int" is not compatible with type "string"
|}]

class type ['a] ct = object method m : 'a @ local -> 'a ref end
class ['a] c : ['a] ct = object method m (x : 'a) = ref x end
[%%expect{|
class type ['a] ct = object method m : 'a @ local -> 'a ref end
Line 2, characters 25-61:
2 | class ['a] c : ['a] ct = object method m (x : 'a) = ref x end
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type object method m : 'a -> 'a ref end
       is not matched by the class type ['_a] ct
       The class type object method m : 'a -> 'a ref end
       is not matched by the class type
         object method m : 'a @ local -> 'a ref end
       The method m has type "'a -> 'a ref" but is expected to have type
         "'a @ local -> 'a ref"
       Type "'a -> 'a ref" is not compatible with type "'a @ local -> 'a ref"
       The argument mode was expected to be "global"
       because it crosses with something but is "local"
|}]

(* The synchronous error is preferred *)
class type ['a] ct = object
  method m : 'a @ local -> 'a ref
  method n : string
end
class ['a] c : ['a] ct = object method m (x : 'a) = ref x method n = 1 end
[%%expect{|
class type ['a] ct =
  object method m : 'a @ local -> 'a ref method n : string end
Line 5, characters 25-74:
5 | class ['a] c : ['a] ct = object method m (x : 'a) = ref x method n = 1 end
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The class type object method m : 'a -> 'a ref method n : int end
       is not matched by the class type ['_a] ct
       The class type object method m : 'a -> 'a ref method n : int end
       is not matched by the class type
         object method m : 'a @ local -> 'a ref method n : string end
       The method n has type "int" but is expected to have type "string"
       Type "int" is not compatible with type "string"
|}]

(* The earliest failing use is reported *)

let f (x @ nonportable) =
  ((x : _ @ portable), (x : _ @ portable), x ())
[%%expect{|
Line 2, characters 4-5:
2 |   ((x : _ @ portable), (x : _ @ portable), x ())
        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]
