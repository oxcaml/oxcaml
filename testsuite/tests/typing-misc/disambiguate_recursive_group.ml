(* TEST
 expect;
*)

(* oxcaml#7459

   Without type information, a constructor shared by types in the same recursive
   group should resolve to the last one, whether the group is defined in the
   current environment or brought in by [open]. *)

let unify x y = ignore (x = y)

type tx = Foo of ty

and ty = Foo of tx

let cycle x y =
  unify x (Foo y);
  unify y (Foo x)

[%%expect
{|
val unify : 'a -> 'a -> unit = <fun>
type tx = Foo of ty
and ty = Foo of tx
val cycle : ty -> tx -> unit = <fun>
|}, Principal{|
val unify : 'a -> 'a -> unit = <fun>
type tx = Foo of ty
and ty = Foo of tx
Line 9, characters 11-14:
9 |   unify y (Foo x)
               ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val cycle : ty -> tx -> unit = <fun>
|}]

module C = struct
  type tx = Foo of ty

  and ty = Foo of tx
end

let cycle x y =
  let open C in
  unify x (Foo y);
  unify y (Foo x)

[%%expect
{|
module C : sig type tx = Foo of ty and ty = Foo of tx end
val cycle : C.ty -> C.tx -> unit = <fun>
|}, Principal{|
module C : sig type tx = Foo of ty and ty = Foo of tx end
Line 10, characters 11-14:
10 |   unify y (Foo x)
                ^^^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val cycle : C.ty -> C.tx -> unit = <fun>
|}]
