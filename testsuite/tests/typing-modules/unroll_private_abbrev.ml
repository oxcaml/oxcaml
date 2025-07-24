(* TEST
   expect;
*)

module M : sig
  type t = private [`Bar of 'a | `Foo] as 'a

  val bar : t
end = struct
  type t = [`Bar of 'a | `Foo] as 'a

  let bar = `Bar `Foo
end

[%%expect
{|
module M : sig type t = private [ `Bar of 'a | `Foo ] as 'a val bar : t end
|}]

let y =
  match (M.bar :> [`Bar of 'a | `Foo] as 'a) with
  | `Bar x -> x
  | `Foo -> assert false

[%%expect {|
val y : [ `Bar of 'a | `Foo ] as 'a = `Foo
|}]

let y =
  match (M.bar :> [`Bar of M.t | `Foo]) with
  | `Bar x -> x
  | `Foo -> assert false

[%%expect
{|
Line 2, characters 8-39:
2 |   match (M.bar :> [`Bar of M.t | `Foo]) with
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "M.t" is not a subtype of "[ `Bar of M.t | `Foo ]"
       Type "M.t" = "[ `Bar of M.t | `Foo ]" is not a subtype of "M.t"
|}]

module F (X : sig end) : sig
  type s = private [`Bar of 'a | `Foo] as 'a

  val from : M.t -> s

  val to_ : s -> M.t
end = struct
  type s = M.t

  let from x = x

  let to_ x = x
end

[%%expect
{|
module F :
  functor (X : sig end) ->
    sig
      type s = private [ `Bar of 'a | `Foo ] as 'a
      val from : M.t -> s
      val to_ : s -> M.t
    end
|}]

module N = F ()

[%%expect
{|
Line 1, characters 11-15:
1 | module N = F ()
               ^^^^
Error: The functor was expected to be applicative at this position
|}]

let y =
  match (N.from M.bar :> [`Bar of N.s | `Foo]) with
  | `Bar x -> N.to_ x
  | `Foo -> assert false

[%%expect
{|
Line 2, characters 34-37:
2 |   match (N.from M.bar :> [`Bar of N.s | `Foo]) with
                                      ^^^
Error: Unbound module "N"
|}]
