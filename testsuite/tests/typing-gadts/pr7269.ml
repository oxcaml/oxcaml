(* TEST
   expect;
*)

type s =
  [ `A
  | `B ]

and sub = [`B]

type +'a t = T : [< `Conj of 'a & sub | `Other of string] -> 'a t

(* ok *)

let f (T (`Other msg) : s t) = print_string msg

let _ = f (T (`Conj `B) :> s t)

(* warn *)
[%%expect
{|
type s = [ `A | `B ]
and sub = [ `B ]
type +'a t = T : [< `Conj of 'a & sub | `Other of string ] -> 'a t
Line 11, characters 6-28:
11 | let f (T (`Other msg) : s t) = print_string msg
           ^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
T (`Conj _)

val f : s t -> unit = <fun>
Exception: Match_failure ("", 11, 6).
|}]

module M : sig
  type s

  type t = T : [< `Conj of int & s | `Other of string] -> t

  val x : t
end = struct
  type s = int

  type t = T : [< `Conj of int | `Other of string] -> t

  let x = T (`Conj 42)
end

let () = M.(match x with T (`Other msg) -> print_string msg)

(* warn *)
[%%expect
{|
module M :
  sig
    type s
    type t = T : [< `Conj of int & s | `Other of string ] -> t
    val x : t
  end
Line 15, characters 12-59:
15 | let () = M.(match x with T (`Other msg) -> print_string msg)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
T (`Conj _)

Exception: Match_failure ("", 15, 12).
|}]

module M : sig
  type s

  type elim =
    { ex : 'a. ([< `Conj of int & s | `Other of string] as 'a) -> unit }

  val e : elim -> unit
end = struct
  type s = int

  type elim = { ex : 'a. ([< `Conj of int | `Other of string] as 'a) -> unit }

  let e { ex } = ex (`Conj 42 : [`Conj of int])
end

let () = M.(e { ex = (fun (`Other msg) -> print_string msg) })

(* warn *)
[%%expect
{|
module M :
  sig
    type s
    type elim = {
      ex : 'a. ([< `Conj of int & s | `Other of string ] as 'a) -> unit;
    }
    val e : elim -> unit
  end
Line 16, characters 26-38:
16 | let () = M.(e { ex = (fun (`Other msg) -> print_string msg) })
                               ^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
`Conj _

Exception: Match_failure ("", 16, 26).
|}]
