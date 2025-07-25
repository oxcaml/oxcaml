(* TEST
   expect;
*)

(**
   Check the behavior of with constraints with respect to
    ghost type items introduced for class and class types
 *)

module type s = sig
  class type c =
    object
      method m : int
    end
end
with type c := < m : int >

[%%expect
{|
Lines 6-12, characters 16-26:
 6 | ................sig
 7 |   class type c =
 8 |     object
 9 |       method m : int
10 |     end
11 | end
12 | with type c := < m : int >
Error: The signature constrained by "with" has no component named "c"
|}]

module type s = sig
  class type ct =
    object
      method m : int
    end
end
with type ct := < m : int >

[%%expect
{|
Lines 1-7, characters 16-27:
1 | ................sig
2 |   class type ct =
3 |     object
4 |       method m : int
5 |     end
6 | end
7 | with type ct := < m : int >
Error: The signature constrained by "with" has no component named "ct"
|}]

(** Check that we keep the same structure even after replacing a ghost item *)

module type s = sig
  type top

  and t = private < .. >

  and mid

  and u = private < .. >

  and v
end
with type t = private < .. >
with type u = private < .. >

[%%expect
{|
module type s =
  sig
    type top
    and t = private < .. >
    and mid
    and u = private < .. >
    and v
  end
|}]
