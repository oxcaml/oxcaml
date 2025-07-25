(* TEST
   expect;
*)

module Add (T : sig
  type two
end) =
struct
  type _ t =
    | One : [`One] t
    | Two : T.two t

  let add (type a) : a t * a t -> string = function
    | One, One -> "two"
    | Two, Two -> "four"
end

[%%expect
{|
Lines 9-11, characters 43-24:
 9 | ...........................................function
10 |     | One, One -> "two"
11 |     | Two, Two -> "four"
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(One, Two)

module Add :
  functor (T : sig type two end) ->
    sig
      type _ t = One : [ `One ] t | Two : T.two t
      val add : 'a t * 'a t -> string
    end
|}]
