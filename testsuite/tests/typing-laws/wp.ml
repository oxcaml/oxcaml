(* TEST
 flags = "-extension laws";
 expect;
*)

(* Logical equality [=:=], conjunctions of clauses, [let%wp] and modes on
   parameters. [e1 = e2] is still the boolean equality of OCaml. *)

module M = struct
  let rec first p n = if p n then n else first p (n + 1)
  let rev = List.rev
  law? rev_rev (xs : 'a list) : rev (rev xs) =:= xs
  law? bool_eq (n : int) : n + 0 = n
  law? both (xs : int list) : rev [] =:= [] && rev (rev xs) =:= xs
  law? first_correct (p : int -> bool) (n : int) :
    let%wp i = first p n in n <= i && p i
  law? first_premise (p : int -> bool) (n : int) :
    p n =:= true ===> let%wp i = first p n in i =:= n
  law? nested (p : int -> bool) (n : int) :
    let%wp i = first p n in let%wp j = first p i in i =:= j
  law? local_param (xs : int list @ local) :
    let%wp () = () in xs =:= xs
  law? local_result (n : int) : let%wp xs @ local = [ n ] in xs =:= [ n ]
end
[%%expect {|
module M :
  sig
    val first : (int -> bool) -> int -> int
    val rev : 'a list -> 'a list
    law? rev_rev (xs : 'a list) : (rev (rev xs)) =:= xs
    law? bool_eq (n : int) : (n + (0)) = n
    law? both (xs : int list) :
      (rev ([] : _ list)) =:= ([] : _ list) && (rev (rev xs)) =:= xs
    law? first_correct (p : int -> bool) (n : int) :
      let%wp i = first p n in (n <= i) && (p i)
    law? first_premise (p : int -> bool) (n : int) :
      (p n) =:= (true : bool) ===> let%wp i = first p n in i =:= n
    law? nested (p : int -> bool) (n : int) :
      let%wp i = first p n in let%wp j = first p i in i =:= j
    law? local_param (xs : int list @ local) :
      let%wp (() : unit) = (() : unit) in xs =:= xs
    law? local_result (n : int) :
      let%wp xs @ local = (( :: ) (n, ([] : _ list)) : _ list) in
      xs =:= (( :: ) (n, ([] : _ list)) : _ list)
  end
|}]

(* The modes of a parameter are those of its uses. *)

let keep (x : 'a) = x
law? escapes (xs : int list @ local) : keep xs =:= xs
[%%expect {|
val keep : 'a -> 'a = <fun>
Line 2, characters 44-46:
2 | law? escapes (xs : int list @ local) : keep xs =:= xs
                                                ^^
Error: This value is "local" but is expected to be "global".
|}]

(* The law-level forms cannot occur inside expressions. *)

law? misplaced (x : int) : not (x =:= x)
[%%expect {|
Line 1, characters 34-37:
1 | law? misplaced (x : int) : not (x =:= x)
                                      ^^^
Error: "=:=" is a clause of a law, not an expression: it may only occur
       at the root of a clause, under "&&" or as the body of "let%wp".
|}]

law? misplaced_wp (x : int) : (let%wp y = x in y = x) || true
[%%expect {|
Line 1, characters 30-53:
1 | law? misplaced_wp (x : int) : (let%wp y = x in y = x) || true
                                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: "let%wp" is a clause of a law, not an expression: it may only occur
       at the root of a clause, under "&&" or as the body of "let%wp".
|}]

law? malformed_wp (x : int) : [%wp x]
[%%expect {|
Line 1, characters 30-37:
1 | law? malformed_wp (x : int) : [%wp x]
                                  ^^^^^^^
Error: The extension "wp" in a law must be "let%wp p = e in Q"
       with a single, non-recursive binding.
|}]

law? recursive_wp (x : int) : let%wp rec f y = y in f x =:= x
[%%expect {|
Line 1, characters 30-61:
1 | law? recursive_wp (x : int) : let%wp rec f y = y in f x =:= x
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "wp" in a law must be "let%wp p = e in Q"
       with a single, non-recursive binding.
|}]

(* The two sides of [=:=] have the same type. *)

law? ill_typed (x : int) : x =:= true
[%%expect {|
Line 1, characters 33-37:
1 | law? ill_typed (x : int) : x =:= true
                                     ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]

(* An implementation must give the parameters of a law the modes of the
   interface. *)

module N : sig
  law? m (xs : int list @ local) : xs =:= xs
end = struct
  law? m (xs : int list) : xs =:= xs
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? m (xs : int list) : xs =:= xs
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? m (xs : int list) : xs =:= xs end
       is not included in
         sig law? m (xs : int list @ local) : xs =:= xs end
       Laws do not match:
         law? m (xs : int list) : xs =:= xs
       is not included in
         law? m (xs : int list @ local) : xs =:= xs
       The parameter "xs" has no modes in the first law, but the modes "local"
       in the second.
|}]
