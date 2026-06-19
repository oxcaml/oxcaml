(* TEST
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression test: computing the sort of a constructor argument whose layout
   is [any] (so the constructor has a variable representation) must not leak the
   [scope] of an expanded GADT equation onto the argument type. Otherwise a
   sound program whose result flows through a nested application is wrongly
   rejected with a spurious "escape the scope of its equation" error.

   This is exercised in the default (non-principal) type-inference mode; the
   program legitimately relies on outside-in propagation of the return type and
   is, as expected, not accepted under [-principal] (the same is true if the
   constructor arguments have layout [value_or_null]). *)

type ('a : any, 'e : any) res = Ok2 of 'a | Err2 of 'e

let bind2 (type a b e) (r : (a, e) res) (f : a -> (b, e) res) : (b, e) res =
  match r with Ok2 x -> f x | Err2 e -> Err2 e

type a = A_value
type b = B_value
type _ witness = A : a witness | B : b witness

let _f (type msg) (witness : msg witness) : (msg, string) res =
  bind2 (Ok2 ()) (fun () ->
    match witness with
    | A -> bind2 (Ok2 ()) (fun () -> Ok2 (A_value : msg))
    | B -> Ok2 B_value)

(* The same shape using the standard library's [result], whose parameters have
   layout [any]. *)
let _g (type msg) (witness : msg witness) : (msg, string) Result.t =
  Result.bind (Ok ()) (fun () ->
    match witness with
    | A -> Result.bind (Ok ()) (fun () -> Ok (A_value : msg))
    | B -> Ok B_value)
