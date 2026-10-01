(* TEST *)

(* XXX Rewrite Claude comments *)

(* Regression test: computing the sort of a constructor argument whose layout
   is [any] (so the constructor has a variable representation) must not leak the
   [scope] of an expanded GADT equation onto the argument type. Otherwise a
   sound program whose result flows through a nested application is wrongly
   rejected with a spurious "escape the scope of its equation" error.

   This is exercised in the default (non-principal) type-inference mode; the
   program legitimately relies on outside-in propagation of the return type and
   is, as expected, not accepted under [-principal] (the same is true if the
   constructor arguments have layout [value_or_null]). *)

type a = A_value
type b = B_value
type 'v witness = A : a witness | B : b witness

let f (type v) (witness : v witness) : (v, string) Result.t =
  Result.bind (Ok ()) (fun () ->
    match witness with
    | A -> Result.bind (Ok ()) (fun () -> Ok (A_value : v))
    | B -> Ok B_value)
