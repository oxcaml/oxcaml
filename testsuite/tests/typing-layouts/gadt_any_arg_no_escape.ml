(* TEST *)

(* Regression test: When determining a constructor's representation at a use
   site with given argument types, expanding GADT equations must not limit the
   scopes of those argument types. The [Ok (A_value : v)] below needs the
   representation of [Ok _ : (v, string) result], which expands the [v = a]
   discovered by the match. If that expansion attaches [v] to the scope of the
   match, then it won't be allowed to flow out of the match to agree with the
   [(v, string) result] in [f]'s return type.

   Doesn't apply in [-principal] mode, which forbids relying on the type
   flowing down from the function return to begin with. *)

type a = A_value
type b = B_value
type 'v witness = A : a witness | B : b witness

(* This test is only interesting because [result] takes an [any]. *)
[@@@ocaml.warning "@imprecise-kind-annotation"]
type ('a : any, 'b : any) result = ('a, 'b) Result.t =
  | Ok of 'a
  | Error of 'b

let f (type v) (witness : v witness) : (v, string) result =
  Result.bind (Ok ()) (fun () ->
    match witness with
    | A -> Result.bind (Ok ()) (fun () -> Ok (A_value : v))
    | B -> Ok B_value)
