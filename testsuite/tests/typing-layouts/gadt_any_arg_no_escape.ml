(* TEST *)

(* Computing constructor argument sorts must not narrow GADT equation scopes.
   This example relies on non-principal propagation of the return type. *)

type a = A_value
type b = B_value
type 'v witness = A : a witness | B : b witness

let f (type v) (witness : v witness) : (v, string) Result.t =
  Result.bind (Ok ()) (fun () ->
    match witness with
    | A -> Result.bind (Ok ()) (fun () -> Ok (A_value : v))
    | B -> Ok B_value)
