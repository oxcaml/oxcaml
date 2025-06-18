(* TEST
 flambda;
 native;
*)

module S : sig
  type id
  type t
  type view =
    | Sub of t * t
    | Add of t * t
    | Id of id
  val view : t -> view
  val v : t
end = struct
  type id = int
  type t =
    | Id of id
    | Add of t * t
    | Sub of t * t

  type view =
    | Sub of t * t
    | Add of t * t
    | Id of id

  let[@inline] view (t : t) : view =
    match t with
    | Id id -> Id id
    | Add (x, y) -> Add (x, y)
    | Sub (x, y) -> Sub (x, y)

  let v : t = Add (Id 1, Sub (Id 1, Id 1))
end

let rec eval env t =
  match S.view t with
  | Id id -> env id
  | Add (x, y) -> eval env x + eval env y
  | Sub (x, y) -> eval env x - eval env y

let run () =
  let env _ = 42 in
  let before = Gc.minor_words () in
  let _ = Sys.opaque_identity (eval env S.v) in
  let after = Gc.minor_words () in
  Format.printf "%g\n%!" (after -. before)
[@@inline never]

let () = run ()
