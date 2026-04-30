(* TEST
  expect;
*)

open Effect
open Effect.Deep

let use_unique : 'a @ unique -> unit = fun _ -> ()

type _ eff += Need_ref : int ref eff

type _ eff += Need_unit : unit eff

type _ eff += Payload : int ref -> unit eff
[%%expect {|
val use_unique : 'a @ unique -> unit = <fun>
type _ eff += Need_ref : int ref eff
type _ eff += Need_unit : unit eff
type _ eff += Payload : int ref -> unit eff
|}]

(* A result returned by [perform] is aliased, not unique. *)
let () =
  match perform Need_ref with
  | r -> use_unique r
  | effect Need_ref, k -> continue k (ref 0)
[%%expect {|
Line 3, characters 20-21:
3 |   | r -> use_unique r
                        ^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* A continuation result cannot be a local reference escaping globally. *)
let _ =
  match perform Need_ref with
  | r -> ignore !r
  | effect Need_ref, k -> continue k (stack_ (ref 0))
[%%expect {|
Line 4, characters 37-53:
4 |   | effect Need_ref, k -> continue k (stack_ (ref 0))
                                         ^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* A payload captured by an effect pattern is aliased, not unique. *)
let () =
  match perform (Payload (ref 0)) with
  | () -> ()
  | effect (Payload x), k ->
      use_unique x;
      continue k ()
[%%expect {|
Line 5, characters 17-18:
5 |       use_unique x;
                     ^
Error: This value is "aliased"
         because it is contained (via constructor "Payload") in the value at line 4, characters 11-22
         which is "aliased".
       However, the highlighted expression is expected to be "unique".
|}]

(* The handled expression of [match] cannot capture a local from outside the
   generated handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  match !r with
  | n -> n
  | effect Need_unit, k -> continue k ()
[%%expect {|
- : int = 0
|}]

(* A value clause of [match] cannot capture a local from outside the generated
   handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  match perform Need_unit with
  | () -> !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
- : int = 0
|}]

(* An effect clause of [match] cannot capture a local from outside the generated
   handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      ignore !r;
      continue k ()
[%%expect {|
- : unit = ()
|}]

(* A [try] continuation result cannot be a local reference escaping globally. *)
let _ =
  try perform Need_ref with
  | effect Need_ref, k -> continue k (stack_ (ref 0))
[%%expect {|
Line 3, characters 37-53:
3 |   | effect Need_ref, k -> continue k (stack_ (ref 0))
                                         ^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(* The body of [try] cannot capture a local from outside the generated handler
   boundary. *)
let _ =
  let (r @ local) = ref 0 in
  try !r with
  | effect Need_unit, k -> continue k ()
[%%expect {|
- : int = 0
|}]

(* An exception clause of [try] cannot capture a local from outside the
   generated handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  try raise Exit with
  | Exit -> !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
- : int = 0
|}]

(* An effect clause of [try] cannot capture a local from outside the generated
   handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  try
    perform Need_unit;
    0
  with
  | Exit -> 0
  | effect Need_unit, k ->
      ignore !r;
      continue k ()
[%%expect {|
- : int = 0
|}]

(* Can allocate local values inside a [match] body. *)
let match_body_internal_local () =
  match
    let (r @ local) = ref 0 in
    !r
  with
  | n -> n
  | effect Need_unit, k -> continue k ()
[%%expect {|
val match_body_internal_local : unit -> int = <fun>
|}]

(* Can allocate local values inside a [try] body. *)
let try_body_internal_local () =
  try
    let (r @ local) = ref 1 in
    !r
  with
  | effect Need_unit, k -> continue k ()
[%%expect {|
val try_body_internal_local : unit -> int = <fun>
|}]

(* Can allocate local values inside a [match] value clause. *)
let match_value_clause_internal_local () =
  match 2 with
  | n ->
      let (r @ local) = ref n in
      !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
val match_value_clause_internal_local : unit -> int = <fun>
|}]

(* Can allocate local values inside a [match] effect clause. *)
let match_effect_clause_internal_local () =
  match perform Need_unit with
  | () -> 0
  | effect Need_unit, k ->
      let (r @ local) = ref 3 in
      ignore !r;
      continue k ()
[%%expect {|
val match_effect_clause_internal_local : unit -> int = <fun>
|}]

(* Can allocate local values inside a [try] exception clause. *)
let try_exception_clause_internal_local () =
  try raise Exit with
  | Exit ->
      let (r @ local) = ref 4 in
      !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
val try_exception_clause_internal_local : unit -> int = <fun>
|}]

(* Can allocate local values inside a [try] effect clause. *)
let try_effect_clause_internal_local () =
  try
    perform Need_unit;
    0
  with
  | Exit -> 0
  | effect Need_unit, k ->
      let (r @ local) = ref 5 in
      ignore !r;
      continue k ()
[%%expect {|
val try_effect_clause_internal_local : unit -> int = <fun>
|}]

(* A global value may be used in a handled [match] body. *)
let match_body_global_capture () =
  let r = ref 6 in
  match !r with
  | n -> n
  | effect Need_unit, k -> continue k ()
[%%expect {|
val match_body_global_capture : unit -> int = <fun>
|}]

(* A global value may be used in handled [match] clauses. *)
let match_handler_global_capture () =
  let r = ref 7 in
  match perform Need_unit with
  | () -> !r
  | effect Need_unit, k ->
      ignore !r;
      continue k ()
[%%expect {|
val match_handler_global_capture : unit -> int = <fun>
|}]

(* A global value may be used in a handled [try] body. *)
let try_body_global_capture () =
  let r = ref 8 in
  try !r with
  | effect Need_unit, k -> continue k ()
[%%expect {|
val try_body_global_capture : unit -> int = <fun>
|}]

(* A global value may be used in handled [try] clauses. *)
let try_handler_global_capture () =
  let r = ref 9 in
  try
    perform Need_unit;
    0
  with
  | Exit -> !r
  | effect Need_unit, k ->
      ignore !r;
      continue k ()
[%%expect {|
val try_handler_global_capture : unit -> int = <fun>
|}]
