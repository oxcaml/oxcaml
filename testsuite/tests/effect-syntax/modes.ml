(* TEST
  flags += "-keywords 5.3";
  expect;
*)

open Effect
open Effect.Deep

let use_unique : 'a @ unique -> unit = fun _ -> ()

let use_portable : 'a @ portable -> unit = fun _ -> ()

let use_portable_function : (unit -> unit) @ portable -> unit = fun _ -> ()

let use_stateless : 'a @ stateless -> unit = fun _ -> ()

let use_contended : 'a @ contended -> unit = fun _ -> ()

let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()

let use_immutable : 'a @ immutable -> unit = fun _ -> ()

let use_read_write : 'a @ read_write -> unit = fun _ -> ()

type record = { a : int }

type _ eff += Need_ref : int ref eff

type _ eff += Need_unit : unit eff

type _ eff += Payload : int ref -> unit eff
[%%expect {|
val use_unique : 'a @ unique -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_portable_function : (unit -> unit) @ portable -> unit = <fun>
val use_stateless : 'a @ stateless -> unit = <fun>
val use_contended : 'a @ contended -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_immutable : 'a @ immutable -> unit = <fun>
val use_read_write : 'a -> unit = <fun>
type record = { a : int; }
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

(* A result returned by [perform] is aliased, not unique. *)
let () =
  use_unique (perform Need_ref)
[%%expect {|
Line 2, characters 13-31:
2 |   use_unique (perform Need_ref)
                 ^^^^^^^^^^^^^^^^^^
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

(* The continuation passed to [continue] must be legacy. *)
let () =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      let (k @ local) = k in
      continue k ()
[%%expect {|
Line 6, characters 15-16:
6 |       continue k ()
                   ^
Error: This value is "local" but is expected to be "global".
|}]

(* A result returned by [continue] is aliased, not unique. *)
let () =
  match perform Need_ref with
  | r -> ignore !r
  | effect Need_ref, k -> use_unique (continue k (ref 0))
[%%expect {|
Line 4, characters 37-57:
4 |   | effect Need_ref, k -> use_unique (continue k (ref 0))
                                         ^^^^^^^^^^^^^^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* An effect continuation is legacy, not portable. *)
let () =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k -> use_portable k
[%%expect {|
Line 4, characters 40-41:
4 |   | effect Need_unit, k -> use_portable k
                                            ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* An effect payload passed to [perform] must be legacy. *)
let () =
  perform (Payload (stack_ (ref 0)))
[%%expect {|
Line 2, characters 19-35:
2 |   perform (Payload (stack_ (ref 0)))
                       ^^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "global"
         because it is contained (via constructor "Payload") in the value at line 2, characters 10-36
         which is expected to be "global".
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

(* A scrutinee of [match] cannot capture a unique value from outside the
   generated handler boundary. *)
let () =
  let r = { a = 42 } in
  match use_unique r with
  | () -> ()
  | effect Need_unit, k -> continue k ()
[%%expect {|
Line 3, characters 19-20:
3 |   match use_unique r with
                       ^
Error: This value is "aliased"
         because it is used in a pattern match with effect cases (at lines 3-5, characters 2-40).
       However, the highlighted expression is expected to be "unique".
|}]

(* A value clause of [match] cannot capture a unique value from outside the
   generated handler boundary. *)
let () =
  let r = { a = 42 } in
  match () with
  | n -> use_unique r
  | effect Need_unit, k -> continue k ()
[%%expect {|
Line 4, characters 20-21:
4 |   | n -> use_unique r
                        ^
Error: This value is "aliased"
         because it is used in a pattern match with effect cases (at lines 3-5, characters 2-40).
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
Line 3, characters 9-10:
3 |   match !r with
             ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a pattern match with effect cases (at lines 3-5, characters 2-40).
|}]

(* A value clause of [match] cannot capture a local from outside the generated
   handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  match perform Need_unit with
  | () -> !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
Line 4, characters 11-12:
4 |   | () -> !r
               ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a pattern match with effect cases (at lines 3-5, characters 2-40).
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
Line 6, characters 14-15:
6 |       ignore !r;
                  ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a pattern match with effect cases (at lines 3-7, characters 2-19).
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
Line 3, characters 7-8:
3 |   try !r with
           ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a try-with with effect cases (at lines 3-4, characters 2-40).
|}]

(* An exception clause of [try] cannot capture a local from outside the
   generated handler boundary. *)
let _ =
  let (r @ local) = ref 0 in
  try raise Exit with
  | Exit -> !r
  | effect Need_unit, k -> continue k ()
[%%expect {|
Line 4, characters 13-14:
4 |   | Exit -> !r
                 ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a try-with with effect cases (at lines 3-5, characters 2-40).
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
Line 9, characters 14-15:
9 |       ignore !r;
                  ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used in a try-with with effect cases (at lines 3-10, characters 2-19).
|}]

(* A [match] body uses legacy modes instead of inheriting an enclosing local
   return expectation. *)
let match_body_enclosing_local_return () =
  exclave_
  match
    let (r @ local) = ref 0 in
    r
  with
  | r -> r
  | effect Need_ref, k -> continue k (ref 0)
[%%expect {|
Line 5, characters 4-5:
5 |     r
        ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [match] value clause uses legacy modes instead of inheriting an enclosing
   local return expectation. *)
let match_value_clause_enclosing_local_return () =
  exclave_
  match perform Need_ref with
  | r ->
      let (r @ local) = ref !r in
      r
  | effect Need_ref, k -> continue k (ref 1)
[%%expect {|
Line 6, characters 6-7:
6 |       r
          ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [match] effect clause uses legacy modes instead of inheriting an enclosing
   local return expectation. *)
let match_effect_clause_enclosing_local_return () =
  exclave_
  match perform Need_unit with
  | () -> ref 2
  | effect Need_unit, k ->
      let (r @ local) = ref 2 in
      r
[%%expect {|
Line 7, characters 6-7:
7 |       r
          ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [try] body uses legacy modes instead of inheriting an enclosing local
   return expectation. *)
let try_body_enclosing_local_return () =
  exclave_
  try
    let (r @ local) = ref 3 in
    r
  with
  | effect Need_ref, k -> continue k (ref 4)
[%%expect {|
Line 5, characters 4-5:
5 |     r
        ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [try] exception clause uses legacy modes instead of inheriting an
   enclosing local return expectation. *)
let try_exception_clause_enclosing_local_return () =
  exclave_
  try raise Exit with
  | Exit ->
      let (r @ local) = ref 5 in
      r
  | effect Need_ref, k -> continue k (ref 6)
[%%expect {|
Line 6, characters 6-7:
6 |       r
          ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [try] effect clause uses legacy modes instead of inheriting an enclosing
   local return expectation. *)
let try_effect_clause_enclosing_local_return () =
  exclave_
  try
    perform Need_unit;
    ref 7
  with
  | effect Need_unit, k ->
      let (r @ local) = ref 7 in
      r
[%%expect {|
Line 9, characters 6-7:
9 |       r
          ^
Error: This value is "local" but is expected to be "global".
|}]

(* A [match] result must preserve an enclosing portable expectation while using
   legacy modes for the generated handler body. *)
let () =
  use_portable_function
    (match () with
     | () ->
         let (f @ nonportable) () = () in
         f
     | effect Need_unit, _ -> fun () -> ())
[%%expect {|
Line 6, characters 9-10:
6 |          f
             ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* A [try] result must preserve an enclosing portable expectation while using
   legacy modes for the generated handler body. *)
let () =
  use_portable_function
    (try
       let (f @ nonportable) () = () in
       f
     with
     | effect Need_unit, _ -> fun () -> ())
[%%expect {|
Line 5, characters 7-8:
5 |        f
           ^
Error: This value is "nonportable" but is expected to be "portable".
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

(* A [match] with effect cases forces the enclosing function to be
   nonportable, even when the result mode-crosses. *)
let (match_in_portable_function @ portable) () =
  match () with
  | () -> ()
  | effect Need_unit, _ -> ()
[%%expect {|
val match_in_portable_function : unit -> unit = <fun>
|}]

(* A [try] with effect cases forces the enclosing function to be
   nonportable. *)
let (try_in_portable_function @ portable) () =
  try () with
  | effect Need_unit, _ -> ()
[%%expect {|
val try_in_portable_function : unit -> unit = <fun>
|}]

(* The constraint applies across all enclosing closures: a handler nested in
   an inner closure makes an outer portable function nonportable. *)
let (nested_match_in_portable_function @ portable) () =
  let g () =
    match () with
    | () -> ()
    | effect Need_unit, _ -> ()
  in
  g ()
[%%expect {|
val nested_match_in_portable_function : unit -> unit = <fun>
|}]

(* The constraint also applies when the function's portability is an
   expectation from the context. *)
let () =
  use_portable_function
    (fun () ->
       match () with
       | () -> ()
       | effect Need_unit, _ -> ())
[%%expect {|
|}]

(* A [match] with effect cases is allowed in an ordinary, nonportable
   function. *)
let match_in_nonportable_function () =
  match () with
  | () -> ()
  | effect Need_unit, _ -> ()
[%%expect {|
val match_in_nonportable_function : unit -> unit = <fun>
|}]

(* A [match] with effect cases forces the enclosing function to be
   stateful. *)
let (match_in_stateless_function @ stateless) () =
  match () with
  | () -> ()
  | effect Need_unit, _ -> ()
[%%expect {|
val match_in_stateless_function : unit -> unit = <fun>
|}]

(* A [try] with effect cases forces the enclosing function to be stateful. *)
let (try_in_stateless_function @ stateless) () =
  try () with
  | effect Need_unit, _ -> ()
[%%expect {|
val try_in_stateless_function : unit -> unit = <fun>
|}]

(* An effect continuation is stateful, not stateless. *)
let () =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k -> use_stateless k
[%%expect {|
Line 4, characters 41-42:
4 |   | effect Need_unit, k -> use_stateless k
                                             ^
Error: This value is "stateful" but is expected to be "stateless".
|}]

(* An effect payload passed to [perform] must be uncontended. *)
let contended_payload_to_perform (r @ contended) = perform (Payload r)
[%%expect {|
Line 1, characters 68-69:
1 | let contended_payload_to_perform (r @ contended) = perform (Payload r)
                                                                        ^
Error: This value is "contended"
       but is expected to be "uncontended"
         because it is contained (via constructor "Payload") in the value at line 1, characters 59-70
         which is expected to be "uncontended".
|}]

(* A payload captured by an effect pattern is uncontended and read-write. *)
let write_captured_payload () =
  match perform (Payload (ref 0)) with
  | () -> ()
  | effect (Payload r), k ->
      r := 1;
      continue k ()
[%%expect {|
val write_captured_payload : unit -> unit = <fun>
|}]

(* A contended value may cross the generated handler boundary. *)
let contended_crosses_handler_boundary (r @ contended) =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      use_contended r;
      continue k ()
[%%expect {|
val contended_crosses_handler_boundary : 'a @ contended -> unit = <fun>
|}]

(* The continuation may be used as uncontended. *)
let k_is_uncontended () =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      use_uncontended k;
      continue k ()
[%%expect {|
val k_is_uncontended : unit -> unit = <fun>
|}]

(* An effect payload passed to [perform] must be read-write. *)
let immutable_payload_to_perform (r @ immutable) = perform (Payload r)
[%%expect {|
Line 1, characters 68-69:
1 | let immutable_payload_to_perform (r @ immutable) = perform (Payload r)
                                                                        ^
Error: This value is "immutable"
       but is expected to be "read_write"
         because it is contained (via constructor "Payload") in the value at line 1, characters 59-70
         which is expected to be "read_write".
|}]

(* An immutable value may cross the generated handler boundary. *)
let immutable_crosses_handler_boundary (r @ immutable) =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      use_immutable r;
      continue k ()
[%%expect {|
val immutable_crosses_handler_boundary : 'a @ immutable -> unit = <fun>
|}]

(* The continuation may be used as read-write. *)
let k_is_read_write () =
  match perform Need_unit with
  | () -> ()
  | effect Need_unit, k ->
      use_read_write k;
      continue k ()
[%%expect {|
val k_is_read_write : unit -> unit = <fun>
|}]
