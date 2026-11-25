(* TEST
 flags += "-keywords 5.3";
 stack-allocation;
 native;
*)

(* TMC calls under exception and effect handlers, in functions with a
   local-allocation region. *)

let[@inline never] use_local (local_ _x : int * int) = ()

let[@inline never] fail () : unit = raise_notrace Exit

let print_list name l =
  Printf.printf "%-32s %s\n" name (String.concat " " (List.map string_of_int l))

(* The body of a [try] is not in tail position: the exception handler is
   still installed when the call returns. The call must neither close the
   region nor be annotated as a tailcall. The handler at [n = 2] reads [p]
   after the deeper calls have allocated in their own regions. *)
let[@tail_mod_cons] rec try_body_local n =
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  try
    if n = 0 then fail ();
    n :: try_body_local (n - 1)
  with Exit when n = 2 -> [ fst p * 100 ]

(* As above, but without [local_]: [p] does not escape, so it is inferred to
   be stack-allocated and the function still has a region. *)
let[@tail_mod_cons] rec try_body_inferred n =
  let p = (n, n) in
  try
    if n = 0 then fail ();
    n :: try_body_inferred (n - 1)
  with Exit -> [ fst p ]

(* No region. The call in the [try] body must not be annotated as a
   tailcall. *)
let[@tail_mod_cons] rec try_body_no_region n =
  try
    if n = 0 then fail ();
    n :: try_body_no_region (n - 1)
  with Exit -> []

(* Call in the [try] body: not a TMC call, so the function is never applied in
   TMC position. *)
let[@tail_mod_cons] rec map_try_body t ~f =
  let local_ p = (Sys.opaque_identity 0, 0) in
  use_local p;
  match t with
  | [] -> []
  | hd :: tl -> (try f hd :: map_try_body tl ~f with Exit -> [ fst p ])

(* TMC call in the handler: the handler is in tail position, so the region
   can be closed. Constant stack. *)
let[@tail_mod_cons] rec map_handler t ~f =
  let local_ p = (Sys.opaque_identity 0, 0) in
  use_local p;
  match t with
  | [] -> []
  | hd :: tl -> (try fail (); [] with Exit -> f hd :: map_handler tl ~f)

(* The call is in tail position of the region of the inlined helper [go],
   but [go] is called in the body of a [try], so it is not a TMC call and the
   function is never applied in TMC position. *)
let[@tail_mod_cons] rec try_helper_region n =
  let go n =
    let local_ q = (Sys.opaque_identity n, n) in
    use_local q;
    if n = 0 then fail ();
    n :: try_helper_region (n - 1)
  in
  if n < 0 then [] else try go n with Exit -> []

type _ Effect.t += Ask : int Effect.t

(* The cases of an effect match are compiled to separate closures, so the
   recursive call in the value case is not in TMC position. *)
let[@tail_mod_cons] rec effect_value_case n =
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  if n = 0 then []
  else
    match Effect.perform Ask with
    | v -> v :: effect_value_case (n - 1)
    | effect Ask, k -> Effect.Deep.continue k (n * 10)

(* [perform] is evaluated before the region is closed at the TMC call. *)
let[@tail_mod_cons] rec perform_in_tmc n =
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  if n = 0 then [] else (Effect.perform Ask + fst p) :: perform_in_tmc (n - 1)

(* [discontinue] raises at the [perform] inside the [try] body; the handler
   then reads [p]. *)
let[@tail_mod_cons] rec perform_in_try n =
  let local_ p = (Sys.opaque_identity n, n) in
  use_local p;
  try if n = 0 then [] else Effect.perform Ask :: perform_in_try (n - 1)
  with Exit -> [ - fst p ]

let handle_ask ?discontinue_at f =
  let count = ref 0 in
  match f () with
  | v -> v
  | effect Ask, k ->
    incr count;
    if Some !count = discontinue_at then Effect.Deep.discontinue k Exit
    else Effect.Deep.continue k (!count * 100)

let depth () = Printexc.raw_backtrace_length (Printexc.get_callstack 10_000)

let rec build acc n = if n = 0 then acc else build (n :: acc) (n - 1)

(* Only the shape is checked, never the absolute depths. The first element is
   mapped by the direct function, so compare from the second onwards. *)
let report name map =
  let samples = ref [] in
  let sample i = samples := depth () :: !samples; i in
  ignore (map (build [] 50) ~f:sample);
  match List.rev !samples with
  | [] | [_] -> Printf.printf "%-32s too few samples\n" name
  | _first :: (base :: _ as rest) ->
    let constant = List.for_all (fun d -> d = base) rest in
    Printf.printf "%-32s constant stack: %b\n" name constant

let () =
  print_list "try_body_local" (try_body_local 4);
  print_list "try_body_inferred" (try_body_inferred 3);
  print_list "try_body_no_region" (try_body_no_region 3);
  report "map_try_body" map_try_body;
  report "map_handler" map_handler;
  print_list "try_helper_region" (try_helper_region 3);
  print_list "effect_value_case" (effect_value_case 3);
  print_list "perform_in_tmc" (handle_ask (fun () -> perform_in_tmc 3));
  print_list "perform_in_try"
    (handle_ask ~discontinue_at:3 (fun () -> perform_in_try 5))
