(* TEST
 include stdlib_upstream_compatible;
 modules = "first_class_modality_lib.mli first_class_modality_lib.ml";
 flambda2;
 flags = "-extension mode_alpha -w +51 -warn-error +51";
 { native; }
 { flags += " -Oclassic"; native; }
 { bytecode; }
*)

open Stdlib_upstream_compatible
open First_class_modality_lib

module Copy : module type of First_class_modality_lib =
  First_class_modality_lib

let[@inline never] pattern_identity
    (((_, _) as whole) : ((int * int) @@ global)) = whole

let[@inline never] wrapped_float (x : float#) : (float# @@ portable) = x
let[@inline never] bare_float (x : (float# @@ portable)) : float# = x
let[@inline never] wrapped_product (x : #(float# * int))
    : (#(float# * int) @@ portable) = x
let[@inline never] bare_product (x : (#(float# * int) @@ portable))
    : #(float# * int) = x

let[@inline never] coerce_portable_list (type a)
    (xs : (a @@ portable) list @ portable) : a list @ portable =
  (xs :> a list)

let events = ref []
let note event x = events := event :: !events; x
let add x y = x + y
let wrapped_add = (add : ((int -> int -> int) @@ global))

let baseline_order () =
  events := [];
  let result = (note 0 add) (note 1 10) (note 2 20) in
  result, !events

let wrapped_order () =
  events := [];
  let result = (note 0 wrapped_add) (note 1 10) (note 2 20) in
  result, !events

let[@inline never] intermediate x : ((int -> int) @@ global) =
  note 3 (add x)
let[@inline never] bare_intermediate x = note 3 (add x)

let intermediate_order () =
  events := [];
  let result = (intermediate (note 1 10)) (note 2 20) in
  result, !events

let baseline_intermediate_order () =
  events := [];
  let result = (bare_intermediate (note 1 10)) (note 2 20) in
  result, !events

type choice = [ `Left of int | `Right of int | `Empty ]
let match_choice (x : (choice @@ global)) =
  match x with `Left n | `Right n -> n | `Empty -> 0

type nested = Present of ((int option @@ global) @@ portable) | Missing
let match_nested (x : (nested @@ portable)) =
  match x with
  | Present (Some n) -> n
  | Present None | Missing -> 0

let rec tail n (x : (string @@ global)) : string =
  if n = 0 then x
  else (tail [@tailcall]) (n - 1) (wrap (unwrap x))

let allocated f =
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  f ();
  Gc.allocated_bytes () -. before

let () =
  let value = Sys.opaque_identity (String.make 20 'x') in
  let wrapped_values =
    Sys.opaque_identity [(value : (string @@ portable))] in
  let values = coerce_portable_list wrapped_values in
  assert (Obj.repr wrapped_values == Obj.repr values);
  assert (List.hd values == value);
  assert (unwrap (Copy.Nested.id (wrap value)) == value);
  assert (unwrap (id (wrap 42)) = 42);
  assert (unwrap (id (wrap true)));
  let pair = Sys.opaque_identity (1, 2) in
  let whole : int * int = pattern_identity pair in
  assert (whole == pair);
  assert (baseline_order () = wrapped_order ());
  assert (baseline_intermediate_order () = intermediate_order ());
  assert (match_choice (`Left 7) = 7);
  assert (match_choice (`Right 8) = 8);
  assert (match_choice `Empty = 0);
  assert (match_nested (Present (Some 9)) = 9);
  assert (match_nested (Present None) = 0);
  assert (match_nested Missing = 0);
  assert (Float_u.to_float (bare_float (wrapped_float #1.5)) = 1.5);
  let #(number, count) = bare_product (wrapped_product #(#2.5, 7)) in
  assert (Float_u.to_float number = 2.5 && count = 7);
  assert (tail 100_000 (wrap value) == value);
  let baseline = allocated (fun () ->
    for _ = 1 to 100_000 do ignore (Sys.opaque_identity value) done) in
  let converted = allocated (fun () ->
    for _ = 1 to 100_000 do
      ignore (unwrap (wrap (Sys.opaque_identity value)))
    done) in
  assert (converted <= baseline +. 128.);
  print_endline "structural modalities: ok"
