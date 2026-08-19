(* TEST
 flags = "-extension layouts_beta -Oclassic";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }
*)

(* Layout-any return values compiled in classic mode: the
   [Layout_any_return_transform] pass runs directly on the
   closure-conversion output, so forwarders, return-continuation aliases,
   never-returning callees and local regions must all compile and run without
   the Simplify pass having normalized them first. *)

module F = Stdlib_upstream_compatible.Float_u

let[@inline never] return_int () = 11

let[@inline never] return_float_u () = #1.25

let[@inline never] raise_any : type (a : any). unit -> a =
  fun () -> assert false

let[@inline never] compose_unit : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let[@inline never] forward_if
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> a =
  fun b f g -> if b then f () else g ()

let[@inline never] forward_match
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> a =
  fun b f g ->
    match Sys.opaque_identity b with true -> f () | false -> g ()

let[@inline never] forward_never : type (a : any). unit -> a =
  fun () -> raise_any ()

let[@inline never] forward_with_region : type (a : any). (unit -> a) -> a =
  fun f ->
    let local_ _x = ref 0 in
    f ()

(* No [@inline never]: classic-mode inlining must decline to inline these
   unknown-result bodies at concrete callsites. *)
let compose_inlinable : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let[@inline always] compose_always : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let rec loop : type (a : any). unit -> a = fun () -> loop ()

let[@inline never] five_or_loop () : int =
  if Sys.opaque_identity false then loop () else 5

type ('a : any) witness =
  | Int : int witness
  | Float : float# witness

let[@inline never] of_witness : type (a : any). a witness -> a = function
  | Int -> 42
  | Float -> assert false

(* Partial application of a never-returning (Bottom result arity) function,
   called under a try-with: the stub inlined by classic mode leaves a
   variable-to-variable alias binding whose enclosing lets were rebuilt by the
   transform pass without occurrence information. *)
let[@inline never] never_partial : int -> string -> 'a = fun _ _ -> assert false

let[@inline never] partial_under_try () =
  let p = never_partial (Sys.opaque_identity 5) in
  try
    let _ : int = p "x" in
    0
  with Assert_failure _ -> 17

let expect_raises (f : unit -> int) =
  try
    let _ : int = f () in
    assert false
  with Assert_failure _ -> ()

let () =
  assert (Int.equal 11 (compose_unit return_int ()));
  assert (Float.equal 1.25 (F.to_float (compose_unit return_float_u ())));
  assert (Int.equal 11 (forward_if true return_int (fun () -> 0)));
  assert (Float.equal 1.25
            (F.to_float (forward_match false (fun () -> #0.0) return_float_u)));
  assert (Int.equal 11 (forward_with_region return_int));
  assert (Float.equal 1.25 (F.to_float (forward_with_region return_float_u)));
  assert (Int.equal 11 (compose_inlinable return_int ()));
  assert (Float.equal 1.25 (F.to_float (compose_always return_float_u ())));
  assert (Int.equal 5 (five_or_loop ()));
  assert (Int.equal 42 (of_witness Int));
  assert (Int.equal 17 (partial_under_try ()));
  expect_raises (fun () -> raise_any ());
  expect_raises (fun () -> forward_never ());
  expect_raises (fun () -> compose_unit raise_any ())
