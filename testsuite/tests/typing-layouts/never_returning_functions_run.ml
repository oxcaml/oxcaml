(* TEST
 reference =
   "${test_source_directory}/never_returning_functions_run.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   flags =
     "-O3 -flambda2-result-types-all-functions -no-flambda2-match-in-match";
   native;
 }{
   flags = "-O3 -flambda2-reaper";
   native;
 }{
   flags = "-Oclassic -flambda2-reaper -X reaper-oclassic=1";
   native;
 }{
   bytecode;
 }
*)

(* Calls to functions that never return, where results of various layouts are
   expected. See also [never_returning_functions.ml]. *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Float32_u = Stdlib_stable.Float32_u

external raise_any : ('a : any). exn -> 'a = "%raise"

exception Fail of string

let[@inline never] fail_never s = raise_any (Fail s)
let[@inline] fail_inline s = raise_any (Fail s)
let fail_default s = raise_any (Fail s)
let[@opaque] fail_opaque s = raise_any (Fail s)
let[@inline never] fail_assert () = assert false

let[@inline never] fail_match = function
  | Some s -> raise_any (Fail s)
  | None -> assert false

let[@inline never] fail_try s =
  try raise_any (Fail s) with Not_found -> raise_any (Fail "not found")

let[@inline never] fail_two_params (s : string) (n : int) =
  raise_any (Fail (s ^ string_of_int n))

let[@inline never] fail_local s =
  let local_ r = stack_ (ref s) in
  raise_any (Fail !r)

let[@inline never] fail_optional ?(prefix = "default ") s =
  raise_any (Fail (prefix ^ s))

let[@opaque] fail_optional_opaque ?(prefix = "default ") s =
  raise_any (Fail (prefix ^ s))

let obj =
  object
    method fail : ('a : any). string -> 'a = fun s -> raise_any (Fail s)
  end

let print_float name (x : float#) =
  Printf.printf "%s: %.1f\n" name (Float_u.to_float x)

let print_int64 name (x : int64_u) =
  Printf.printf "%s: %Ld\n" name (Int64_u.to_int64 x)

let print_product name (#(n, x) : #(int * float#)) =
  Printf.printf "%s: %d, %.1f\n" name n (Float_u.to_float x)

let print_string name (s : string) = Printf.printf "%s: %s\n" name s

(* Runs [f] where it returns and where it calls a function that never
   returns. *)
let check name (f : string -> bool -> unit) =
  f name false;
  match f name true with
  | () -> Printf.printf "%s: returned\n" name
  | exception Fail s -> Printf.printf "%s: Fail %s\n" name s
  | exception Assert_failure _ -> Printf.printf "%s: Assert_failure\n" name

(* Direct calls *)

let () =
  check "never inlined, float#" (fun name fail ->
    print_float name (if fail then fail_never "a" else #1.5));
  check "never inlined, int64_u" (fun name fail ->
    print_int64 name (if fail then fail_never "b" else #2L));
  check "never inlined, product" (fun name fail ->
    print_product name (if fail then fail_never "c" else #(3, #3.5)));
  check "never inlined, value" (fun name fail ->
    print_string name (if fail then fail_never "d" else "four"));
  check "inlined, float#" (fun name fail ->
    print_float name (if fail then fail_inline "e" else #5.5));
  check "inlined, product" (fun name fail ->
    print_product name (if fail then fail_inline "f" else #(6, #6.5)));
  check "default, int64_u" (fun name fail ->
    print_int64 name (if fail then fail_default "g" else #7L));
  check "opaque, float#" (fun name fail ->
    print_float name (if fail then fail_opaque "h" else #8.5));
  check "assert false, product" (fun name fail ->
    print_product name (if fail then fail_assert () else #(9, #9.5)));
  check "match, int64_u" (fun name fail ->
    print_int64 name (if fail then fail_match (Some "i") else #10L));
  check "try, float#" (fun name fail ->
    print_float name (if fail then fail_try "j" else #11.5))

(* Calls in tail position *)

let[@inline never] tail_float fail : float# =
  if fail then fail_never "k" else #12.5

let[@inline never] tail_product fail : #(int * float#) =
  if fail then fail_default "l" else #(13, #13.5)

let () =
  check "tail call, float#" (fun name fail ->
    print_float name (tail_float fail));
  check "tail call, product" (fun name fail ->
    print_product name (tail_product fail))

(* Indirect calls *)

let[@inline never] call_float (f : string -> float#) fail =
  if fail then f "m" else #14.5

let[@inline never] call_product (f : string -> #(int * float#)) fail =
  if fail then f "n" else #(15, #15.5)

let () =
  check "indirect, float#" (fun name fail ->
    print_float name (call_float fail_never fail));
  check "indirect, product" (fun name fail ->
    print_product name (call_product (Sys.opaque_identity fail_default) fail));
  check "indirect, primitive" (fun name fail ->
    print_float name
      (call_float (fun s -> (Sys.opaque_identity raise_any) (Fail s)) fail));
  check "method, float#" (fun name fail ->
    print_float name (if fail then obj#fail "o" else #16.5))

(* Partial and over-applications *)

let () =
  check "partial application, float#" (fun name fail ->
    let partial = fail_two_params "p" in
    print_float name (if fail then partial 1 else #17.5));
  check "partial application, product" (fun name fail ->
    let partial = Sys.opaque_identity (fail_two_params "q") in
    print_product name (if fail then partial 2 else #(18, #18.5)));
  check "over-application, int64_u" (fun name fail ->
    let over : string -> int -> int -> int64_u = fail_never in
    print_int64 name (if fail then over "r" 1 2 else #19L))

(* A partial application called from a closure *)

let[@inline never] make_partial s =
  let partial = fail_two_params s in
  fun n -> (partial n : #(int * float#))

let () =
  check "partial application in a closure, product" (fun name fail ->
    let f = make_partial "ab" in
    print_product name (if fail then f 3 else #(28, #28.5)))

(* Optional arguments *)

let () =
  check "optional argument omitted, float#" (fun name fail ->
    print_float name (if fail then fail_optional "ac" else #29.5));
  check "optional argument given, product" (fun name fail ->
    print_product name
      (if fail then fail_optional ~prefix:"given " "ad" else #(30, #30.5)));
  check "optional argument, indirect, float#" (fun name fail ->
    print_float name
      (call_float (Sys.opaque_identity (fun s -> fail_optional s)) fail))

(* A result that is only representable inside the function, through a
   partial match on a GADT parameter *)

type ('a : any) witness =
  | Int : int witness
  | Float_u : float# witness

let[@opaque] [@warning "-8"] fail_refined : type (a : any).
  a witness -> string -> a =
  fun Int s -> raise_any (Fail s)

let () =
  check "refined result, float#" (fun name fail ->
    let x : float# =
      if fail
      then
        try fail_refined Float_u "ae"
        with Match_failure _ -> raise_any (Fail "match failure")
      else #31.5
    in
    print_float name x);
  check "refined result, value" (fun name fail ->
    Printf.printf "%s: %d\n" name (if fail then fail_refined Int "af" else 32))

(* An opaque function with an optional argument *)

let () =
  check "optional argument, opaque, float#" (fun name fail ->
    print_float name (if fail then fail_optional_opaque "ag" else #33.5));
  check "optional argument, opaque, not inlined, value" (fun name fail ->
    print_string name
      (if fail
       then (fail_optional_opaque [@inlined never]) "ah"
       else "thirty-four"))

(* Local functions *)

let () =
  check "several call sites, float#" (fun name fail ->
    let n = if fail then 1 else 0 in
    let local_fail n =
      if n = 1 then raise_any (Fail "s") else raise_any (Fail "not s")
    in
    let x : float# =
      match Sys.opaque_identity n with
      | 0 -> #20.5
      | 1 -> local_fail 1
      | 2 -> local_fail 2
      | _ -> #0.0
    in
    print_float name x);
  check "several layouts, product" (fun name fail ->
    let local_fail s = raise_any (Fail s) in
    let n : int64_u =
      if Sys.opaque_identity false then local_fail "t" else #21L
    in
    let x : float# = if fail then local_fail "u" else #21.5 in
    print_product name #(Int64_u.to_int n, x));
  check "immediately applied, float#" (fun name fail ->
    print_float name
      (if fail then (fun s -> raise_any (Fail s)) "v" else #22.5));
  check "immediately applied, product" (fun name fail ->
    let x : #(int * float#) =
      if fail
      then
        (function
          | Some s -> raise_any (Fail s)
          | None -> assert false)
          (Sys.opaque_identity (Some "w"))
      else #(23, #23.5)
    in
    print_product name x)

(* Exception handlers and regions *)

let () =
  check "inside try, float#" (fun name fail ->
    let x : float# =
      try if fail then fail_never "x" else #24.5 with Not_found -> #0.0
    in
    print_float name x);
  check "caught and raised again, int64_u" (fun name fail ->
    let x : int64_u =
      try if fail then fail_never "y" else #25L
      with Fail s -> fail_default (s ^ "y")
    in
    print_int64 name x);
  check "local allocation in callee, float#" (fun name fail ->
    print_float name (if fail then fail_local "z" else #26.5));
  check "local allocation in caller, product" (fun name fail ->
    let local_ r = stack_ (ref 27.5) in
    let x : #(int * float#) =
      if fail then fail_never "aa" else #(27, Float_u.of_float !r)
    in
    print_product name x)

(* Raise primitives declared with a result of a concrete layout *)

external raise_float : exn -> float# = "%raise"
external raise_int32 : exn -> int32_u = "%raise"
external raise_int64 : exn -> int64_u = "%raise"
external raise_nativeint : exn -> nativeint_u = "%raise"
external raise_float32 : exn -> float32_u = "%raise"
external raise_product : exn -> #(int * float#) = "%raise"
external raise_void : exn -> unit# = "%raise"
external reraise_product : exn -> #(float# * int64_u) = "%reraise"
external raise_notrace_float : exn -> float# = "%raise_notrace"
external raise_unboxed_float : exn -> (float [@unboxed]) = "%raise" "%raise"

let fail_concrete s = raise_float (Fail s)

let[@inline never] tail_concrete fail : #(int * float#) =
  if fail then raise_product (Fail "at") else #(41, #41.5)

let () =
  check "concrete primitive, float#" (fun name fail ->
    print_float name (if fail then raise_float (Fail "ai") else #35.5));
  check "concrete primitive, int32_u" (fun name fail ->
    let x : int32_u = if fail then raise_int32 (Fail "aj") else #36l in
    print_int64 name (Int64_u.of_int32_u x));
  check "concrete primitive, int64_u" (fun name fail ->
    print_int64 name (if fail then raise_int64 (Fail "ak") else #37L));
  check "concrete primitive, nativeint_u" (fun name fail ->
    let x : nativeint_u = if fail then raise_nativeint (Fail "al") else #38n in
    print_int64 name (Int64_u.of_nativeint_u x));
  check "concrete primitive, float32_u" (fun name fail ->
    let x : float32_u =
      if fail then raise_float32 (Fail "am") else Float32_u.of_float #39.5
    in
    print_float name (Float32_u.to_float x));
  check "concrete primitive, product" (fun name fail ->
    print_product name
      (if fail then raise_product (Fail "an") else #(40, #40.5)));
  check "concrete primitive, void" (fun name fail ->
    let #() = if fail then raise_void (Fail "ao") else #() in
    print_string name "void");
  check "concrete primitive, tail call" (fun name fail ->
    print_product name (tail_concrete fail));
  check "concrete reraise, product" (fun name fail ->
    let #(x, n) =
      if fail then reraise_product (Fail "ap") else #(#42.5, #42L)
    in
    print_product name #(Int64_u.to_int n, x));
  check "concrete raise_notrace, float#" (fun name fail ->
    print_float name
      (if fail then raise_notrace_float (Fail "aq") else #43.5));
  check "concrete primitive, [@unboxed]" (fun name fail ->
    print_float name
      (Float_u.of_float
         (if fail then raise_unboxed_float (Fail "ar") else 44.5)));
  check "concrete primitive in a function, float#" (fun name fail ->
    print_float name (if fail then fail_concrete "as" else #45.5));
  check "concrete primitive, indirect, float#" (fun name fail ->
    print_float name
      (call_float (Sys.opaque_identity (fun s -> raise_float (Fail s))) fail))
