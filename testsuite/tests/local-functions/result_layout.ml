(* TEST
 {
   native;
 } {
   unset OCAMLPARAM;
   flags = "-O3 -zero-alloc-check opt";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

(* When a local function becomes a static catch, the catch can return more than
   the function does. *)

let[@inline never] is_null (x : int or_null) =
  match x with Null -> true | This _ -> false

(* A call under [This], which has no runtime representation *)
let[@inline never] under_this (n : int) (m : int or_null) =
  let h (_ : int) = n in
  is_null (match m with Null -> Null | This x -> This (h x))

let () = assert (under_this 1 (Sys.opaque_identity Null))

(* The same, where the call is in the body of another local function *)
let[@inline never] under_this_in_local_function (n : int) (m : int or_null) =
  let h (_ : int) = n in
  is_null
    (let h' x = match x with Null -> Null | This x -> This (h x) in
     h' m)

let () = assert (under_this_in_local_function 1 (Sys.opaque_identity Null))

(* A call under a coercion *)
type t = [ `A | `B of string ]

let[@inline always] to_string (x : t) = match x with `A -> "A" | `B s -> s

let[@inline never] under_coercion (b : bool) (y : t) =
  let h () : [ `A ] = `A in
  to_string (if b then (h () :> t) else y)

let () =
  let s = under_coercion (Sys.opaque_identity false) (`B "B") in
  assert (String.equal s "B")

(* The program from #8382 *)
type r = { d : int64 }

let[@inline never] g (_ : int or_null) = 5

let[@inline never] f (n : int) =
  let h = (fun (_ : int) -> n) in
  g (match
       (match (Sys.opaque_identity Null : int or_null) with
        | Null -> Null
        | This _ -> This { d = Int64.of_int n })
     with
     | Null -> Null
     | This x ->
       This (h (if Sys.opaque_identity (Int64.to_int x.d <= 0) then 0 else 1)))

let () = assert (f 1 = 5)

external is_immediate : 'a -> bool = "%obj_is_int"

(* [h] returns an opaque value of polymorphic type, but the catch for [&&]
   returns a boolean. With the catch's immediate layout, -O3 can eliminate
   the allocating [Some] branch. The two calls keep the catch from inlining. *)
let[@inline never][@zero_alloc opt] under_sequand b c =
  let h x = Sys.opaque_identity x in
  if is_immediate (b && (if c then h true else h false)) then None else Some c

let () =
  assert (under_sequand (Sys.opaque_identity false) true = None);
  assert (under_sequand (Sys.opaque_identity true) false = None)
