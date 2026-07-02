(* TEST
 flags = "-w -8";
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression tests: an argument that follows a non-exhaustive GADT
   constructor match gets its value kind from the matched branch's equations,
   miscompiling callers that pick a missing constructor. The native version of
   this test currently segfaults. *)

(* The matched branch says [f]'s second argument is a tuple, but a caller can
   reach it with an immediate. This segfaults. *)
type 'a rep = Block : (int * int) rep | Int : int rep

let[@inline never] g b =
  let[@local] f : type a. a rep -> a -> int = fun Block (a, _b) -> a in
  if b then f Block (1, 2) else f Int (Sys.opaque_identity 0)

let () =
  assert (g true = 1);
  match g false with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* The matched branch says [f2]'s second argument is immediate, but a partial
   application can store a string there; it must be scanned by the GC while it
   lives in the wrapper's closure. *)
type 'a rep2 = Int2 : int rep2 | String2 : string rep2

let f2 : type a. a rep2 -> a -> unit -> a = fun Int2 x () -> x
let[@inline always] g2 x = x

let p =
  (Sys.opaque_identity (g2 f2)) String2
    (Sys.opaque_identity (String.make 4 'h'))

let () = Gc.compact ()

let () =
  match p () with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* The matched branch says [f]'s second argument is a tagged immediate, but a
   caller can reach it with a boxed float. *)
type 'a rep3 = Int3 : int rep3 | Float3 : float rep3

let g3 b =
  let[@local] f : type a. a rep3 -> a -> int = fun Int3 z -> z in
  if b then f Int3 0 else f Float3 (Sys.opaque_identity 0.)

let () =
  assert (g3 true = 0);
  match g3 false with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* The pattern on the optional argument covers its payload, but a caller can
   omit the argument, so [y]'s slot must not be marked immediate. This
   segfaults. *)
type 'a rep4 = Int4 : int rep4

let f4 : type a. ?x:(a rep4) -> a -> unit -> a =
  fun ?x:(Int4 = assert false) y () -> y

let p4 = (Sys.opaque_identity f4) (Sys.opaque_identity (String.make 4 'h'))

let () = Gc.compact ()

let () =
  match p4 () with
  | _ -> assert false
  | exception Assert_failure _ -> ()
