(* TEST
 flags = "-w -8";
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression tests: an argument that follows a non-exhaustive GADT
   constructor match used to get its value kind from the matched branch's
   equations, miscompiling callers that pick a missing constructor. *)

(* The matched branch says [f]'s second argument is a tuple, but a caller can
   reach it with an immediate. This used to segfault. *)
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
   omit the argument, so [y]'s slot must not be marked immediate. This used to
   segfault. *)
type 'a rep4 = Int4 : int rep4

let f4 : type a. ?x:(a rep4) -> a -> unit -> a =
  fun ?x:(Int4 = assert false) y () -> y

let p4 = (Sys.opaque_identity f4) (Sys.opaque_identity (String.make 4 'h'))

let () = Gc.compact ()

let () =
  match p4 () with
  | _ -> assert false
  | exception Assert_failure _ -> ()

(* Regression test: partial match unpacking, unarized. *)
type 'a rep5 = Block5 : (int * int) rep5 | Int5 : int rep5

let[@inline never] g5 b =
  let[@local] f : type a. a rep5 * a -> int = fun (Block5, (a, _b)) -> a in
  if b then f (Block5, (1, 2)) else f (Int5, Sys.opaque_identity 0)

let () =
  assert (g5 true = 1);
  match g5 false with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* Regression test: total match unpacking a tuple, unarized *)
type 'a rep6 = Block6 : (int * int) rep6 | Int6 : int rep6

let[@inline never] g6 b =
  let[@local] f : type a. a rep6 * a -> int = function
    | Block6, (a, _b) -> a
    | Int6, x -> x
  in
  if b then f (Block6, (1, 2)) else f (Int6, Sys.opaque_identity 0)

let () =
  assert (g6 true = 1);
  assert (g6 false = 0)

(* Regression test: partial match unpacking a tuple,
   not unarized due to the extra argument. *)
type 'a rep7 = Block7 : (int * int) rep7 | Int7 : int rep7

let[@inline never] g7 b =
  let[@local] f : type a. a rep7 * a -> unit -> int =
    fun (Block7, (a, _b)) () -> a
  in
  if b then f (Block7, (1, 2)) () else f (Int7, Sys.opaque_identity 0) ()

let () =
  assert (g7 true = 1);
  match g7 false with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* Regression test: partial match unpacking a tuple,
   not unarized due to [as _p]. *)
type 'a rep8 = Block8 : (int * int) rep8 | Int8 : int rep8

let[@inline never] g8 b =
  let[@local] f : type a. a rep8 * a -> int =
    fun ((Block8, (a, _b)) as _p) -> a
  in
  if b then f (Block8, (1, 2)) else f (Int8, Sys.opaque_identity 0)

let () =
  assert (g8 true = 1);
  match g8 false with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* Regression test: total match unpacking a tuple,
   not unarized due to the extra argument. *)
type 'a rep9 = Block9 : (int * int) rep9 | Int9 : int rep9

let[@inline never] g9 b =
  let[@local] f : type a. unit -> a rep9 * a -> int = fun () -> function
    | Block9, (a, _b) -> a
    | Int9, x -> x
  in
  if b then f () (Block9, (1, 2)) else f () (Int9, Sys.opaque_identity 0)

let () =
  assert (g9 true = 1);
  assert (g9 false = 0)

(* Regression test: total match unpacking a tuple,
   not unarized due to [as _t]. *)
type 'a rep10 = Int10 : int rep10 | Float10 : float rep10

let[@inline never] g10 b =
  let[@local] f : type a. a rep10 * a -> float = function
    | (Float10, x) as _t -> x +. 1.0
    | Int10, z -> float_of_int z
  in
  if b then f (Float10, Sys.opaque_identity 3.5)
  else f (Int10, Sys.opaque_identity 7)

let () =
  assert (g10 true = 4.5);
  assert (g10 false = 7.0)

(* Regression test: partial match unpacking a tuple, compiled with the
   tupled calling convention. *)
type 'a rep11 = Block11 : (int * int) rep11 | Int11 : int rep11

let[@inline never] f11 : type a. a rep11 * a -> int =
  fun (Block11, (a, _b)) -> a

let () =
  assert (f11 (Block11, (1, 2)) = 1);
  match f11 (Int11, Sys.opaque_identity 0) with
  | _ -> assert false
  | exception Match_failure _ -> ()

(* Regression test: total match unpacking a tuple whose branch kinds
   disagree, compiled with the tupled calling convention. *)
type 'a rep12 = Block12 : (int * int) rep12 | Int12 : int rep12

let[@inline never] f12 : type a. a rep12 * a -> int = function
  | Block12, (a, _b) -> a
  | Int12, x -> x

let () =
  assert (f12 (Block12, (1, 2)) = 1);
  assert (f12 (Int12, Sys.opaque_identity 0) = 0)
