(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression test for a use-after-free in layout-[any] returns: when a function
   with a layout-[any] (hence local-region-wrapped) body returns concretely via
   [exclave_], the exclave allocation must land in the *parent* region so it
   survives the teardown of the function's own region.  Stripping the
   [exclave_] and allocating the result in the function's own region would
   leave a dangling pointer once that region is closed.  The alternative arm
   is a same-sort (value) concrete result, so the function has a single
   direct-return
   layout; this test returns an allocating [exclave_] branch and reads the value
   back after stressing the caller's region. *)

type (_ : any) w =
  | A : int ref w
  | B : string w

external opaque : 'a -> 'a = "%opaque"
external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let[@inline never] f : type (a : any). int -> a w -> a @ local =
  fun n w ->
    let local_ scratch = ref 0 in
    let local_ _scratch = opaque_local scratch in
    match w with
    | A -> exclave_ (ref (opaque n))
    | B -> "x"

let[@inline never] caller n =
  let local_ r = f n A in
  (* Allocate heavily in the caller's region; if [r] dangled into [f]'s freed
     region these allocations would clobber it. *)
  let local_ a = ref (opaque 11) in
  let local_ b = ref (opaque 22) in
  let local_ c = ref (opaque 33) in
  let local_ d = ref (opaque 44) in
  let v = !r in
  let noise = !a + !b + !c + !d in
  v + (noise land 0)

let () =
  for i = 0 to 1000 do
    assert (Int.equal i (caller i))
  done
