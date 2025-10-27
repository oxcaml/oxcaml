(* TEST
 expect;
*)

module M : sig
  val mt : int ref list mod contended
  (*= val x : int *)
end = struct
  let mt = []  (* doesn't explode *)
  (*= let mt = [ ref 42 ]  (* explodes *) *)
end

let test () =
  let x = M.mt in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
