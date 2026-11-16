(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   native with dump-simplify, dump-reaper;
   check-fexpr-dump;
*)

(* The abstract type prevents simplify from unboxing [p] below.
   The [@loop never] prevents it from seeing [g] may never return normally.
   However, for the reaper, the result of [g] has no source, so [p] (and the
   result of [mk]) can be unboxed.
 *)
module M : sig
  type t
  val mk : int -> t
  val get : t -> int
  val g : int -> t
end = struct
  type t = int * int
  let[@inline never] mk x = (x, x + 1)
  let[@inline never] get (a, _) = a
  let[@loop never] rec g n = if n > 0 then g (n - 1) else raise Exit
end

let[@inline never] f b x =
  let p = if b then M.mk x else M.g x in
  M.get p

let[@inline never] test b x =
  match f b x with
  | v -> Printf.printf "f %b %d = %d\n" b x v
  | exception Exit -> Printf.printf "f %b %d raised Exit\n" b x

let () =
  test (Sys.opaque_identity true) 3;
  test (Sys.opaque_identity false) 5;
  test (Sys.opaque_identity true) 7
