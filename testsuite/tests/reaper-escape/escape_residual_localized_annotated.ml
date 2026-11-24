(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   script = "sh ${test_source_directory}/compare-with-annotated.sh escape_residual_localized";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-reaper;
   script;
   run;
   check-program-output;
 *)

(* [escape_residual_localized] with the annotations the escape analysis infers (see
   [compare-with-annotated.sh]). The type checker stack-allocates the residual
   allocations, as [-X reaper-escape-localize-residual=1] does. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

type triple =
  { a : int;
    b : int;
    c : int
  }

let[@inline never] sum3 (local_ t) = t.a + t.b + t.c

let[@inline never] sum_pair (local_ (p : int * int)) = fst p + snd p

let[@inline never] first (local_ (p : (int * int) * int)) = fst (fst p) + snd p

let base = ref 0

let[@inline never] used () = local_stack_offset () - !base

module M : sig
  val run : int -> unit
end = struct
  (* [t] is used after the result has been allocated, so the function has no
     region of its own: [t] is a residual allocation. *)
  let[@inline never] plain x y =
    exclave_
      (let r = x, y in
       let s = sum_pair r in
       let t = { a = s; b = y; c = x } in
       let u = sum3 t in
       if u > 0 then r else y, x)

  (* Here [t] contains the result, which is local, so it stays local. *)
  let[@inline never] containing x y =
    exclave_
      (let r = x, y in
       let t = r, x in
       let s = first t in
       if s > 0 then r else y, x)

  let[@inline never] run n =
    base := local_stack_offset ();
    let p = plain n 1 in
    let u1 = used () in
    let q = containing n 2 in
    let u2 = used () in
    (Printf.printf [@inlined never]) "%d %d; used %d %d\n" (sum_pair p)
      (sum_pair q) u1 (u2 - u1)
end

let () = M.run 10
