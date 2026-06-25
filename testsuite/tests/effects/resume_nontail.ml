(* TEST
   { bytecode; }
*)

(* Regression test for a bytecode bug where [Instruct(RESUME)] popped one
   stack word too many. All [resume] sites in the stdlib are in tail
   position, compiling to RESUMETERM, so to exercise [Instruct(RESUME)] we
   use the [%resume] primitive directly in NON-tail position inside an
   effect handler, keeping several locals live across the call. *)

open Effect
open Effect.Deep

(* [%resume cont f v] resumes [cont] running [f v].  The continuation argument
   has the same representation as [Effect.Deep.continuation]. *)
external resume_nontail :
  ('a, 'b) continuation -> ('c -> 'a) -> 'c -> 'b = "%resume"

type _ t += E : int t

let () =
  let r =
    try_with
      (fun () -> 100 + perform E)
      ()
      { effc =
          (fun (type a) (e : a t) ->
            match e with
            | E ->
              Some
                (fun (k : (a, _) continuation) ->
                  (* Keep several locals live across the non-tail resume.  If
                     RESUME pops one word too many, [sp] is left off by one
                     when the resumed fiber returns, so these locals read back
                     wrong (or the program crashes). *)
                  let a = 1 in
                  let b = 2 in
                  let c = 3 in
                  let d = 4 in
                  let e = 5 in
                  let resumed = resume_nontail k (fun x -> x) 7 in
                  (* [resumed] should be 100 + 7 = 107. *)
                  resumed + a + b + c + d + e)
            | _ -> None)
      }
  in
  Printf.printf "result = %d\n%!" r;
  assert (r = 107 + 1 + 2 + 3 + 4 + 5);
  print_string "ok\n"
