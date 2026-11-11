(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3 -flambda2-join-algorithm=n-way";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 *)

(* This test used to fail with a fatal error:

        >> Fatal error: concat: mismatching domains on id 8.
outer: ((extra_params
         (Pmakeblock55_1/88N ∷ 𝕍=tagged_ℕ𝕚) (Pmakeblock55_0/87N ∷ 𝕍=tagged_ℕ𝕚))
        (extra_args
         {(5
           (Ok ((Already_in_scope Pmakeblock55_1/86N)
           (Already_in_scope Pmakeblock55_0/85N))))}))
inner: ((extra_params (naked_immediate/76N ∷ ℕ𝕚))
        (extra_args
         {(5
           (Ok ((New_let_binding naked_immediate/77N (Untag_imm *match*/68N)))))
          (8 Invalid)}))

  We now allow concatenation of mismatched extra params and args as long as the
  mismatch is only between some dead arms that are explicitly marked as invalid
  on one side and have been removed entirely on the other side.
 *)

type r = { b : int; mutable c : int }

let[@inline never] f () = assert false

let () =
  let mutable r = { b = 0; c = 0 } in
  (try f () with _ -> ());
  r.c <- (match This r.b with This x -> x | Null -> r.b)
