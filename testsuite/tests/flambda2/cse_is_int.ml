(* TEST
   flags += " -O3 -flambda2-join-algorithm=n-way";
   only-default-codegen;
   expect.opt
 *)

(* This is an instance where flambda2 is "too smart" and we end up with
   slightly worse code than we could have.

   Previously (and with the legacy join algorithm), we had a bug where the
   flambda IR we generate for this would have two instances of the %is_int
   primitive [1]:

   ```fexpr
   (let prim = %is_int (x) in
   switch prim
     | 0 -> apply g (0) -> k6 * exn
     | 1 -> apply f (0) -> k6 * exn)
    where k6 (y) =
      (apply h (x, y) -> k11 * exn
         where k11 (z) =
           let prim = %is_int (x) in
           (switch prim
              | 0 ->
                ret (0)
              | 1 -> 
                let Pmakeblock = %block.[`0`] (z) in
                ret (Pmakeblock)))
   ```

   but with the new join algorithm we now compute the following IR instead:

   ```fexpr
   (let prim = %is_int (x) in
   switch prim
     | 0 -> 
      (apply g (0) -> k31 * exn
        where k31 (param) =
          cont k6 (param, 0i))
     | 1 ->
      (apply f (0) -> k30 * exn
        where k30 (param) =
          cont k6 (param, 1i)))
    where k6 (y, join_param : imm) =
      (apply h (x, y) -> k11 * exn
         where k11 (z) =
           (switch join_param
              | 0 ->
                ret (0)
              | 1 ->
                let Pmakeblock = %block.[`0`] (z) in
                ret (Pmakeblock)))
   ```

   This looks better at the flambda2 level (we have eliminated one instance of
   the `%is_int` primitive), but actually leads to worse assembly.

   With the old code (where the second `%is_int` primitive was *NOT*
   eliminated), we would generate a single `testb $1, %al` instruction for the
   each `%is_int` primitive.

   With the new code (where the second `%is_int` primitive is eliminated),
   instead, we instead store the result (using `movl` or `xorl`), and use a
   `testq %rax, %rax` instruction for the second `%is_int` primitive. This is 3
   instructions instead of 1.

   On the other hand, if there is no call to `h` in the code below, the cfg
   backend is able to simplify the control flow better and remove the second
   `testb` entirely (on real-world code, this situation is both rarer and
   should be handled by the match-in-match flambda transformation soon).

   One way of fixing this would be for the flambda IR to be consider an
   alternative:

   ```fexpr
   (let prim = %is_int (x) in
   switch prim
     | 0 -> 
      (apply g (0) -> k31 * exn
        where k31 (param) =
          cont k6 (param, 0i))
     | 1 ->
      (apply f (0) -> k30 * exn
        where k30 (param) =
          cont k6 (param, 1i)))
    where k6 (y, join_param : imm) =
      (apply h (x, y) -> k11 * exn
         where k11 (z) =
           (let prim = either [ join_param | %is_int (x) ] in
           switch prim
              | 0 ->
                ret (0)
              | 1 ->
                let Pmakeblock = %block.[`0`] (z) in
                ret (Pmakeblock)))
   ```

   and delay the decision of whether to keep the `join_param` variable or to
   re-materialize the `%is_int` primitive. This could be done wholly inside the
   flambda2 pass (during the global dataflow analysis), or potentially by
   propagating this information to the backend where the decision could be made
   using more accurate information regarding e.g. liveness, register pressure,
   etc.

   [1]: https://github.com/oxcaml/oxcaml/issues/3181
 *)

let main ~f ~g ~h x =
  let y =
    match x with
    | None -> f ()
    | Some _ -> g ()
  in
  let z = h x y in
  match x with
  | None -> Some z
  | Some _ -> None
[%expect_asm X86_64 {| |}]
