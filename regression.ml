type void : void

type r = { i : int; v : void }
type variant = V of void
exception E of int * variant

let f x =
  match x with
  | { i = _; v }
  | exception (E (_, V v)) ->
      v

(*
$ ./out/bin/ocamlopt -dlambda regression.ml
(let
  (E/282 = (makeblock_unique 248 "Regression.E" (caml_fresh_oo_id 0))
   f/283 =
     (function {nlocal = 0}
       x/285[(consts ()) (non_consts ([0: [int], product ]))]
       : #()(catch
              (catch
                (try (exit 2 x/285) with exn/297
                  (if (== (field_imm 0 exn/297) E/282)
                    (let (v/286 =a#() (make_unboxed_product #()))
                      (exit 1 v/286))
                    (reraise exn/297)))
               with (2 val/295[(consts ())
                               (non_consts ([0: [int], product ]))])
                (exit 1 (mixedfield 1  (,product ) val/295)))
             with (1 v/286#()) v/286)))
  (makeblock 0 E/282 f/283))
>> Fatal error: Closure_conversion.Env.find_var: v_286
Raised by primitive operation at Flambda2_from_lambda__Closure_conversion_aux.Env.find_var in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 262, characters 42-69
Called from Flambda2_from_lambda__Lambda_to_flambda.cps in file "middle_end/flambda2/from_lambda/lambda_to_flambda.ml", line 448, characters 37-62
Called from Flambda2_from_lambda__Closure_conversion_aux.Let_cont_with_acc.build_non_recursive.(fun) in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 1194, characters 29-40
Called from Flambda2_from_lambda__Closure_conversion_aux.Acc.eval_branch_free_names in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 670, characters 17-65
Called from Flambda2_from_lambda__Closure_conversion_aux.Acc.measure_cost_metrics in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 676, characters 34-63
Called from Flambda2_from_lambda__Closure_conversion_aux.Let_cont_with_acc.build_non_recursive in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 1193, characters 6-206
Called from Flambda2_from_lambda__Closure_conversion.close_one_function.compute_body in file "middle_end/flambda2/from_lambda/closure_conversion.ml", line 2423, characters 10-30
Called from Flambda2_from_lambda__Closure_conversion.close_one_function in file "middle_end/flambda2/from_lambda/closure_conversion.ml", line 2491, characters 22-38
Called from Flambda2_from_lambda__Closure_conversion_aux.Acc.eval_branch_free_names in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 670, characters 17-65
Called from Flambda2_from_lambda__Closure_conversion_aux.Acc.measure_cost_metrics in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml", line 676, characters 34-63
Called from Flambda2_from_lambda__Closure_conversion.close_functions.(fun) in file "middle_end/flambda2/from_lambda/closure_conversion.ml", line 2801, characters 10-361
*)
