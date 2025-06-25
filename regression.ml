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
Raised by primitive operation at Flambda2_from_lambda__Closure_conversion_aux.Env.find_var in file "middle_end/flambda2/from_lambda/closure_conversion_aux.ml" (inlined), line 262, characters 42-69
*)
