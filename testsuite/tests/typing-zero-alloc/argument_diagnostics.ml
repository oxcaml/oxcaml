(* TEST
   flags = "-g -O3";
   expect.opt;
*)

let[@zero_alloc] non_tail_argument f x = f x + 1
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] non_tail_argument f x = f x + 1
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP1.non_tail_argument (camlTOP1__non_tail_argument_0_1_code).
Line 1, characters 41-44:
1 | let[@zero_alloc] non_tail_argument f x = f x + 1
                                             ^^^
Error: called function may allocate (indirect call through argument #1)
|}]

let[@zero_alloc] tail_argument x f = f x
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] tail_argument x f = f x
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP2.tail_argument (camlTOP2__tail_argument_1_3_code).
Line 1, characters 37-40:
1 | let[@zero_alloc] tail_argument x f = f x
                                         ^^^
Error: called function may allocate (indirect tailcall through argument #2)
|}]

let[@zero_alloc] tupled_argument (x, f) = f x
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] tupled_argument (x, f) = f x
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP3.tupled_argument (camlTOP3__tupled_argument_2_5_code).
Line 1, characters 42-45:
1 | let[@zero_alloc] tupled_argument (x, f) = f x
                                              ^^^
Error: called function may allocate (indirect tailcall through argument #2)
|}]

let[@zero_alloc] unknown_callback cell x = !cell x
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] unknown_callback cell x = !cell x
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP4.unknown_callback (camlTOP4__unknown_callback_3_7_code).
Line 1, characters 43-50:
1 | let[@zero_alloc] unknown_callback cell x = !cell x
                                               ^^^^^^^
Error: called function may allocate (indirect tailcall)
|}]

let[@zero_alloc] known_callbacks b x =
  let[@inline never] left y = ref y in
  let[@inline never] right y = ref (y + 1) in
  (if b then left else right) x
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] known_callbacks b x =
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP5.known_callbacks (camlTOP5__known_callbacks_4_11_code).
Line 4, characters 2-31:
4 |   (if b then left else right) x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (indirect tailcall (possibly calling camlTOP5__left_5_12_code))
Line 4, characters 2-31:
4 |   (if b then left else right) x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: called function may allocate (indirect tailcall (possibly calling camlTOP5__right_6_13_code))
|}]
