(* Same as [stability.ml] (see the test description there), except for this
   comment, the [unrelated_*] functions and the use of
   [Stability_lib_edited] instead of [Stability_lib]. *)

let[@inline never] unrelated_prefix x = Sys.opaque_identity (x * 7)

let adder = Stability_lib_edited.make_adder (Sys.opaque_identity 10)

let[@inline never] double_even numbers =
  let even = List.filter (fun[@cold] x -> x mod 2 = 0) numbers in
  List.map (fun[@cold] x -> x * 2) even

let[@inline never] labelled ~a ~b = a - b

let[@inline never] omit_first x = labelled ~b:x

let curried = labelled ~a:(Sys.opaque_identity 1)

let thunk = lazy (Sys.opaque_identity 41 + 1)

let[@inline never] use_lib xs =
  Stability_lib_edited.twice
    (fun[@cold] z -> z * 3)
    (List.length (Stability_lib_edited.scale_all 2 xs))

let[@inline never] unrelated_suffix x = Sys.opaque_identity (x - 7)

let () =
  ignore (unrelated_prefix 1);
  ignore (unrelated_suffix 1);
  ignore (adder 1);
  ignore (double_even [ 1; 2; 3; 4 ]);
  ignore (omit_first 1 ~a:2);
  ignore (curried ~b:3);
  ignore (Lazy.force thunk);
  ignore (use_lib [ 5; 6 ])
