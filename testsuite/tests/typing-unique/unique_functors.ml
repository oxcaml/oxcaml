(* TEST
 expect;
*)

(* This file tests the tracking of functors by the uniqueness analysis. *)

let unique_id (x @ unique) = ignore x

module type s = sig val x : string end

[%%expect{|
val unique_id : 'a @ unique -> unit = <fun>
module type s = sig val x : string end
|}]

(* A functor with an aliased parameter can be applied repeatedly to the same
   module. *)
let apply_aliased_twice () =
  let module M = struct let x = "foo" end in
  let module F (X : s) = struct let y = X.x end in
  let module A = F(M) in
  let module B = F(M) in
  ()
[%%expect{|
val apply_aliased_twice : unit -> unit = <fun>
|}]

(* A functor with a unique parameter consumes its argument. *)
let apply_unique_once () =
  let module M = struct let x = "foo" end in
  let module F (X : s @ unique) = struct let y = X.x end in
  let module A = F(M) in
  ()
[%%expect{|
val apply_unique_once : unit -> unit = <fun>
|}]

(* The same argument cannot be consumed by two applications. *)
let apply_unique_twice () =
  let module M = struct let x = "foo" end in
  let module F (X : s @ unique) = struct let y = X.x end in
  let module A = F(M) in
  let module B = F(M) in
  ()
[%%expect{|
val apply_unique_twice : unit -> unit = <fun>
|}]

(* The argument's components cannot be consumed uniquely after the argument
   itself was consumed by an application. *)
let arg_used_after_apply () =
  let module M = struct let x = "foo" end in
  let module F (X : s @ unique) = struct let y = X.x end in
  let module A = F(M) in
  unique_id M.x
[%%expect{|
Line 5, characters 12-15:
5 |   unique_id M.x
                ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* The components of a module returned by a (generative) functor are fresh
   and can be consumed uniquely. *)
let functor_returns_unique () =
  let module F () = struct let x = "fresh" end in
  let module A = F () in
  unique_id A.x
[%%expect{|
Line 4, characters 12-15:
4 |   unique_id A.x
                ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* But not twice. *)
let functor_returns_unique_twice () =
  let module F () = struct let x = "fresh" end in
  let module A = F () in
  unique_id A.x;
  unique_id A.x
[%%expect{|
Line 4, characters 12-15:
4 |   unique_id A.x;
                ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* Components included from an application of a functor with an aliased
   parameter are aliased. *)
let include_apply () =
  let module M = struct let x = "foo" end in
  let module F (X : s) = struct let y = X.x end in
  let module N = struct include F(M) end in
  unique_id N.y;
  unique_id N.y
[%%expect{|
Line 5, characters 12-15:
5 |   unique_id N.y;
                ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* A functor that consumes a free variable uniquely can be applied once. *)
let functor_body_unique () =
  let x = "foo" in
  let module F () = struct let y = unique_id x end in
  let module A = F () in
  ()
[%%expect{|
Line 3, characters 45-46:
3 |   let module F () = struct let y = unique_id x end in
                                                 ^
Error: This value is aliased but used as unique.
Hint: This value comes from outside the current module or class.
|}]

(* But not twice, since each application would consume the free variable. *)
let functor_body_unique_twice () =
  let x = "foo" in
  let module F () = struct let y = unique_id x end in
  let module A = F () in
  let module B = F () in
  ()
[%%expect{|
Line 3, characters 45-46:
3 |   let module F () = struct let y = unique_id x end in
                                                 ^
Error: This value is aliased but used as unique.
Hint: This value comes from outside the current module or class.
|}]

(* A component of a functor result that aliases a captured free variable
   shares that variable's paths: reading the component then consuming the
   variable uniquely conflicts. *)
let functor_result_captures () =
  let x = "foo" in
  let module F () = struct let y = x end in
  let module M = F () in
  let r = M.y in
  unique_id x;
  ignore r
[%%expect{|
Line 6, characters 12-13:
6 |   unique_id x;
                ^
Error: This value is used here as unique, but it has already been used at:
Line 3, characters 35-36:
3 |   let module F () = struct let y = x end in
                                       ^

|}]

(* Two components of a functor result that alias the same parameter component
   alias each other. *)
let functor_result_siblings_alias () =
  let module M = struct let x = "foo" end in
  let module F (X : s @ unique) = struct let y = X.x let z = X.x end in
  let module A = F(M) in
  unique_id A.y;
  ignore A.z
[%%expect{|
Line 5, characters 12-15:
5 |   unique_id A.y;
                ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* Each application of a generative functor creates a fresh structure, so the
   results can be consumed uniquely, independently. *)
let generative_results_independent () =
  let module F () = struct let x = "fresh" end in
  let module A = F () in
  let module B = F () in
  unique_id A.x;
  unique_id B.x
[%%expect{|
Line 5, characters 12-15:
5 |   unique_id A.x;
                ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* Applications to different arguments yield independent results. *)
let apply_results_independent () =
  let module M1 = struct let x = "a" end in
  let module M2 = struct let x = "b" end in
  let module F (X : s @ unique) = struct let y = X.x end in
  let module A = F(M1) in
  let module B = F(M2) in
  unique_id A.y;
  unique_id B.y
[%%expect{|
Line 7, characters 12-15:
7 |   unique_id A.y;
                ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* The same holds for each application of a partially-applied curried
   functor. *)
let curried_results_independent () =
  let module M = struct let x = "m" end in
  let module N1 = struct let x = "a" end in
  let module N2 = struct let x = "b" end in
  let module F (X : s) (Y : s @ unique) = struct let y = Y.x end in
  let module G = F(M) in
  let module A = G(N1) in
  let module B = G(N2) in
  unique_id A.y;
  unique_id B.y
[%%expect{|
Line 9, characters 12-15:
9 |   unique_id A.y;
                ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* A component aliasing a captured free variable shares that variable's paths
   even through several applications of a curried functor. *)
let curried_result_captures () =
  let x = "foo" in
  let module F () () = struct let y = x end in
  let module G = F () in
  let module A = G () in
  let r = A.y in
  unique_id x;
  ignore r
[%%expect{|
Line 7, characters 12-13:
7 |   unique_id x;
                ^
Error: This value is used here as unique, but it has already been used at:
Line 3, characters 38-39:
3 |   let module F () () = struct let y = x end in
                                          ^

|}]

(* A functor with a [unique] parameter consumes its argument's components
   uniquely. When the argument is an inline structure whose component aliases an
   outer value, that consumption is at the functor's parameter mode (see
   [functor_arg_demand]): the two applications below each consume the outer [x]
   uniquely, so the second is rejected, just like [apply_unique_twice] with a
   bound argument. *)
let unique_arg_struct_literal () =
  let x = "foo" in
  let module F (X : s @ unique) = struct let _ = unique_id X.x end in
  let module _ = F (struct let x = x end) in
  let module _ = F (struct let x = x end) in
  ()
[%%expect{|
Line 3, characters 59-62:
3 |   let module F (X : s @ unique) = struct let _ = unique_id X.x end in
                                                               ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* The same, shown as a use-after-consume: [F] consumes the outer [x] uniquely
   through the inline-structure argument, so the later read of [x] is rejected,
   like the bound-argument analogue. *)
let read_after_unique_arg_struct_literal () =
  let x = "foo" in
  let module F (X : s @ unique) = struct let _ = unique_id X.x end in
  let module _ = F (struct let x = x end) in
  ignore x
[%%expect{|
Line 3, characters 59-62:
3 |   let module F (X : s @ unique) = struct let _ = unique_id X.x end in
                                                               ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]
