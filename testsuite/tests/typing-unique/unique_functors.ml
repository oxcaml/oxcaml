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
