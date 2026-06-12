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
Line 5, characters 19-20:
5 |   let module B = F(M) in
                       ^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 19-20:
4 |   let module A = F(M) in
                       ^

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
Error: This value is read from here,
       but it has already been used as unique at:
Line 4, characters 19-20:
4 |   let module A = F(M) in
                       ^

|}]

(* The components of a module returned by a (generative) functor are fresh
   and can be consumed uniquely. *)
let functor_returns_unique () =
  let module F () = struct let x = "fresh" end in
  let module A = F () in
  unique_id A.x
[%%expect{|
val functor_returns_unique : unit -> unit = <fun>
|}]

(* But not twice. *)
let functor_returns_unique_twice () =
  let module F () = struct let x = "fresh" end in
  let module A = F () in
  unique_id A.x;
  unique_id A.x
[%%expect{|
Line 5, characters 12-15:
5 |   unique_id A.x
                ^^^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 12-15:
4 |   unique_id A.x;
                ^^^

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
val functor_body_unique : unit -> unit = <fun>
|}]

(* But not twice, since each application would consume the free variable. *)
let functor_body_unique_twice () =
  let x = "foo" in
  let module F () = struct let y = unique_id x end in
  let module A = F () in
  let module B = F () in
  ()
[%%expect{|
Line 5, characters 17-18:
5 |   let module B = F () in
                     ^
Error: This value is used here,
       but it is defined as once and has already been used at:
Line 4, characters 17-18:
4 |   let module A = F () in
                     ^

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
val functor_result_captures : unit -> unit = <fun>
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
val functor_result_siblings_alias : unit -> unit = <fun>
|}]
