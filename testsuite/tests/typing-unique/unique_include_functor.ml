(* TEST
 flags = "-extension include_functor";
 expect;
*)

(* This file tests the interaction of [include functor] with the uniqueness
   analysis. *)

let unique_id (x @ unique) = ignore x

module type s = sig val x : string end

[%%expect{|
val unique_id : 'a @ unique -> unit = <fun>
module type s = sig val x : string end
|}]

(* [include functor] may not consume the preceding items uniquely, since they
   remain accessible afterwards. *)
let include_functor_unique_param () =
  let module F (X : s @ unique) = struct let y = X.x end in
  let module M = struct
    let x = "foo"
    include functor F
  end in
  ()
[%%expect{|
Line 5, characters 4-21:
5 |     include functor F
        ^^^^^^^^^^^^^^^^^
Error: The module is "aliased" but is expected to be "unique".
|}]

(* The preceding items matched by the parameter signature are captured by the
   result of the application, so they cannot be consumed uniquely later. *)
let include_functor_capture () =
  let module F (X : s) = struct let y = X.x end in
  let module M = struct
    let x = "foo"
    include functor F
  end in
  unique_id M.x
[%%expect{|
Line 7, characters 12-15:
7 |   unique_id M.x
                ^^^
Error: This value is used here as unique, but it has already been used at:
Line 5, characters 4-21:
5 |     include functor F
        ^^^^^^^^^^^^^^^^^

|}]

(* Items not matched by the parameter signature are unaffected. *)
let include_functor_unmatched () =
  let module F (X : sig end) = struct let z = "bar" end in
  let module M = struct
    let x = "foo"
    let () = unique_id x
    include functor F
  end in
  ()
[%%expect{|
val include_functor_unmatched : unit -> unit = <fun>
|}]
