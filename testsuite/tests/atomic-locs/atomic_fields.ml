(* TEST
   expect;
*)

type t = {
  x : int;
  mutable y : int [@atomic];
}

let get_y (t : t) = t.y
let set_y (t : t) new_y = t.y <- new_y
[%%expect{|
type t = { x : int; mutable y : int [@atomic]; }
val get_y : t -> int = <fun>
val set_y : t -> int -> unit = <fun>
|}]

let t : t = { x = 1; y = 2 }
[%%expect{|
val t : t = {x = 1; y = 2}
|}]

let () = Format.printf "%d@." (get_y t)
[%%expect{|
2
|}]

let () = set_y t 7
[%%expect{|
|}]

let () = Format.printf "%d@." (get_y t)
[%%expect{|
7
|}]

(* Test with non-immediates too *)

type u = {
  x : int;
  mutable y : string [@atomic];
}

let get_y (t : u) = t.y
let set_y (t : u) new_y = t.y <- new_y
[%%expect{|
type u = { x : int; mutable y : string [@atomic]; }
val get_y : u -> string = <fun>
val set_y : u -> string -> unit = <fun>
|}]

let t : u = { x = 1; y = "two" }
[%%expect{|
val t : u = {x = 1; y = "two"}
|}]

let () = Format.printf "%s@." (get_y t)
[%%expect{|
two
|}]

let () = set_y t "seven"
[%%expect{|
|}]

let () = Format.printf "%s@." (get_y t)
[%%expect{|
seven
|}]

(* Test with floats *)

module Floats_some_atomic = struct
  type t =
    { x : float
    ; mutable y : float
    ; mutable z : float [@atomic]
    }
  [@@warning "-214"]

  let t : t = { x = 1.; y = 2.; z = 3.; }

  let () = Format.printf "after init, x = %f@." (t.x)
  let () = Format.printf "after init, y = %f@." (t.y)
  let () = Format.printf "after init, z = %f@." (t.z)

  let () =
    t.y <- 2.5;
    t.z <- 3.5


  let () = Format.printf "after set, x = %f@." (t.x)
  let () = Format.printf "after set, y = %f@." (t.y)
  let () = Format.printf "after set, z = %f@." (t.z)
end
[%%expect{|
after init, x = 1.000000
after init, y = 2.000000
after init, z = 3.000000
after set, x = 1.000000
after set, y = 2.500000
after set, z = 3.500000
module Floats_some_atomic :
  sig
    type t = { x : float; mutable y : float; mutable z : float [@atomic]; }
    val t : t
  end
|}]

(* Test with or_null *)

type string_or_null_atomic = { mutable s : string or_null [@atomic] }

let print t = match t.s with
  | Null -> Format.printf "Null@.";
  | This s -> Format.printf "This %s@." s;
;;
[%%expect{|
type string_or_null_atomic = { mutable s : string or_null [@atomic]; }
val print : string_or_null_atomic -> unit = <fun>
|}]

let x : string_or_null_atomic = { s = Null } ;;
[%%expect{|
val x : string_or_null_atomic = {s = Null}
|}]

let () = print x ;;
[%%expect{|
Null
|}]

let () = x.s <- This "Value" ;;
let () = print x ;;
[%%expect{|
This Value
|}]

let () = x.s <- Null ;;
let () = print x ;;
[%%expect{|
Null
|}]

(* Test atomic.loc construct *)

type atomic_record = {
  x : int;
  mutable atomic_field : string [@atomic];
  mutable regular_field : string;
}

let test_atomic_loc (r : atomic_record) = [%atomic.loc r.atomic_field]
[%%expect{|
type atomic_record = {
  x : int;
  mutable atomic_field : string [@atomic];
  mutable regular_field : string;
}
val test_atomic_loc : atomic_record -> string atomic_loc = <fun>
|}]

(* Test Label_not_atomic error *)
let test_non_atomic_field (r : atomic_record) = [%atomic.loc r.regular_field]
[%%expect{|
Line 1, characters 48-77:
1 | let test_non_atomic_field (r : atomic_record) = [%atomic.loc r.regular_field]
                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The record field "regular_field" is not atomic
|}]

(* Test Invalid_atomic_loc_payload errors *)

(* Empty payload *)
let test_empty_payload = [%atomic.loc]
[%%expect{|
Line 1, characters 25-38:
1 | let test_empty_payload = [%atomic.loc]
                             ^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}]

(* Non-field expression *)
let test_non_field = [%atomic.loc 42]
[%%expect{|
Line 1, characters 21-37:
1 | let test_non_field = [%atomic.loc 42]
                         ^^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}]

(* Missing field access *)
let test_missing_field (r : atomic_record) = [%atomic.loc r]
[%%expect{|
Line 1, characters 45-60:
1 | let test_missing_field (r : atomic_record) = [%atomic.loc r]
                                                 ^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}]

(* Test with function application result *)
let get_record () : atomic_record = { x = 1; atomic_field = "test"; regular_field = "test" }
let test_function_result = [%atomic.loc (get_record ()).atomic_field]
[%%expect{|
val get_record : unit -> atomic_record = <fun>
val test_function_result : string atomic_loc = <abstr>
|}]
