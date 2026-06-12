(* TEST
 expect;
*)

(* This file tests the tracking of modules and their components by the
   uniqueness analysis. *)

let unique_id (x @ unique) = ignore x

module type s = sig val x : string end

[%%expect{|
val unique_id : 'a @ unique -> unit = <fun>
module type s = sig val x : string end
|}]

(* Distinct components of a module can be consumed uniquely, separately. *)
let proj_two_components () =
  let module M = struct
    let x = "foo"
    let y = "bar"
  end in
  unique_id M.x;
  unique_id M.y
[%%expect{|
val proj_two_components : unit -> unit = <fun>
|}]

(* The same component cannot be consumed uniquely twice. *)
let proj_twice () =
  let module M = struct
    let x = "foo"
  end in
  unique_id M.x;
  unique_id M.x
[%%expect{|
Line 6, characters 12-15:
6 |   unique_id M.x
                ^^^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 12-15:
5 |   unique_id M.x;
                ^^^

|}]

(* Using a component as aliased is fine, many times. *)
let proj_aliased () =
  let module M = struct
    let x = "foo"
  end in
  ignore M.x;
  ignore M.x
[%%expect{|
val proj_aliased : unit -> unit = <fun>
|}]

(* A module alias shares the components of the original module. *)
let alias_conflict () =
  let module M = struct
    let x = "foo"
  end in
  let module N = M in
  unique_id N.x;
  unique_id M.x
[%%expect{|
Line 7, characters 12-15:
7 |   unique_id M.x
                ^^^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 12-15:
6 |   unique_id N.x;
                ^^^

|}]

(* A binding inside a module aliases the bound value: consuming the component
   consumes the value. *)
let module_binding_aliases () =
  let x = "foo" in
  let module M = struct
    let y = x
  end in
  unique_id M.y;
  unique_id x
[%%expect{|
Line 7, characters 12-13:
7 |   unique_id x
                ^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 12-15:
6 |   unique_id M.y;
                ^^^

|}]

(* Opened components are tracked like ordinary projections. *)
let open_conflict () =
  let module M = struct
    let x = "foo"
  end in
  let open M in
  unique_id x;
  unique_id M.x
[%%expect{|
Line 7, characters 12-15:
7 |   unique_id M.x
                ^^^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 12-13:
6 |   unique_id x;
                ^

|}]

(* [open struct ... end] binds the structure's components. *)
let open_struct_conflict () =
  let y = "foo" in
  let open (struct let x = y end) in
  unique_id x;
  unique_id y
[%%expect{|
Line 5, characters 12-13:
5 |   unique_id y
                ^
Error: This value is used here, but it has already been used as unique at:
Line 4, characters 12-13:
4 |   unique_id x;
                ^

|}]

(* Included components share the components of the included module. *)
let include_conflict () =
  let module M = struct
    let x = "foo"
  end in
  let module N = struct include M end in
  unique_id N.x;
  unique_id M.x
[%%expect{|
Line 7, characters 12-15:
7 |   unique_id M.x
                ^^^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 12-15:
6 |   unique_id N.x;
                ^^^

|}]

(* Packing and unpacking a module preserves the tracking of its components. *)
let pack_unpack () =
  let z = (module struct let x = "foo" end : s) in
  let (module M) = z in
  unique_id M.x
[%%expect{|
val pack_unpack : unit -> unit = <fun>
|}]

(* A module cannot be packed uniquely twice. *)
let pack_twice () =
  let module M = struct
    let x = "foo"
  end in
  unique_id (module M : s);
  unique_id (module M : s)
[%%expect{|
Line 6, characters 20-21:
6 |   unique_id (module M : s)
                        ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 20-21:
5 |   unique_id (module M : s);
                        ^

|}]

(* A component cannot be used after the module is consumed uniquely. *)
let proj_after_pack () =
  let module M = struct
    let x = "foo"
  end in
  unique_id (module M : s);
  ignore M.x
[%%expect{|
Line 6, characters 9-12:
6 |   ignore M.x
             ^^^
Error: This value is read from here,
       but it has already been used as unique at:
Line 5, characters 20-21:
5 |   unique_id (module M : s);
                        ^

|}]

(* Packing a module uses its components: the values they alias cannot be
   consumed uniquely afterwards. *)
let pack_uses_components () =
  let y = "foo" in
  let module M = struct
    let x = y
  end in
  ignore (module M : s);
  unique_id y
[%%expect{|
Line 7, characters 12-13:
7 |   unique_id y
                ^
Error: This value is used here as unique, but it has already been used at:
Line 6, characters 17-18:
6 |   ignore (module M : s);
                     ^

|}]

(* Bindings at the top level of a phrase (or of a compilation unit without an
   interface) are exported, so they cannot be consumed uniquely. *)
let exported = "foo"
let () = unique_id exported
[%%expect{|
val exported : string = "foo"
Line 2, characters 19-27:
2 | let () = unique_id exported
                       ^^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* Same for components of a top-level module. *)
module M = struct
  let x = "foo"
end
let () = unique_id M.x
[%%expect{|
module M : sig val x : string end
Line 4, characters 19-22:
4 | let () = unique_id M.x
                       ^^^
Error: This value is aliased but used as unique.
Hint: This value comes from another module or class.
|}]

(* The body of [module type of] is never evaluated, but it is still checked
   like any other code. *)
module type t_ok = module type of struct
  let x = "foo"
  let () = unique_id x
end
[%%expect{|
module type t_ok = sig val x : string @@ stateless end
|}]

module type t_bad = module type of struct
  let x = "foo"
  let () = unique_id x
  let () = unique_id x
end
[%%expect{|
Line 4, characters 21-22:
4 |   let () = unique_id x
                         ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 21-22:
3 |   let () = unique_id x
                         ^

|}]

(* Recursive modules are not tracked: their components are conservatively
   aliased. *)
module rec R : s = struct
  let x = "foo"
end
and Use_r : sig end = struct
  let () = ignore (unique_id R.x)
end
[%%expect{|
Line 5, characters 29-32:
5 |   let () = ignore (unique_id R.x)
                                 ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* A component of a recursive module that aliases an outer value shares that
   value's paths: consuming the value uniquely conflicts with reading the
   alias. *)
let recmodule_binding_aliases () =
  let s = "foo" in
  let module M = struct
    module rec R : s = struct let x = s end
  end in
  let alias = M.R.x in
  unique_id s;
  alias
[%%expect{|
Line 8, characters 2-7:
8 |   alias
      ^^^^^
Error: This value is used here, but it has already been used as unique at:
Line 7, characters 12-13:
7 |   unique_id s;
                ^

|}]

(* Consuming a module uniquely consumes its components uniquely too: a value
   aliased by a component cannot even be read afterwards. *)
let consume_unique_then_read_alias () =
  let y = "foo" in
  let module M = struct let x = y end in
  unique_id (module M : s);
  ignore y
[%%expect{|
val consume_unique_then_read_alias : unit -> unit = <fun>
|}]
