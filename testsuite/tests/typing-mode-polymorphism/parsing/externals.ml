(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* This is fine *)
external magic : 'a @ [< 'm] -> 'b @ [> 'm] = "%identity"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(magic : 'a -> 'a);;
[%%expect{|
Line 1, characters 1-6:
1 | (magic : 'a -> 'a);;
     ^^^^^
Error: Unbound value "magic"
|}];;

(magic : 'a @ local -> 'a @ local);;
[%%expect{|
Line 1, characters 1-6:
1 | (magic : 'a @ local -> 'a @ local);;
     ^^^^^
Error: Unbound value "magic"
|}];;

(magic : 'a @ unique -> 'a @ unique);;
[%%expect{|
Line 1, characters 1-6:
1 | (magic : 'a @ unique -> 'a @ unique);;
     ^^^^^
Error: Unbound value "magic"
|}];;

(fun x -> magic x : 'a -> 'a);;
[%%expect{|
Line 1, characters 10-15:
1 | (fun x -> magic x : 'a -> 'a);;
              ^^^^^
Error: Unbound value "magic"
|}];;

(fun x -> magic x : 'a @ unique -> 'a @ unique);;
[%%expect{|
Line 1, characters 10-15:
1 | (fun x -> magic x : 'a @ unique -> 'a @ unique);;
              ^^^^^
Error: Unbound value "magic"
|}];;

(fun x -> exclave_ magic x : 'a @ local -> 'a @ local);;
[%%expect{|
Line 1, characters 19-24:
1 | (fun x -> exclave_ magic x : 'a @ local -> 'a @ local);;
                       ^^^^^
Error: Unbound value "magic"
|}];;

(fun x -> magic x : 'a @ yielding -> 'a @ yielding);;
[%%expect{|
Line 1, characters 10-15:
1 | (fun x -> magic x : 'a @ yielding -> 'a @ yielding);;
              ^^^^^
Error: Unbound value "magic"
|}];;

(* But not under an instantiation that violates the mode polymorphic signature *)
(fun x -> magic x : 'a @ aliased -> 'a @ unique);;
[%%expect{|
Line 1, characters 10-15:
1 | (fun x -> magic x : 'a @ aliased -> 'a @ unique);;
              ^^^^^
Error: Unbound value "magic"
|}];;

module Id_locality_should_fail : sig
  val id : 'a @ [< 'm] -> 'a @ [> 'm]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Foo : sig
  val id : 'a @ [< 'm] -> 'a @ [> 'm | local]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}]

module Foo : sig
  val id : 'a @ [< 'm & global] -> 'a @ [> 'm]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}]

module Foo : sig
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(fun x y -> add x y);;
[%%expect{|
Line 1, characters 12-15:
1 | (fun x y -> add x y);;
                ^^^
Error: Unbound value "add"
|}];;

(fun (x @ local) y -> exclave_ add x y);;
[%%expect{|
Line 1, characters 31-34:
1 | (fun (x @ local) y -> exclave_ add x y);;
                                   ^^^
Error: Unbound value "add"
|}];;

(add : int32 -> int32 -> int32);;
[%%expect{|
Line 1, characters 1-4:
1 | (add : int32 -> int32 -> int32);;
     ^^^
Error: Unbound value "add"
|}];;

(add : int32 @ local -> int32 @ local -> int32 @ local);;
[%%expect{|
Line 1, characters 1-4:
1 | (add : int32 @ local -> int32 @ local -> int32 @ local);;
     ^^^
Error: Unbound value "add"
|}];;

(fun x -> add x);;
[%%expect{|
Line 1, characters 10-13:
1 | (fun x -> add x);;
              ^^^
Error: Unbound value "add"
|}];;

(fun (x @ local) -> exclave_ add x);;
[%%expect{|
Line 1, characters 29-32:
1 | (fun (x @ local) -> exclave_ add x);;
                                 ^^^
Error: Unbound value "add"
|}];;

let () =
  let use_global (_ @ global) = () in
  let xh = Int32.of_int 3 in
  let xl = stack_ (Int32.of_int 4) in
  use_global (add xh);
  use_global (add xl) (* should fail *)
[%%expect{|
Line 5, characters 14-17:
5 |   use_global (add xh);
                  ^^^
Error: Unbound value "add"
|}];;

external add_old :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
external add_old :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
|}];;

(add_old : int32 -> int32 -> int32);;
[%%expect{|
- : int32 -> int32 -> int32 = <fun>
|}];;

(fun x -> add_old x);;
[%%expect{|
- : int32 @ [< global] -> (int32 -> int32) @ [> nonportable stateful dynamic]
= <fun>
|}];;

external add_indep : int32 @ [< 'm] -> int32 @ [< 'n] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(fun x (y @ local) -> add_indep x y);;
[%%expect{|
Line 1, characters 22-31:
1 | (fun x (y @ local) -> add_indep x y);;
                          ^^^^^^^^^
Error: Unbound value "add_indep"
|}];;

external add_local_arg : int32 @ local -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(fun (x @ local) y -> add_local_arg x y);;
[%%expect{|
Line 1, characters 22-35:
1 | (fun (x @ local) y -> add_local_arg x y);;
                          ^^^^^^^^^^^^^
Error: Unbound value "add_local_arg"
|}];;

(fun x (y @ local) -> exclave_ add_local_arg x y);;
[%%expect{|
Line 1, characters 31-44:
1 | (fun x (y @ local) -> exclave_ add_local_arg x y);;
                                   ^^^^^^^^^^^^^
Error: Unbound value "add_local_arg"
|}];;

external add_global :
  int32 @ [< 'm & global] -> int32 @ [< 'm] -> int32 @ [> 'm] = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(fun (x @ local) y -> exclave_ add_global x y);;
[%%expect{|
Line 1, characters 31-41:
1 | (fun (x @ local) y -> exclave_ add_global x y);;
                                   ^^^^^^^^^^
Error: Unbound value "add_global"
|}];;

(fun x (y @ local) -> exclave_ add_global x y);;
[%%expect{|
Line 1, characters 31-41:
1 | (fun x (y @ local) -> exclave_ add_global x y);;
                                   ^^^^^^^^^^
Error: Unbound value "add_global"
|}];;

external revapply : 'a @ [< 'm] -> ('a @ [> 'm] -> 'b) -> 'b = "%revapply"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let use_global (_ @ global) = ()
[%%expect{|
val use_global : 'a @ [< global] -> unit @ 'm = <fun>
|}];;

(fun x -> revapply x (fun y -> use_global y));;
[%%expect{|
Line 1, characters 10-18:
1 | (fun x -> revapply x (fun y -> use_global y));;
              ^^^^^^^^
Error: Unbound value "revapply"
|}];;

(fun (x @ local) -> revapply x (fun y -> use_global y));;
[%%expect{|
Line 1, characters 20-28:
1 | (fun (x @ local) -> revapply x (fun y -> use_global y));;
                        ^^^^^^^^
Error: Unbound value "revapply"
|}];;

(* local_opt should never be used in a signature with mode polymorphic annotations *)

external add_opt : (int32 [@local_opt]) -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external add_opt_res :
  int32 @ [< 'm] -> int32 @ [< 'm] -> (int32 [@local_opt]) = "%int32_add"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external opt_arg_var : (int32 [@local_opt]) @ 'm -> int32 = "%int32_neg"
[%%expect{|
external opt_arg_var : (int32 [@local_opt]) @ 'm -> int32 = "%int32_neg"
|}];;

external opt_res_var : int32 -> (int32 [@local_opt]) @ [> 'm] = "%int32_neg"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external opt_global_var :
  (int32 [@local_opt]) -> int32 @ [< 'm & global] -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
external opt_global_var :
  (int32 [@local_opt]) -> int32 @ [< global] -> (int32 [@local_opt])
  = "%int32_add"
|}];;

external opt_callback_arg :
  ('a [@local_opt]) -> ('a @ [< 'm] -> 'b) -> 'b = "%revapply"
[%%expect{|
external opt_callback_arg : ('a [@local_opt]) -> ('a @ 'm -> 'b) -> 'b
  = "%revapply"
|}];;

external opt_callback_res :
  ('a [@local_opt]) -> ('a -> 'b @ [> 'm]) -> 'b = "%revapply"
[%%expect{|
external opt_callback_res : ('a [@local_opt]) -> ('a -> 'b @ 'm) -> 'b
  = "%revapply"
|}];;

external opt_variant :
  ('a [@local_opt]) -> [ `A of 'a @ 'm -> unit ] -> unit = "caml_opt_variant"
[%%expect{|
external opt_variant : ('a [@local_opt]) -> [ `A of 'a @ 'm -> unit ] -> unit
  = "caml_opt_variant"
|}];;

external opt_object :
  ('a [@local_opt]) -> < f : 'a @ 'm -> unit > -> unit = "caml_opt_object"
[%%expect{|
external opt_object : ('a [@local_opt]) -> < f : 'a @ 'm -> unit > -> unit
  = "caml_opt_object"
|}];;

external opt_polytype :
  ('a [@local_opt]) -> ('b. 'b @ 'm -> unit) -> unit = "caml_opt_polytype"
[%%expect{|
external opt_polytype : ('a [@local_opt]) -> ('b. 'b @ 'm -> unit) -> unit
  = "caml_opt_polytype"
|}];;

external opt_constr_arg :
  ('a [@local_opt]) -> ('a @ [< 'm] -> unit) list -> unit = "caml_opt_list"
[%%expect{|
external opt_constr_arg : ('a [@local_opt]) -> ('a @ 'm -> unit) list -> unit
  = "caml_opt_list"
|}];;

external opt_returned_closure :
  (int32 [@local_opt]) -> (int32 @ [< 'm] -> int32 @ [> 'm])
  = "caml_opt_closure"
[%%expect{|
external opt_returned_closure :
  (int32 [@local_opt]) -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "caml_opt_closure"
|}];;

external opt_only :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
external opt_only :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
|}];;

external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
  = "%caml_bytes_set32"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let set32_one_arg = set32 (Bytes.create 4)
[%%expect{|
Line 1, characters 20-25:
1 | let set32_one_arg = set32 (Bytes.create 4)
                        ^^^^^
Error: Unbound value "set32"
|}];;

let set32_two_args = set32 (Bytes.create 4) 0
[%%expect{|
Line 1, characters 21-26:
1 | let set32_two_args = set32 (Bytes.create 4) 0
                         ^^^^^
Error: Unbound value "set32"
|}];;

(fun (b @ local) -> exclave_ set32 b);;
[%%expect{|
Line 1, characters 29-34:
1 | (fun (b @ local) -> exclave_ set32 b);;
                                 ^^^^^
Error: Unbound value "set32"
|}];;

(fun (b @ local) -> exclave_ set32 b 0);;
[%%expect{|
Line 1, characters 29-34:
1 | (fun (b @ local) -> exclave_ set32 b 0);;
                                 ^^^^^
Error: Unbound value "set32"
|}];;

(fun (b @ local) -> set32 b);;
[%%expect{|
Line 1, characters 20-25:
1 | (fun (b @ local) -> set32 b);;
                        ^^^^^
Error: Unbound value "set32"
|}];;

module Set32_val : sig
  val set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
end = struct
  external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let set32_val_one_arg = Set32_val.set32 (Bytes.create 4)
[%%expect{|
Line 1, characters 24-33:
1 | let set32_val_one_arg = Set32_val.set32 (Bytes.create 4)
                            ^^^^^^^^^
Error: Unbound module "Set32_val"
|}];;

module Set32_legacy : sig val set32 : bytes -> int -> int32 -> unit end =
struct
  external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let set32_legacy_one_arg = Set32_legacy.set32 (Bytes.create 4)
[%%expect{|
Line 1, characters 27-39:
1 | let set32_legacy_one_arg = Set32_legacy.set32 (Bytes.create 4)
                               ^^^^^^^^^^^^
Error: Unbound module "Set32_legacy"
|}];;

module type Same_type = sig
  external e : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
  val v : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let same_type_external (module X : Same_type) = X.e (Bytes.create 4)
[%%expect{|
Line 1, characters 35-44:
1 | let same_type_external (module X : Same_type) = X.e (Bytes.create 4)
                                       ^^^^^^^^^
Error: Unbound module type "Same_type"
|}];;

let same_type_val (module X : Same_type) = X.v (Bytes.create 4)
[%%expect{|
Line 1, characters 30-39:
1 | let same_type_val (module X : Same_type) = X.v (Bytes.create 4)
                                  ^^^^^^^^^
Error: Unbound module type "Same_type"
|}];;

external set32_local_bytes :
  bytes @ local -> int @ [< 'm] -> int32 @ [< 'm] -> unit
  = "%caml_bytes_set32"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(fun (b @ local) -> exclave_ set32_local_bytes b 0);;
[%%expect{|
Line 1, characters 29-46:
1 | (fun (b @ local) -> exclave_ set32_local_bytes b 0);;
                                 ^^^^^^^^^^^^^^^^^
Error: Unbound value "set32_local_bytes"
|}];;

external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let eq_one_arg = eq "a"
[%%expect{|
Line 1, characters 17-19:
1 | let eq_one_arg = eq "a"
                     ^^
Error: Unbound value "eq"
|}];;

module Eq_val : sig val eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let eq_val_one_arg = Eq_val.eq "a"
[%%expect{|
Line 1, characters 21-27:
1 | let eq_val_one_arg = Eq_val.eq "a"
                         ^^^^^^
Error: Unbound module "Eq_val"
|}];;

module Eq_val_close : sig
  val eq : 'a @ [< 'm] -> ('a @ [< 'm] -> bool) @ [> close('m)]
end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Eq_val_shared : sig
  val eq : 'a @ [< 'm] -> ('a @ [< 'm] -> bool) @ [> 'm]
end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

let eq_later_arg_local (y @ local) =
  let p = eq "a" in
  let _ = p y in
  p
[%%expect{|
Line 2, characters 10-12:
2 |   let p = eq "a" in
              ^^
Error: Unbound value "eq"
|}];;

let eq_later_arg_global (y : string) =
  let p = eq "a" in
  let _ = p y in
  p
[%%expect{|
Line 2, characters 10-12:
2 |   let p = eq "a" in
              ^^
Error: Unbound value "eq"
|}];;

(fun (x @ nonportable) -> eq x);;
[%%expect{|
Line 1, characters 26-28:
1 | (fun (x @ nonportable) -> eq x);;
                              ^^
Error: Unbound value "eq"
|}];;

let add_alias = add
[%%expect{|
Line 1, characters 16-19:
1 | let add_alias = add
                    ^^^
Error: Unbound value "add"
|}];;

(add_alias : int32 -> int32 -> int32);;
[%%expect{|
Line 1, characters 1-10:
1 | (add_alias : int32 -> int32 -> int32);;
     ^^^^^^^^^
Error: Unbound value "add_alias"
|}];;

(add_alias : int32 @ local -> int32 @ local -> int32 @ local);;
[%%expect{|
Line 1, characters 1-10:
1 | (add_alias : int32 @ local -> int32 @ local -> int32 @ local);;
     ^^^^^^^^^
Error: Unbound value "add_alias"
|}];;

module type Of_struct = module type of struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
  let add_val = add
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Ext_legacy : sig
  external add : int32 -> int32 -> int32 = "%int32_add"
end = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Ext_local_opt : sig
  external add :
    (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
    = "%int32_add"
end = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Id_local_opt : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(* The implementation is *less* general than the interface *)

(* [@local_opt] varies yielding together on the argument and the result, so the
   implementation must accept a yielding argument *)
module Id_local_opt_unyielding : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm & unyielding] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

(* Similar for forkability *)
module Id_local_opt_forkable : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm & forkable] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Ext_from_legacy : sig
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

module Ext_from_local_opt : sig
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end = struct
  external add :
    (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
    = "%int32_add"
end
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external returns_closure :
  int32 @ [< 'm] -> (int32 @ [< 'm] -> int32 -> int32) = "caml_returns_closure"
[%%expect{|
Uncaught exception: File "typing/typedecl.ml", line 4553, characters 12-18: Assertion failed

|}];;

external takes_callback :
  (int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]) -> unit
  = "caml_takes_callback"
[%%expect{|
external takes_callback :
  (int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]) -> unit
  = "caml_takes_callback"
|}];;
