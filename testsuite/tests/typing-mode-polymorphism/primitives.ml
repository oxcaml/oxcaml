(* TEST
 flags = "-dlambda -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* This is fine *)
external magic : 'a @ [< 'm] -> 'b @ [> 'm] = "%identity"
[%%expect{|
0
external magic : 'a @ [< 'm] -> 'b @ [> 'm] = "%identity"
|}];;

(magic : 'a -> 'a);;
[%%expect{|
(function {nlocal = 0} prim/0 stub prim/0)
- : 'a -> 'a = <fun>
|}];;

(magic : 'a @ local -> 'a @ local);;
[%%expect{|
(function {nlocal = 1} prim/1[L] stub : stack prim/1)
- : 'a @ local -> 'a @ local = <fun>
|}];;

(magic : 'a @ unique -> 'a @ unique);;
[%%expect{|
(function {nlocal = 0} prim/2 stub prim/2)
- : 'a @ unique -> 'a @ unique = <fun>
|}];;

(fun x -> magic x : 'a -> 'a);;
[%%expect{|
(function {nlocal = 0} x/0 x/0)
- : 'a -> 'a = <fun>
|}];;

(fun x -> magic x : 'a @ unique -> 'a @ unique);;
[%%expect{|
(function {nlocal = 0} x/1 x/1)
- : 'a @ unique -> 'a @ unique = <fun>
|}];;

(fun x -> exclave_ magic x : 'a @ local -> 'a @ local);;
[%%expect{|
(function {nlocal = 1} x/2[L] : stack x/2)
- : 'a @ local -> 'a @ local = <fun>
|}];;

(fun x -> magic x : 'a @ yielding -> 'a @ yielding);;
[%%expect{|
(function {nlocal = 0} x/3 x/3)
- : 'a @ yielding -> 'a @ yielding = <fun>
|}];;

(* This should emit [caml_modify_local] *)
external setfield : 'a ref @ [< past('m)] -> 'a -> unit = "%setfield0"
[%%expect{|
0
external setfield : 'a ref @ 'm -> 'a -> unit = "%setfield0"
|}];;

(fun x y -> setfield x y : 'a ref -> 'a -> unit);;
[%%expect{|
(function {nlocal = 0} x/4 y/0 : int (setfield_ptr(maybe-stack) 0 x/4 y/0))
- : 'a ref -> 'a -> unit = <fun>
|}];;

(fun x y -> setfield x y : 'a ref @ local -> 'a -> unit);;
[%%expect{|
(function {nlocal = 2} x/5[L] y/1 : int
  (setfield_ptr(maybe-stack) 0 x/5 y/1))
- : 'a ref @ local -> 'a -> unit = <fun>
|}];;

(setfield : 'a ref -> ('a -> unit) @ local);;
[%%expect{|
(function {nlocal = 0} prim/3 prim/4 stub : int
  (setfield_ptr(maybe-stack) 0 prim/3 prim/4))
- : 'a ref -> ('a -> unit) @ local = <fun>
|}];;

(setfield : 'a ref @ local -> 'a -> unit);;
[%%expect{|
(function {nlocal = 2} prim/5[L] prim/6 stub : int
  (setfield_ptr(maybe-stack) 0 prim/5 prim/6))
- : 'a ref @ local -> 'a -> unit = <fun>
|}];;

external int32_neg : int32 @ [< 'm] -> int32 @ [> 'm] = "%int32_neg"
[%%expect{|
0
external int32_neg : int32 @ [< 'm] -> int32 @ [> 'm] = "%int32_neg"
|}];;

(fun x -> int32_neg x)
[%%expect{|
(function {nlocal = 0} x/6[value<int32>] : int32 (%int32_neg x/6))
- : int32 @ [< global] -> int32 @ [> dynamic] = <fun>
|}];;

(fun (x @ local) -> int32_neg x)
[%%expect{|
Line 1, characters 20-31:
1 | (fun (x @ local) -> int32_neg x)
                        ^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}];;

(fun x -> exclave_ int32_neg x)
[%%expect{|
(function {nlocal = 1} x/7[L][value<int32>] : stackint32 (%int32_neg[L] x/7))
- : int32 @ 'm -> int32 @ [> local dynamic] = <fun>
|}];;

(* Each use of a mode-polymorphic external gets its own instance of the mode
   variables, and applying one gives a mode-polymorphic function. *)
external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
let id_unique () = (id : 'a @ unique -> 'a @ unique)
let id_local () = (id : 'a @ local -> 'a @ local)
let apply_id x = id x
[%%expect{|
0
external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
(let
  (id_unique/0 =
     (function {nlocal = 1} param/0[L][value<int>]
       (function {nlocal = 0} prim/7 stub prim/7)))
  (apply (field_imm 1 (global Toploop!)) "id_unique" id_unique/0))
val id_unique : unit @ 'm -> ('a @ unique -> 'a @ unique) @ [> stateful] =
  <fun>
(let
  (id_local/0 =
     (function {nlocal = 1} param/1[L][value<int>]
       (function {nlocal = 1} prim/8[L] stub : stack prim/8)))
  (apply (field_imm 1 (global Toploop!)) "id_local" id_local/0))
val id_local : unit @ 'm -> ('a @ local -> 'a @ local) @ [> stateful] = <fun>
(let (apply_id/0 = (function {nlocal = 0} x/8 x/8))
  (apply (field_imm 1 (global Toploop!)) "apply_id" apply_id/0))
val apply_id : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}];;
