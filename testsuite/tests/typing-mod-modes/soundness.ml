(* TEST
   flags = "-w -220";
   expect;
*)

(* Regression tests for soundness holes found by adversarial testing. *)

let spawn (f : (unit -> unit) @ portable) = f ()
let counter = ref 0
let g () = counter := !counter + 1
[%%expect{|
val spawn : (unit -> unit) @ portable -> unit = <fun>
val counter : int ref = {contents = 0}
val g : unit -> unit = <fun>
|}]

(* Over-application: the intermediate result [id g] is [mod_portable]. *)
let race () =
  spawn (fun () -> let id x = x in id g ())
[%%expect{|
Line 2, characters 35-42:
2 |   spawn (fun () -> let id x = x in id g ())
                                       ^^^^^^^
Error: The expression is "mod_portable" but is expected to be "nonportable".
|}]

let get @ stateless = fun () -> g
let bad @ stateless = fun () -> get () ()
[%%expect{|
val get : unit -> (unit -> unit) @ mod_stateless = <fun>
Line 2, characters 32-35:
2 | let bad @ stateless = fun () -> get () ()
                                    ^^^
Error: The value "get" is "stateful"
       but is expected to be "stateless"
         because it is used inside the function at line 2, characters 22-41
         which is expected to be "stateless".
|}]

(* Forcing a lazy by pattern runs it. *)
type t = { mutable v : int }
let x = { v = 0 }
let l = lazy (x.v <- 1)
let bad @ stateless = fun () -> (fun (lazy ()) -> ()) l
[%%expect{|
type t = { mutable v : int; }
val x : t = {v = 0}
val l : unit lazy_t = <lazy>
Line 4, characters 54-55:
4 | let bad @ stateless = fun () -> (fun (lazy ()) -> ()) l
                                                          ^
Error: The value "l" is "stateful"
       but is expected to be "stateless"
         because it is used inside the function at line 4, characters 22-55
         which is expected to be "stateless".
|}]

(* A class of a module in the mod region. *)
module type S = sig class c : object method m : unit end end
module Impl = struct class c = object method m = counter := !counter + 1 end end
let m = (module Impl : S)
let race () = spawn (fun () -> let module M = (val m) in (new M.c)#m)
[%%expect{|
module type S = sig class c : object method m : unit end end
module Impl : sig class c : object method m : unit end end
val m : (module S) = <module>
Line 4, characters 62-65:
4 | let race () = spawn (fun () -> let module M = (val m) in (new M.c)#m)
                                                                  ^^^
Error: The class "M.c" is "mod_portable" but is expected to be "nonportable".
|}]

let race () =
  spawn (fun () ->
    let module M = (val m) in
    let o = object inherit M.c end in
    o#m)
[%%expect{|
Line 4, characters 27-30:
4 |     let o = object inherit M.c end in
                               ^^^
Error: The class "M.c" is "mod_portable" but is expected to be "nonportable".
|}]
