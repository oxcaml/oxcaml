(* TEST
 flags = "-extension-universe alpha";
 include stdlib_alpha;
 expect;
*)

(* Typing tests for [Stdlib_alpha.Effect_reflection], whose interface is
   built from module-dependent function types (modular explicits). *)

module ER = Stdlib_alpha.Effect_reflection;;
[%%expect{|
module ER = Stdlib_alpha.Effect_reflection
|}];;

(* The module-dependent types of the core interface. *)

let reify = ER.reify
let perform = ER.perform
let reflect = ER.reflect
let outl = ER.outl;;
[%%expect{|
val reify :
  (module O : ER.Op) -> (ER.Handler(O).t @ local -> 'a) -> 'a ER.Term(O).t =
  <fun>
val perform : (module O : ER.Op) -> 'a O.t -> ER.Handler(O).t @ local -> 'a =
  <fun>
val reflect :
  (module O : ER.Op) -> 'a ER.Term(O).t -> ER.Handler(O).t @ local -> 'a =
  <fun>
val outl :
  (module L : ER.Op) ->
  (module R : ER.Op) ->
  ER.Handler(ER.Sum(L)(R)).t @ local -> ER.Handler(L).t @ local = <fun>
|}];;

module State = struct
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end;;
[%%expect{|
module State : sig type 'a t = Get : int t | Set : int -> unit t end @@
  stateless
|}];;

(* Applying [perform] to a signature and an operation gives a function
   over that signature's handlers. *)

let get (h @ local) = ER.perform (module State) State.Get h
let set (h @ local) i = ER.perform (module State) (State.Set i) h
let perform_get = ER.perform (module State) State.Get;;
[%%expect{|
val get : ER.Handler(State).t @ local -> int = <fun>
val set : ER.Handler(State).t @ local -> int -> unit = <fun>
val perform_get : ER.Handler(State).t @ local -> int = <fun>
|}];;

(* The key safety property: a handler cannot escape the scope of its
   [reify], directly... *)

let escape = ER.reify (module State) (fun h -> h);;
[%%expect{|
Line 1, characters 47-48:
1 | let escape = ER.reify (module State) (fun h -> h);;
                                                   ^
Error: This value is "local" to the parent region but is expected to be "global".
|}];;

(* ...or by being captured in a returned closure. *)

let escape = ER.reify (module State) (fun h -> fun () -> get h);;
[%%expect{|
Line 1, characters 61-62:
1 | let escape = ER.reify (module State) (fun h -> fun () -> get h);;
                                                                 ^
Error: The value "h" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the function at line 1, characters 47-62
         which is expected to be "global".
|}];;

(* Within the handler's scope, performing operations is fine. *)

let term =
  ER.reify (module State) (fun h ->
    let x = get h in
    set h (x + 1));;
[%%expect{|
val term : unit ER.Term(State).t = ER.Term(State).Op (State.Get, <fun>)
|}];;

(* Modules not matching [Op] are rejected. *)

module Not_op = struct
  type t = int
end

let bad = ER.reify (module Not_op) (fun h -> ());;
[%%expect{|
module Not_op : sig type t = int end @@ stateless
Line 5, characters 27-33:
5 | let bad = ER.reify (module Not_op) (fun h -> ());;
                               ^^^^^^
Error: Signature mismatch:
       Modules do not match: sig type t = int end is not included in ER.Op
       Type declarations do not match:
         type t = int
       is not included in
         type 'a t
       They have different arities.
       File "otherlibs/stdlib_alpha/effect_reflection.mli", line 31, characters 2-11:
         Expected declaration
|}];;
