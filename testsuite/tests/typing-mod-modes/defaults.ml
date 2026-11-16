(* TEST
   flags = "-w -220";
   expect;
*)

(* Defaults: a variable is in the mod region only when no old-region value is
   feasible; programs accepted by the old checker keep their old modes. *)

let f () = let r = ref 0 in fun () -> incr r
[%%expect{|
val f : unit -> unit -> unit = <fun>
|}]

let f () = let g = fun () -> () in let h @ portable = fun () -> g () in (g, h)
[%%expect{|
val f : unit -> (unit -> unit) * (unit -> unit) = <fun>
|}]

(* [g] is only named inside [h]: old default [portable] is kept for [g]. *)
let f () =
  let g = fun () -> () in
  let h @ portable = fun () -> let _ = g in () in
  (g, h)
[%%expect{|
val f : unit -> (unit -> unit) * (unit -> unit) = <fun>
|}]

let x = None
[%%expect{|
val x : ('a : any). 'a option = None
|}]

let x = []
[%%expect{|
val x : ('a : any). 'a list = []
|}]

module M = struct
  let r = ref 0
  let g () = incr r
  let h @ portable = fun () -> g
end
[%%expect{|
module M :
  sig
    val r : int ref
    val g : unit -> unit
    val h : unit -> (unit -> unit) @ mod_portable
  end
|}]

module M = struct
  let r = ref 0
  let g () = incr r
  let h = fun () -> g ()
end
[%%expect{|
module M : sig val r : int ref val g : unit -> unit val h : unit -> unit end
|}]

let id x = x
let app f x = f x
let compose f g x = f (g x)
[%%expect{|
val id : 'a -> 'a = <fun>
val app : ('a -> 'b) -> 'a -> 'b = <fun>
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>
|}]

(* The parameter defaults to [portable], as in the old checker. *)
let f (g : unit -> unit) =
  let h @ portable = fun () -> let k = g in k in
  h
[%%expect{|
val f : (unit -> unit) @ portable -> unit -> unit -> unit = <fun>
|}]

(* A let-bound illegally captured closure stays [mod_portable]. *)
let f () =
  let r = ref 0 in
  let g () = incr r in
  let h @ portable = fun () -> let k = g in k in
  h
[%%expect{|
val f : unit -> unit -> (unit -> unit) @ mod_portable = <fun>
|}]
