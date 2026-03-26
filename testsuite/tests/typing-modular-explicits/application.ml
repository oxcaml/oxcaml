(* TEST
 flags = "-extension modular_explicits";
 expect;
*)

(* Application warnings for module-dependent functions, from
   typing-warnings/application.ml (ocaml/ocaml#14667), which runs
   without the modular_explicits extension. *)
[@@@warning "+20"]

(* Simple cases *)
let raise_late _ _ = assert false
let f id x = id x
[%%expect {|
val raise_late : 'a -> 'b -> 'c = <fun>
val f : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let work x y z = raise_late x y z
[%%expect {|
Line 1, characters 32-33:
1 | let work x y z = raise_late x y z
                                    ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val work : 'a -> 'b -> 'c -> 'd = <fun>
|}]

let fail x y z = f (raise_late x) y z
[%%expect {|
val fail : 'a -> 'b -> 'c -> 'd = <fun>
|}]

(* GADTS *)

type _ t = F: (int -> string) t
let print (type a) (x: a t): a = match x with
  | F -> string_of_int
[%%expect {|
type _ t = F : (int -> string) t
val print : 'a t -> 'a = <fun>
|}]

let s = print F 0
[%%expect {|
val s : string = "0"
|}]

(* Modular explicits *)

module type T = sig type 'a t end
let f (module _:T) _ = assert false
[%%expect {|
module type T = sig type 'a t end
val f : (module T) -> 'a -> 'b = <fun>
|}]


let ok = f (module List) () ()
[%%expect {|
Line 1, characters 28-30:
1 | let ok = f (module List) () ()
                                ^^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

Exception: Assert_failure ("", 2, 23).
|}]

let g (module M:T) (x:_ M.t) = x
module Raise = struct
  type 'a t = unit -> 'a
end
[%%expect {|
val g : (module M : T) -> 'a M.t -> 'a M.t = <fun>
module Raise : sig type 'a t = unit -> 'a end
|}]

let fail = g (module Raise) (fun () -> assert false) () ()
[%%expect {|
Exception: Assert_failure ("", 1, 39).
|}]


module type Arrow = sig type ('a,'b) t = 'a -> 'b end
module Arrow = struct type ('a,'b) t = 'a -> 'b end
[%%expect {|
module type Arrow = sig type ('a, 'b) t = 'a -> 'b end
module Arrow : sig type ('a, 'b) t = 'a -> 'b end
|}]

let f (module M:Arrow): ('a,'a) M.t = fun x -> x
let ok x y = f (module Arrow) x y
[%%expect {|
val f : (module M : Arrow) -> ('a, 'a) M.t = <fun>
val ok : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let f (module M:Arrow): ('a,'b) M.t = fun _ -> assert false
let ok_error x y = f (module Arrow) x y
[%%expect {|
val f : (module M : Arrow) -> ('a, 'b) M.t = <fun>
Line 2, characters 38-39:
2 | let ok_error x y = f (module Arrow) x y
                                          ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val ok_error : 'a -> 'b -> 'c = <fun>
|}]

module type T = sig type t val x:t end
let f (type a) (module M: T with type t = a) = M.x
let no_warning x y = f x y
[%%expect {|
module type T = sig type t val x : t end
val f : (module M : T with type t = 'a) -> M.t = <fun>
val no_warning : (module T with type t = 'a -> 'b) -> 'a -> 'b = <fun>
|}]

(* GADTS + modular explicits *)

module type Show = sig
  type t
  val show: t -> string
end
type (_,_) t = S: ('x, 'x -> string) t

module I = struct type t = int let show = string_of_int end

let f: type a. (module M:Show) -> (M.t,a) t -> a =
  fun (module M:Show) x -> match x with
    | S -> M.show
[%%expect {|
module type Show = sig type t val show : t -> string end
type (_, _) t = S : ('x, 'x -> string) t
module I : sig type t = int val show : int -> string end
val f : (module M : Show) -> (M.t, 'a) t -> 'a = <fun>
|}]

let s = f (module I) S 0
[%%expect {|
val s : string = "0"
|}]
