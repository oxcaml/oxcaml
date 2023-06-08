(* TEST
   * expect
   flags = "-extension layouts"
*)


type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
;;
[%%expect{|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
|}];;

type t_any : any;;

[%%expect{|
failure
|}]

type t_void : void

[%%expect{|
failure
|}]

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a : value) = 5
let x : int as ('a : immediate) = 5
;;
[%%expect {|
val x : int = 5
val x : int = 5
|}]

let x : int as ('a : any) = 5;;

[%%expect{|
extension
|}]
(* CR layouts: fix when [any] becomes available in [layouts] *)

let x : (int as ('a : immediate)) list as ('b : value) = [3;4;5]
;;
[%%expect {|
val x : int list = [3; 4; 5]
|}]

let x : int list as ('a : immediate) = [3;4;5]
;;
[%%expect {|
Line 1, characters 8-30:
1 | let x : (int list : immediate) = [3;4;5]
            ^^^^^^^^^^^^^^^^^^^^^^
Error: Bad layout annotation:
         int list has layout value, which is not a sublayout of immediate.
|}]

(****************************************)
(* Test 2: Annotation on type parameter *)

type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t = int t2_imm
type t = bool t2_imm
;;
[%%expect {|
type ('a : immediate) t2_imm
type (_ : immediate) t2_imm'
type t = int t2_imm
type t = bool t2_imm
|}]

module M1 : sig
  type ('a : immediate) t
end = struct
  type (_ : immediate) t
end

module M2 : sig
  type (_ : immediate) t
end = struct
  type ('a : immediate) t
end

[%%expect {|
success
|}]

type t = string t2_imm
;;
[%%expect {|
Line 1, characters 9-15:
1 | type t = string t2_imm
             ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}]

let f : 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : immediate) t2_imm -> ('a : value) t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : value) t2_imm -> ('a : value) t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : immediate). 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
val f : ('a : immediate). 'a t2_imm -> 'a t2_imm = <fun>
|}]

let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
;;
[%%expect {|
Line 1, characters 8-44:
1 | let f : ('a : value). 'a t2_imm -> 'a t2_imm = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have
       layout value, but was inferred to have layout immediate.
|}]

type 'a t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

type ('a : value) t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

type ('a : immediate) t = 'a t2_imm
;;
[%%expect {|
type ('a : immediate) t = 'a t2_imm
|}]

let f : (_ : value) t2_imm -> unit = fun _ -> ()
let g : (_ : immediate) t2_imm -> unit = fun _ -> ()

[%%expect {|
success
|}]

let f : (_ : immediate) -> unit = fun _ -> ()
let g : (_ : value) -> unit = fun _ -> ()

[%%expect {|
success
|}]

let f : (_ : immediate) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : immediate) = fun _ -> assert false

[%%expect {|
success
|}]

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any) -> 'a = fun x -> x
;;
[%%expect {|
extension
|}]

let f : ('a : any). 'a -> 'a = fun x -> x
;;
[%%expect {|
extension
|}]
(* CR layouts: fix when [any] becomes available in [layouts] *)

(********************************************)
(* Test 4: Annotation on record field types *)

type r = { field : ('a : immediate). 'a -> 'a }

[%%expect{|
extension
|}]
(* CR layouts: fix when we allow annotations on field types in [layouts] *)

(********************)
(* Test 5: newtypes *)

let f = fun (type (a : value)) (x : a) -> x
;;
[%%expect {|
extension
|}]

let f = fun (type (a : immediate)) (x : a) -> x
;;
[%%expect {|
extension
|}]

let f = fun (type (a : any)) (x : a) -> x
;;
[%%expect {|
extension
|}]
(* CR layouts: fix when we allow annotations on newtypes in [layouts] *)

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value). a -> a = fun x -> x
;;
[%%expect {|
extension
|}]

let f : type (a : immediate). a -> a = fun x -> x
;;
[%%expect {|
extension
|}]

let f : type (a : any). a -> a = fun x -> x
;;
[%%expect {|
extension
|}]
(* CR layouts: fix when we allow annotations on newtypes in [layouts] *)

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

module type S = sig
  val f : 'a. 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
Line 2, characters 10-36:
2 |   val f : 'a. 'a t2_imm -> 'a t2_imm
              ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was defaulted to have
       layout value, but was inferred to have layout immediate.
|}]

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
extension
|}]
(* CR layouts: fix when we allow annotations on universals in [layouts] *)

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm
  val g : ('a : immediate). 'a t2_imm -> 'a t2_imm
end
;;
[%%expect {|
extension
|}]
(* CR layouts: fix when we allow annotations on universals in [layouts] *)

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : immediate). 'a -> 'a) = x "string"

[%%expect {|
extension
|}]
(* CR layouts: fix when we allow annotations on universals in [layouts] *)

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate) (x : a) = x

[%%expect{|
extension
|}]

let f = fun (type a : immediate) (x : a) -> x

[%%expect{|
extension
|}]

let f = fun (type a : value) (x : a) -> x

[%%expect{|
extension
|}]

let o = object
  method m : type (a : immediate). a -> a = fun x -> x
end

[%%expect{|
extension
|}]

let f : type (a : immediate). a -> a = fun x -> x

[%%expect{|
extension
|}]

let f x =
  let local_ g (type a : immediate) (x : a) = x in
  g x [@nontail]

[%%expect{|
extension
|}]

let f = fun x y (type (a : immediate)) (z : a) -> z

[%%expect{|
extension
|}]

let f = fun x y (type a : immediate) (z : a) -> z

[%%expect{|
extension
|}]

external f : ('a : immediate). 'a -> 'a = "%identity"

[%%expect{|
extension
|}]

type (_ : any) t2_any

[%%expect{|
extension
|}]

exception E : ('a : immediate) ('b : any). 'b t2_any * 'a list -> exn

[%%expect{|
extension
|}]

let f (x : ('a : immediate). 'a -> 'a) = x 3, x true

[%%expect {|
extension
|}]

type _ a = Mk : [> ] * ('a : immediate) -> int a

[%%expect {|
extension
|}]

let f_imm : ('a : immediate). 'a -> 'a = fun x -> x

[%%expect {|
extension
|}]

let f_val : ('a : value). 'a -> 'a = fun x -> f_imm x

[%%expect {|
extension
|}]

type (_ : value) g =
  | MkG : ('a : immediate). 'a g

[%%expect {|
extension
|}]

type t = int as (_ : immediate)

[%%expect {|
extension
|}]
