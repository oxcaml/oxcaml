(* TEST
 toplevel;
*)

(* This file contains some syntax errors for mod crossings, as well as
   documentation for behavior which is not currently supported. *)

(* mostly from [typing-modes/syntax-error.ml] *)

let foo : type a. (a -> a) mod  = fun x -> x;;

let (x, y) mod = "hello", "world";;

let (x, y) : _ mod = "hello", "world";;

let foo mod = "hello";;

let foo = ("hello" : _ mod );;

let foo = ("hello" mod );;

let foo ~bar = bar ^ "hello";;

let x =
  let bar = "world" in
  (* this error message doesn't say that a crossing expression is expected *)
  foo ~(bar : _ mod )
;;

let x =
  let bar = "world" in
  foo ~(bar mod )
;;

type r = {a : string; b : string};;

(* this error is slightly different than in [typing-modes/syntax-error.ml] version *)
let r = {a : _ mod = "hello";
         b : _ mod = "world"}
;;

let r =
  {a mod  = "hello";
   b mod  = "world"}
;;

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar mod ), ~(biz mod )
;;

type r = {
  x : string mod
}
;;

let foo ((x mod ), (y@)) = x + y ;;

let foo ((x : _ mod ), (y : _ mod )) = x + y;;

let foo () =
  let (bar mod ) a b = () in
  bar 42 24;
  ()
;;

let foo = ("bar" :> int mod contended);;

(* currently, mod crossings can appear by some arrows, but will _not_ parse properly.
   specifically,
      [val v : int -> int mod contended aliased @@ once portable]
   currently parses as
       [val v : (int -> int) mod contended aliased @@ once portable]
   instead of
       [val v : int -> (int mod contended aliased @@ once portable)]

   it should parse in the second way, since parentheses are not allowed to appear in
   that position, whereas they are allowed to appear for the first case.
*)

(* more examples with arrow types:

module type S = sig
  val v : int -> int mod contended aliased
  val v : int -> int @@ once portable mod contended aliased

  external ex : int -> int mod contended aliased = "foo3"
  external ex : int -> int @@ once portable mod contended aliased = "foo4"

  type t = int mod contended aliased -> int
  type 'a t = 'a -> 'a mod contended aliased @@ once portable
  type fn = int @ portable contended mod contended aliased -> local_ int @ portable contended;;
end

module M = struct
  let v : int -> int mod contended aliased = fun _ -> 42
  let v : int -> int @ once portable mod contended aliased = fun _ -> 42
  let v : int mod contended aliased -> int = fun _ -> 42

  let f ?(x : ('a : any) 'b . 'a -> 'b @ once portable mod contended aliased = assert false)
      ?x:(local_ (y, z) : ('a : any) 'b . 'a -> 'b @ once portable mod contended aliased = 42)
      ~(x : ('a : any) 'b . 'a -> 'b @ once portable mod contended aliased)
      ~x:((y, z) : ('a : any) 'b . 'a -> 'b @ once portable mod contended aliased)
      ((y, z) : ('a : any) 'b . 'a -> 'b @ once portable mod contended aliased)

  let g () =
    let a : ('a : any) 'b. 'a -> 'a @ once portable mod contended aliased = 42 in
    let a : type (a : any) b. a -> b @ once portable mod contended aliased = 42 in
    let (a, b) : int -> int @ once portable mod contended aliased = 42 in
  ()
end
*)

(* mod crossings are not supported with modules / functors / etc, both
   in mode positions and in modality positions. these do not parse and
   are not currently intended to be supported *)

(* in addition to the mod modes on arrow types, there are some more odd consequences
   of this choice of syntax, which are noted here for examination:

  mod modalities interaction with with-bounds: parses as mod jkind_annotation

  type 'a t : immutable_data with 'a mod contended
  type 'a t : immutable_data with 'a -> 'a mod contended

  kind_of_ similarly has weird precedence-related cases.

  type 'a t : kind_of_ 'a mod global
  (* these two cases parse identically *)
  type 'a t : kind_of_ 'a -> 'a mod global
  type 'a t : kind_of_ ('a -> 'a) mod global
*)

(* it is worthwhile to also test...
    - that comments work; specifically, that doc comments are attached to the
      right thing after parsing
    - that attributes work too
*)

(* currently, mod crossings can coexist with legacy modes, but this is not
   explicitly intended to be supported. I believe that these will parse but not
   pretty-print in the expected way. Examples are included below:

let f ?(local_ x @ once portable mod contended aliased = 42) = ()

type record =
  { global_ field1 : int mod contended aliased
  ; global_ field5 : int @@ portable contended mod contended aliased
  };;

type 'a parameterized_record =
  { global_ field1 : 'a mod contended aliased
  ; global_ field5 : 'a @@ portable contended mod contended aliased
  };;

(* there are more interesting combinations here too *)
type t =
  | Foo of global_ int mod contended * int
  | Foo1 of global_ int @@ portable contended mod contended * int

let g () =
  let local_ f a b @ once portable mod contended aliased = 42 in
  let local_ a : (int -> int) @ once portable mod contended aliased = 42 in
  let local_ a : ('a : any) 'b. ('a -> 'a) @ once portable mod contended aliased = 42 in
  ()
*)
