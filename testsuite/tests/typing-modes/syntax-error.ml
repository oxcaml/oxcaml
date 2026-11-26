(* TEST
 toplevel;
*)

let foo : string @ = "hello";;

let foo @ = "hello";;

let foo : ('a. 'a -> 'a) @  = fun x -> x;;

let foo : type a. (a -> a) @  = fun x -> x;;

let (x, y) @ = "hello", "world";;

let (x, y) : _ @ = "hello", "world";;

let foo @ = "hello";;

let foo = ("hello" : _ @ );;

let foo = ("hello" @ );;

let foo ~bar = bar ^ "hello";;

let x =
  let bar = "world" in
  foo ~(bar : _ @ )
;;

let x =
  let bar = "world" in
  foo ~(bar @ )
;;

type r = {a : string; b : string};;

let r = {a : _ @ = "hello";
          b : _ @ = "world"}
;;

let r =
  {a @  = "hello";
   b @  = "world"}
;;

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar:_@), ~(biz:_@)
;;

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar @ ), ~(biz @ )
;;

type r = string @  -> string @ ;;

type r = string * y:string @  -> string * w:string @;;

type r = x:string * y:string @  -> string * w:string @;;

type r = string @ -> string;;

type t = Foo of string @@ * string;;

type r = {
  x : string @@
}
;;

let foo ?(x @ = 42) () = () ;;

let foo ?(x : _ @ = 42) () = ();;

let foo ?(x : ('a. 'a -> 'a) @ ) = ();;

let foo ?x:((x,y) @ = (42, 42)) () = ();;

let foo ?x:((x,y) : _ @ = (42, 42)) () = ();;

let foo ?x:((x,y) : ('a.'a->'a) @ ) () = ();;

let foo ((x @ ), (y@)) = x + y ;;

let foo ((x : _ @ ), (y : _ @ )) = x + y;;

let foo () =
  let (bar @ ) a b = () in
  bar 42 24;
  ()
;;

let foo ((x @ unique once), (y@local unique)) = x + y;;

let foo ((x : _ @ unique once), (y : _ @ local unique)) = x + y;;

module type S = sig
  module M : S @ portable
end;;

module type S = sig
  module F : S @@ portable -> S @@ portable
end;;

let foo = ("bar" :> int @ local);;

(* Removed legacy local_ and global_ syntax *)
let local_ x = "hello";;

let f (local_ x) = x;;

let f x = local_ x;;

type t = local_ int -> int;;

type r = { global_ x : int };;

type v = Foo of global_ int;;
