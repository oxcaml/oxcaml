(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let run s =
  let pe = Parse.expression (Lexing.from_string s) in
  let te = Typecore.type_expression (Lazy.force Env.initial) pe in
  let ute = Untypeast.untype_expression te in
  Format.printf "%a@." Pprintast.expression ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

let run_structure s =
  let structure = Parse.implementation (Lexing.from_string s) in
  let typed_structure, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) structure
  in
  let structure = Untypeast.untype_structure typed_structure in
  Format.printf "%a@." Pprintast.structure structure
;;

[%%expect{|
val run_structure : string -> unit = <fun>
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

run {| let open struct type t = { mutable x : int [@atomic] } end in
       let _ = fun (v : t) -> v.x in () |};;

[%%expect{|
let open struct type t = {
                  mutable x: int [@atomic ]} end in
  let _ = fun (v : t) -> v.x in ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

run {| match None with Some (Some _) -> () | _ -> () |};;

[%%expect{|
match None with | Some (Some _) -> () | _ -> ()
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast maintain the arity of a function. *)

(* 4-ary function *)
run {| fun x y z -> function w -> x y z w |};;

[%%expect{|
fun x y z -> function | w -> x y z w
- : unit = ()
|}];;

(* 3-ary function returning a 1-ary function *)
run {| fun x y z -> (function w -> x y z w) |};;

[%%expect{|
fun x y z -> (function | w -> x y z w)
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle value binding type annotations. *)

run {| let foo : 'a. 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : 'a . 'a -> 'a = fun (type a) -> (fun x -> x : a -> a) in foo
- : unit = ()
|}];;

run {| let foo : ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let foo : ('a -> 'a) @ portable = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo
- : unit = ()
|}];;

run {|
  let module M = struct type t = { x : int } end in
  fun x -> let M.{ x } = M.{ x } in x
|}

[%%expect{|
let module M = struct type t = {
                        x: int } end in
  fun x -> let M.{ x }  = let open M in { x } in x
- : unit = ()
|}];;

run {| let foo : 'a -> 'a = fun x -> x in foo |}

[%%expect{|
let (foo : 'a -> 'a) = (fun x -> x : 'a -> 'a) in foo
- : unit = ()
|}];;

let run s =
  let pe = Parse.implementation (Lexing.from_string s) in
  let te,_,_,_,_,_ = Typemod.type_structure (Lazy.force Env.initial) pe in
  let ute = Untypeast.untype_structure te in
  Format.printf "%a@." Pprintast.structure ute
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

(* That test would hang before ocaml/ocaml#14105 *)
run {|type t = (::);; let f (x : t) = match x with (::) -> 4|}

[%%expect{|
type t =
  | (::)
let f (x : t) = match x with | (::) -> 4
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle declaration modalities. *)

run_structure {|
  module State = struct
    type t = int
    external next : t -> t @@ portable = "%identity"
  end |};;

[%%expect{|
module State =
  struct type t = int
         external next : t -> t @@ portable = "%identity" end
- : unit = ()
|}];;

run_structure {|
  module type S = sig
    val x : int -> int @@ portable
  end |};;

[%%expect{|
module type S  = sig val x : int -> int @@ portable end
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle modes on let bindings and functions. *)

run {| fun y ->
       let (x @ local) = Some y in match x with Some _ -> () | None -> () |};;

[%%expect{|
fun y -> let (x @ local) = Some y in match x with | Some _ -> () | None -> ()
- : unit = ()
|}];;

run {| let (f @ local) x = x in f 1 |};;

[%%expect{|
let (f @ local) x = x in f 1
- : unit = ()
|}];;

run {| let f x @ local = exclave_ Some x in f |};;

[%%expect{|
let f x  @ local= exclave_ Some x in f
- : unit = ()
|}];;

run {| let (f @ local) x : int -> int -> int = fun _ _ -> x in f 1 2 3 |};;

[%%expect{|
let (f @ local) x  : int -> int -> int = fun _ _ -> x in f 1 2 3
- : unit = ()
|}];;

run {| let f : (int -> int -> int) @ local = fun _ z -> z in f 1 2 |};;

[%%expect{|
let f : (int -> int -> int) @ local = fun _ z -> z in f 1 2
- : unit = ()
|}];;

run {| let f (type a) (x : a) : a option @ local = exclave_ Some x in f |};;

[%%expect{|
let f (type a) (x : a)  : a option @ local = exclave_ Some x in f
- : unit = ()
|}];;

run_structure {| type t = int -> (int -> int -> int) @ local |};;

[%%expect{|
type t = int -> (int -> int -> int) @ local
- : unit = ()
|}];;

run_structure {| module type S = sig module M : sig end @@ portable end |};;

[%%expect{|
module type S  = sig module M : sig  end @@ portable end
- : unit = ()
|}];;

run_structure {|
  let f () = let mutable g = fun x -> x in g <- (fun x -> x); g 1 |};;

[%%expect{|
let f () = let mutable g = fun x -> x in g <- (fun x -> x); g 1
- : unit = ()
|}];;

(***********************************)
(* Untypeast/pprintast correctly handle constructs elaborated by the
   type-checker. *)

run_structure {|
  let ( let+ ) x f = f x
  let ( and+ ) x y = (x, y)
  let res = let+ x = 1 and+ y = 2 and+ z = 3 in [x; y; z] |};;

[%%expect{|
let (let+) x f = f x
let (and+) x y = (x, y)
let res = let+ x = 1
          and+ y = 2
          and+ z = 3 in [x; y; z]
- : unit = ()
|}];;

run_structure {| type t = private [> `A ] |};;

[%%expect{|
type t = private [> `A ]
- : unit = ()
|}];;

run_structure {| class c ?(x = 1) () = object method x = x end |};;

[%%expect{|
class c ?(x= 1) () = object method x = x end
- : unit = ()
|}];;

run_structure {|
  let f ~(here : [%call_pos]) () = here
  let g () = f () |};;

[%%expect{|
let f ~here:(here : [%call_pos ]) () = here
let g () = f ~here:([%src_pos ] : [%call_pos ]) ()
- : unit = ()
|}];;

run_structure {| let f (g : ?b:bool -> unit -> int) = (g : unit -> int) |};;

[%%expect{|
let f (g : ?b:bool -> unit -> int) = (g : unit -> int)
- : unit = ()
|}];;

run_structure {| type 'a t = 'a constraint 'a = [< `A of & int ] |};;

[%%expect{|
type 'a t = 'a constraint 'a = [< `A of & int ]
- : unit = ()
|}];;

run_structure {| type 'a t = int -> (int as 'a) |};;

[%%expect{|
type 'a t = int -> (int as 'a)
- : unit = ()
|}]
