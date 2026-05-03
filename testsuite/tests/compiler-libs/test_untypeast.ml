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
let foo : ('a : value) . 'a -> 'a = fun x -> x in foo
- : unit = ()
|}];;

run {| let foo : type a . a -> a = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . 'a -> 'a = fun (type a) -> (fun x -> x : a -> a) in
foo
- : unit = ()
|}];;

run {| let foo : ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let (foo : 'a -> 'a) = ((fun x -> x : 'a -> 'a) : @ portable) in foo
- : unit = ()
|}];;

run {| let foo : 'a . ('a -> 'a) @ portable = fun x -> x in foo |}

[%%expect{|
let foo : ('a : value) . ('a -> 'a) @ portable = (fun x -> x : @ portable) in
foo
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
|}]
