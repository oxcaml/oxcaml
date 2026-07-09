(* TEST
 native-compiler;
 {
   compiler_directory_suffix = ".structured";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -c";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "e2e_ocaml.structured.table";
   script = "sh ${test_source_directory}/e2e_table.sh \
             ${test_build_directory}/e2e_ocaml.o \
             '(caml|U[0-9]+)(Stdlib|Camlinternal)'";
   script;
   reference = "${test_source_directory}/e2e_ocaml.structured.reference";
   check-program-output;
 }{
   compiler_directory_suffix = ".flat";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme flat -c";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "e2e_ocaml.flat.table";
   script = "sh ${test_source_directory}/e2e_table.sh \
             ${test_build_directory}/e2e_ocaml.o \
             '(caml|U[0-9]+)(Stdlib|Camlinternal)'";
   script;
   reference = "${test_source_directory}/e2e_ocaml.flat.reference";
   check-program-output;
 }
*)

(* Comprehensive exercise of standard OCaml constructs that produce
   distinct path-item kinds in the structured mangling scheme:
   compilation units (U), modules (M), classes (O), named functions
   (F), anonymous functions (L), and identifiers whose names need
   hexadecimal escaping in the encoded form. *)

(* {1 Modules with signatures, abstract types, deep nesting} *)

module Stack : sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a * 'a t) option
end = struct
  type 'a t = 'a list
  let empty = []
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

module Outer = struct
  module Middle = struct
    module Inner = struct
      module Leaf = struct
        let answer = 42
        let greet name = "hello, " ^ name
      end
    end
  end
end

(* {1 Records, variants, and let-punning} *)

type point = { x : int; y : int; z : int }

let make_point x y z = { x; y; z }

let distance_sq { x; y; z } = (x * x) + (y * y) + (z * z)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec tree_size = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + tree_size l + tree_size r

(* {1 Functors (one-arg, multi-arg, with sig constraints)} *)

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type SET = sig
  type elem
  type t
  val empty : t
  val add : elem -> t -> t
  val mem : elem -> t -> bool
end

module Make_set (O : ORDERED) : SET with type elem = O.t = struct
  type elem = O.t
  type t = elem list

  let empty = []

  let rec add x = function
    | [] -> [ x ]
    | y :: ys as ys' ->
      let c = O.compare x y in
      if c = 0 then ys' else if c < 0 then x :: ys' else y :: add x ys

  let rec mem x = function
    | [] -> false
    | y :: ys -> O.compare x y = 0 || mem x ys
end

module Int_order = struct
  type t = int
  let compare : t -> t -> int = Stdlib.compare
end

module Int_set = Make_set (Int_order)

module Make_pair (A : ORDERED) (B : ORDERED) : sig
  type t
  val make : A.t -> B.t -> t
  val swap : t -> B.t * A.t
end = struct
  type t = A.t * B.t
  let make a b = (a, b)
  let swap (a, b) = (b, a)
end

module Pair = Make_pair (Int_order) (Int_order)

(* {1 Anonymous functions interleaved with named functions and functors} *)

module Functor_with_lambdas (M : ORDERED) = struct
  let pairs xs =
    List.map (fun x -> (x, fun y -> M.compare x y)) xs

  let nested xs =
    List.fold_left
      (fun acc x ->
        let inner k =
          (fun y -> M.compare x y + k + acc)
        in
        inner 1 (List.hd xs))
      0 xs
end

module Pair_user = Functor_with_lambdas (Int_order)

(* {1 Local modules and "let open ... in"} *)

let with_local_module xs =
  let module M = struct
    let factor = 3
    let scale x = x * factor
  end in
  List.map M.scale xs

let with_open_in lst =
  let open List in
  map (fun x -> x + 1) (filter (fun x -> x > 0) lst)

(* {1 Custom infix operators and let-operators (require hex escaping)} *)

module Option_monad = struct
  let ( >>= ) m f = match m with None -> None | Some x -> f x
  let ( >>| ) m f = match m with None -> None | Some x -> Some (f x)
  let ( let* ) = ( >>= )
  let ( and* ) a b =
    match a, b with Some x, Some y -> Some (x, y) | _ -> None
  let return x = Some x

  let chain x =
    return x >>= fun y ->
    return (y + 1) >>| fun z -> z * 2

  let with_let_star a b =
    let* x = Some a
    and* y = Some b in
    Some (x + y)
end

(* {1 Extended indexing operators (heavy non-output character escaping)} *)

module Indexing = struct
  let ( .%() ) arr i = Array.get arr i
  let ( .%()<- ) arr i v = Array.set arr i v
  let ( .%[] ) s i = String.get s i
  let ( .%{} ) assoc k = List.assoc k assoc

  let demo () =
    let a = [| 10; 20; 30 |] in
    a.%(0) <- 99;
    let _ = a.%(1) in
    let _ = "abc".%[0] in
    let _ = [ ("k", 1) ].%{"k"} in
    a.%(2)
end

(* {1 Classes (emit the [O] path item)} *)

class point2d x y = object
  val mutable px = x
  val mutable py = y
  method get_x = px
  method get_y = py
  method translate dx dy =
    px <- px + dx;
    py <- py + dy
end

class virtual shape = object
  method virtual area : float
  method describe = Printf.sprintf "area=%.2f" Float.zero
end

class circle r = object
  inherit shape
  method area = 3.14159 *. r *. r
end

(* {1 Deep nesting: module -> named fn -> local module -> named fn} *)

module Deep_nest = struct
  module Layer1 = struct
    let do_thing x =
      let module Local_in_fn = struct
        let[@inline never] helper y = y * 2
        let[@inline never] combine z = helper z + z
      end in
      Local_in_fn.combine x

    module Layer2 = struct
      let down_we_go x =
        (* Local module declared inside a named function inside a
           nested module: exercises [M] / [F] / [S] interleaving in
           the structured path. *)
        let module Way_down = struct
          let[@inline never] bump n = n + 1
          let[@inline never] twice n = bump (bump n)
        end in
        Way_down.twice x
    end
  end
end

(* {1 Alternation of named and anonymous functions} *)

(* Named outer -> anonymous -> named -> anonymous -> named, with
   each layer closing over the previous. This is the corner case the
   structured mangling has the most room to improve on: each [fun]
   produces a separate anonymous-function path-item ([L]), and the
   reference here pins how those currently print. *)
module Alternating = struct
  let[@inline never] outer_named x =
    let[@inline never] middle_named y =
      (fun z ->
         let[@inline never] inner_named w =
           (fun v -> x + y + z + w + v)
         in
         (inner_named z) y)
    in
    (fun y -> middle_named y) x

  (* Same idea but the named/anonymous order is flipped, so the
     mangling alternates [L]-[F]-[L]-[F] instead of [F]-[L]-[F]-[L]. *)
  let[@inline never] anon_first =
    fun x ->
      let[@inline never] named1 y =
        (fun z ->
           let[@inline never] named2 w = x + y + z + w in
           named2 z)
      in
      named1 x
end

(* {1 Force evaluation so the symbols are not stripped} *)

let _ =
  let s = Stack.push 1 (Stack.push 2 Stack.empty) in
  ignore (Stack.pop s);
  ignore (Outer.Middle.Inner.Leaf.greet "world");
  ignore (Outer.Middle.Inner.Leaf.answer);
  let p = make_point 1 2 3 in
  ignore (distance_sq p);
  ignore (tree_size (Node (1, Leaf, Node (2, Leaf, Leaf))));
  let set = Int_set.add 1 (Int_set.add 2 Int_set.empty) in
  ignore (Int_set.mem 1 set);
  ignore (Pair.swap (Pair.make 1 2));
  ignore (Pair_user.pairs [ 1; 2; 3 ]);
  ignore (Pair_user.nested [ 1; 2; 3 ]);
  ignore (with_local_module [ 1; 2; 3 ]);
  ignore (with_open_in [ -1; 2; -3; 4 ]);
  ignore Option_monad.(chain 5);
  ignore Option_monad.(with_let_star 1 2);
  ignore (Indexing.demo ());
  let pt = new point2d 0 0 in
  pt#translate 1 1;
  ignore (pt#get_x + pt#get_y);
  let c = new circle 2.0 in
  ignore c#area;
  ignore (Deep_nest.Layer1.do_thing 7);
  ignore (Deep_nest.Layer1.Layer2.down_we_go 11);
  ignore (Alternating.outer_named 2 3);
  ignore (Alternating.anon_first 4 5)
