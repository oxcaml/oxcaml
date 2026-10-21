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

(* {1 Ordinals of anonymous functions, modules and lazy expressions}

   Anonymous items are numbered among their siblings of the same kind
   directly under the nearest scope, in the order the compiler translates
   them, which is not always source order: the arguments of an application
   are visited in the order of the callee's parameters (in
   [labelled_out_of_order], the closure passed to [~first] is [fn_{0}]
   even though it is written second), and the body of a [let], the
   second half of a sequence or the [else] branch of a conditional is
   translated before what precedes it. The [@cold] local functions
   inside the closures survive as their own symbols and make the
   numbering visible: [labelled_out_of_order.fn_{0}.in_first]. Each kind
   has its own counter: in [lazy_then_lambda], the thunk is [lazy_{0}]
   and the lambda is [fn_{0}], not [fn_{1}]. *)
module Ordinals = struct
  let[@inline never] siblings_and_nesting xs =
    List.map
      (fun[@inline never] x ->
        List.iter (fun[@inline never] y -> ignore (x + y)) xs;
        x)
      (List.filter (fun[@inline never] z -> z > 0) xs)

  let[@inline never] apply_labelled ~first ~second x = first (second x)

  let[@inline never] labelled_out_of_order x =
    apply_labelled
      ~second:(fun[@inline never] a ->
        let[@cold] in_second () = a * 2 in
        in_second ())
      ~first:(fun[@inline never] b ->
        let[@cold] in_first () = b + 1 in
        in_first ())
      x

  let thunk = lazy (Sys.opaque_identity 41 + 1)

  let[@inline never] lazy_then_lambda xs =
    let t = lazy (Sys.opaque_identity 1) in
    List.map (fun[@inline never] x -> x + Lazy.force t) xs
end

(* {1 Anonymous first-class modules}

   The two [(module struct ... end)] bodies are anonymous modules, numbered
   among the anonymous items of [pick]: [pick.mod_{0}.v] and
   [pick.mod_{1}.v]. *)
module type V = sig
  val v : int -> int
end

let pick flag : (module V) =
  if flag
  then
    (module struct
      let[@cold] v x = x + 1
    end)
  else
    (module struct
      let[@cold] v x = x - 1
    end)

(* {1 Partial application}

   A partial application that the frontend compiles to a closure (a
   later labelled argument is supplied while an earlier one is omitted)
   is marked [partial{<callee>}], with the callee's path as written at
   the application site. Merely using a primitive as a first-class value
   ([let neg = ( ~- )]) is an eta-expansion, not a partial application,
   so it gets no marker: the wrapper simply is [neg]. The middle end
   marks the wrappers it creates for the partial applications it
   compiles itself ([curried]) in the same way. *)
module Partial = struct
  let[@inline never] labelled ~a ~b = a - b

  let[@inline never] sum3 ~a ~b ~c = a + b + c

  let[@inline never] text ?(indent = 0) ~wrap s =
    String.make indent ' ' ^ s ^ string_of_int wrap

  let[@inline never] omit_first x = labelled ~b:x

  let neg = ( ~- )
end

let[@inline never] omit_first_qualified x = Partial.labelled ~b:x

let curried = Partial.labelled ~a:(Sys.opaque_identity 1)

(* Partially applying a partial application applies the original callee
   to more arguments: the wrapper is [partial{Partial.sum3}], not
   [partial{first.partial{Partial.sum3}}]. *)
let first = Partial.sum3 ~a:(Sys.opaque_identity 1)

let second = first ~b:(Sys.opaque_identity 2)

(* Applying a function with optional arguments to a labelled argument only
   makes the type checker eta-expand the application; the resulting
   closure is the partial application too: [render.partial{Partial.text}]. *)
let[@inline never] render l = List.map (Partial.text ~wrap:80) l

(* The eta-expansion of a complete application whose result happens to be a
   function with optional arguments is a partial application of that result,
   not of the function applied, which is left unknown: [use_app.partial{?}]
   rather than [use_app.partial{Stdlib.Sys.opaque_identity}]. *)
let[@inline never] with_opt ?(opt = 0) b = opt + b

let[@inline never] apply_int (g : int -> int) = g 2

let[@inline never] use_app () = apply_int (Sys.opaque_identity with_opt)

(* {1 Primitives re-exported as values}

   When a signature declares as a [val] what the structure defines as an
   [external], the compiler wraps the primitive in a closure to fill the
   module field. The wrapper is named after the value it implements
   ([Prims.<], [Prims.compare]), not after the primitive. *)
module Prims : sig
  val ( < ) : int -> int -> bool

  val compare : int -> int -> int
end = struct
  external ( < ) : int -> int -> bool = "%lessthan"

  external compare : int -> int -> int = "%compare"
end

(* {1 A named function defined under a capturing [let]}

   [let f = let x = ... in let f ... = ... x ... in f] is an idiom for
   sharing a value between calls while keeping a named symbol. The inner
   function keeps its name, [Captured.find_exn.find_exn], even though its
   binding is the whole body of the outer one. *)
module Captured = struct
  let find_exn =
    let not_found = Not_found in
    let[@inline never] find_exn t ~f =
      match List.find_opt f t with None -> raise not_found | Some x -> x
    in
    find_exn
end

(* {1 A functor coerced by its signature}

   When a signature hides items of a functor's body, the compiler wraps
   the functor in a closure that applies the coercion to its result. The
   wrapper is named after the functor with the [coerce] marker,
   [Coerced.Make.{coerce}], and the functor's own body is [Coerced.Make];
   the latter is how every functor body is named. *)
module Coerced : sig
  module type S = sig
    val x : int
  end

  module Make (X : S) : sig
    val get : unit -> int
  end
end = struct
  module type S = sig
    val x : int
  end

  module Make (X : S) = struct
    let[@inline never] helper () = X.x * 2

    let[@inline never] get () = helper () + 1
  end
end

module Coerced_inst = Coerced.Make (struct
  let x = 3
end)

(* The same, one module deeper: the coercion of [Nested] applies a nested
   coercion to [Inner], whose functor is named [Nested.Inner.Make.{coerce}]. *)
module Nested : sig
  module Inner : sig
    module Make (X : Coerced.S) : sig
      val get : unit -> int
    end
  end
end = struct
  module Inner = struct
    module Make (X : Coerced.S) = struct
      let[@inline never] helper () = X.x * 3

      let[@inline never] get () = helper () + 1
    end
  end
end

module Nested_inst = Nested.Inner.Make (struct
  let x = 4
end)

(* {1 A primitive used as a value}

   Passing a primitive such as [Fun.id] as a value eta-expands it into a
   closure. Every such closure of a primitive does the same thing, so it is
   named after the primitive's declaration rather than the use site:
   [First_class.bump] below, and [Stdlib.Fun.id] for [use_id], a copy from
   [Stdlib__Fun] shown at its definition site (the table excludes
   stdlib-rooted symbols, so that one is only visible in the object; see
   [e2e_inline.ml] for the cross-unit case). *)
module First_class = struct
  let[@inline never] keep (f : int -> int) = Sys.opaque_identity f

  let[@inline never] use_id () = keep Fun.id

  (* A C external used as a value: its closure is named after the external
     as declared, [First_class.bump], not after the C symbol or the use
     site, like the field a signature coercion makes of it. *)
  external bump : int -> int = "caml_e2e_bump_stub"

  let[@inline never] use_bump () = keep bump
end

(* {1 Compiler-generated variants of a function}

   A function with optional arguments is split into a wrapper filling in
   the defaults, which keeps the name, and a body marked [{inner}]:
   [Generated.escape.{inner}]. A [[@tail_mod_cons]] function gets a
   destination-passing version marked [{dps}]:
   [Generated.map.map_loop.{dps}]. *)
module Generated = struct
  let[@inline never] escape ?(quote = '"') ?(sep = ',') s =
    if String.contains s sep
    then String.make 1 quote ^ s ^ String.make 1 quote
    else s

  let[@inline never] map l ~f =
    let[@tail_mod_cons] rec map_loop = function
      | [] -> []
      | x :: l -> f x :: map_loop l
    in
    map_loop l
end

(* {1 Arity chunks and class parameters}

   A native function with more than 126 parameters is split into chunks
   of at most that many; the inner chunks are marked [{curry}]:
   [Wide.wide.{curry}]. The closure taking a class's parameters creates
   its objects and is marked [{obj_init}] like the parameterless case:
   [reader.{obj_init}]. *)
module Wide = struct
  let[@inline never] wide ~a0 ~a1 ~a2 ~a3 ~a4 ~a5 ~a6 ~a7 ~a8 ~a9 ~a10 ~a11
        ~a12 ~a13 ~a14 ~a15 ~a16 ~a17 ~a18 ~a19 ~a20 ~a21 ~a22 ~a23 ~a24 ~a25
        ~a26 ~a27 ~a28 ~a29 ~a30 ~a31 ~a32 ~a33 ~a34 ~a35 ~a36 ~a37 ~a38 ~a39
        ~a40 ~a41 ~a42 ~a43 ~a44 ~a45 ~a46 ~a47 ~a48 ~a49 ~a50 ~a51 ~a52 ~a53
        ~a54 ~a55 ~a56 ~a57 ~a58 ~a59 ~a60 ~a61 ~a62 ~a63 ~a64 ~a65 ~a66 ~a67
        ~a68 ~a69 ~a70 ~a71 ~a72 ~a73 ~a74 ~a75 ~a76 ~a77 ~a78 ~a79 ~a80 ~a81
        ~a82 ~a83 ~a84 ~a85 ~a86 ~a87 ~a88 ~a89 ~a90 ~a91 ~a92 ~a93 ~a94 ~a95
        ~a96 ~a97 ~a98 ~a99 ~a100 ~a101 ~a102 ~a103 ~a104 ~a105 ~a106 ~a107
        ~a108 ~a109 ~a110 ~a111 ~a112 ~a113 ~a114 ~a115 ~a116 ~a117 ~a118
        ~a119 ~a120 ~a121 ~a122 ~a123 ~a124 ~a125 ~a126 ~a127 ~a128 ~a129 =
      a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 +
      a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24
      + a25 + a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 +
      a36 + a37 + a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47
      + a48 + a49 + a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 +
      a59 + a60 + a61 + a62 + a63 + a64 + a65 + a66 + a67 + a68 + a69 + a70
      + a71 + a72 + a73 + a74 + a75 + a76 + a77 + a78 + a79 + a80 + a81 +
      a82 + a83 + a84 + a85 + a86 + a87 + a88 + a89 + a90 + a91 + a92 + a93
      + a94 + a95 + a96 + a97 + a98 + a99 + a100 + a101 + a102 + a103 + a104
      + a105 + a106 + a107 + a108 + a109 + a110 + a111 + a112 + a113 + a114
      + a115 + a116 + a117 + a118 + a119 + a120 + a121 + a122 + a123 + a124
      + a125 + a126 + a127 + a128 + a129
end

class reader filename =
  object
    val name = String.uppercase_ascii filename

    method name = name
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
  ignore (Alternating.anon_first 4 5);
  ignore (Ordinals.siblings_and_nesting [ 1; -2; 3 ]);
  ignore (Ordinals.labelled_out_of_order 6);
  ignore (Lazy.force Ordinals.thunk);
  ignore (Ordinals.lazy_then_lambda [ 7 ]);
  ignore (Partial.omit_first 1 ~a:2);
  ignore (omit_first_qualified 3 ~a:4);
  ignore (Partial.neg 5);
  ignore (curried ~b:6);
  ignore (second ~c:7);
  ignore (render [ "a" ]);
  ignore (use_app ());
  ignore (Wide.wide ~a0:0 ~a1:1 ~a2:2 ~a3:3 ~a4:4 ~a5:5 ~a6:6 ~a7:7 ~a8:8
     ~a9:9 ~a10:10 ~a11:11 ~a12:12 ~a13:13 ~a14:14 ~a15:15 ~a16:16 ~a17:17
     ~a18:18 ~a19:19 ~a20:20 ~a21:21 ~a22:22 ~a23:23 ~a24:24 ~a25:25 ~a26:26
     ~a27:27 ~a28:28 ~a29:29 ~a30:30 ~a31:31 ~a32:32 ~a33:33 ~a34:34 ~a35:35
     ~a36:36 ~a37:37 ~a38:38 ~a39:39 ~a40:40 ~a41:41 ~a42:42 ~a43:43 ~a44:44
     ~a45:45 ~a46:46 ~a47:47 ~a48:48 ~a49:49 ~a50:50 ~a51:51 ~a52:52 ~a53:53
     ~a54:54 ~a55:55 ~a56:56 ~a57:57 ~a58:58 ~a59:59 ~a60:60 ~a61:61 ~a62:62
     ~a63:63 ~a64:64 ~a65:65 ~a66:66 ~a67:67 ~a68:68 ~a69:69 ~a70:70 ~a71:71
     ~a72:72 ~a73:73 ~a74:74 ~a75:75 ~a76:76 ~a77:77 ~a78:78 ~a79:79 ~a80:80
     ~a81:81 ~a82:82 ~a83:83 ~a84:84 ~a85:85 ~a86:86 ~a87:87 ~a88:88 ~a89:89
     ~a90:90 ~a91:91 ~a92:92 ~a93:93 ~a94:94 ~a95:95 ~a96:96 ~a97:97 ~a98:98
     ~a99:99 ~a100:100 ~a101:101 ~a102:102 ~a103:103 ~a104:104 ~a105:105
     ~a106:106 ~a107:107 ~a108:108 ~a109:109 ~a110:110 ~a111:111 ~a112:112
     ~a113:113 ~a114:114 ~a115:115 ~a116:116 ~a117:117 ~a118:118 ~a119:119
     ~a120:120 ~a121:121 ~a122:122 ~a123:123 ~a124:124 ~a125:125 ~a126:126
     ~a127:127 ~a128:128 ~a129:129);
  ignore (new reader "r")#name;
  ignore (pick true);
  ignore (Prims.( < ) 1 2);
  ignore (Prims.compare 1 2);
  ignore (Captured.find_exn [ 1; 2 ] ~f:(fun x -> x > 1));
  ignore (Coerced_inst.get ());
  ignore (Nested_inst.get ());
  ignore (First_class.use_id () 5);
  ignore (First_class.use_bump () 6);
  ignore (Generated.escape "a,b");
  ignore (Generated.map [ 1; 2 ] ~f:(fun x -> x + 1))
