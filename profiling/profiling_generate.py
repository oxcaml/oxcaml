import os
import textwrap

MODULE_COUNT = int(os.environ.get("LARGE_TYPECHECK_MODULES", "240"))

modules = []
body_template = """
module M{i} = struct
  module type OPS = sig
    type t
    val merge : t -> t -> t
    val neutral : t
    val digest : t -> string
  end

  module Int_ops = struct
    type t = int
    let merge a b = a + b + {i}
    let neutral = {i}
    let digest x = string_of_int (x + {i})
  end

  module Accumulate (X : OPS) = struct
    type state = {{ total : X.t; desc : string list }}

    let update acc value =
      {{ total = X.merge acc.total value;
        desc = X.digest value :: acc.desc }}

    let rec build n acc =
      if n <= 0 then acc else build (n - 1) (update acc X.neutral)
  end

  type _ tree =
    | Value : 'a -> 'a tree
    | Pair : 'a tree * 'a tree -> 'a tree
    | Map : ('a -> 'a) * 'a tree -> 'a tree

  let rec eval : type a. a tree -> a = function
    | Value v -> v
    | Pair (a, b) ->
      let _ = eval a in
      eval b
    | Map (f, x) -> f (eval x)

  let rec height : type a. a tree -> int = function
    | Value _ -> 1
    | Pair (a, b) -> 1 + max (height a) (height b)
    | Map (_, x) -> 1 + height x

  let rec map_tree : type a. (a -> a) -> a tree -> a tree = fun f -> function
    | Value v -> Value (f v)
    | Pair (a, b) -> Pair (map_tree f a, map_tree f b)
    | Map (g, x) -> Map ((fun y -> f (g y)), map_tree f x)

  let combine_all (type a) (module X : OPS with type t = a) (items : a tree list) : a =
    let rec fold acc = function
      | [] -> acc
      | hd :: tl ->
        let value = eval hd in
        let acc = X.merge acc value in
        fold acc tl
    in
    fold X.neutral items

  type 'a cell =
    | Leaf of 'a tree
    | Link of 'a tree * 'a cell

  type ('a, 'b) zipper =
    | ZNil
    | ZCons of 'a tree * 'b tree * ('a, 'b) zipper

  type 'a cell_ref = {{
    mutable focus : 'a cell;
    mutable zipper : ('a, 'a) zipper;
    mutable depth : int;
  }}

  module Nested = struct
    type 'a context = {{ value : 'a tree; printer : 'a -> string }}

    let rec show_tree : type a. (a -> string) -> a tree -> string = fun show -> function
      | Value v -> show v
      | Pair (x, y) -> "(" ^ show_tree show x ^ ")" ^ show_tree show y
      | Map (_, x) -> "map " ^ show_tree show x

    let elaborate (type a) (ctx : a context) : string =
      let printer value = ctx.printer value in
      let module Printer = struct
        type t = string
        let merge a b = a ^ "|" ^ b
        let neutral = printer (eval ctx.value)
        let digest x = x
      end in
      let module A = Accumulate (Printer) in
      let open A in
      let seed = {{ total = Printer.neutral; desc = [] }} in
      let result = A.build {i} seed in
      show_tree printer ctx.value ^ "::" ^ String.concat ":" result.desc
  end

  let use_first_class_module input =
    let module Local = struct
      include Int_ops
      let neutral = Int_ops.neutral + input
    end in
    let module A = Accumulate (Local) in
    let open A in
    let seed = {{ total = Local.neutral; desc = [] }} in
    let final = A.build (input land 7 + {i}) seed in
    final.total

  let rec stress_polymorphism ~f ~count acc =
    if count <= 0 then acc else stress_polymorphism ~f ~count:(count - 1) (f acc)

  let cascade value =
    let module C = (val (module Int_ops : OPS with type t = int)) in
    let trees =
      [ Value value;
        Map ((fun x -> x + {i}), Value (value + 1));
        Pair (Value (value + 2), Value (value + 3)) ]
    in
    let total = combine_all (module C) trees in
    let _ = stress_polymorphism ~f:(fun x -> x + total) ~count:{i} value in
    total

  let example_cell (value : int tree) =
    let rec build depth =
      if depth <= 0 then Leaf value
      else Link (value, build (depth - 1))
    in
    build (3 + ({i} mod 5))

  let _ = example_cell (Value {i})

  let example_zipper () =
    let rec loop n acc =
      if n <= 0 then acc
      else loop (n - 1) (ZCons (Value n, Value (n + {i}), acc))
    in
    loop (2 + ({i} mod 4)) ZNil

  let _ = example_zipper ()

  let run value =
    let open Nested in
    let ctx = {{ value = Pair (Value value, Map ((fun x -> x + {i}), Value (value + 4))); printer = string_of_int }} in
    let description = elaborate ctx in
    let cascade_result = cascade value in
    let local_total = use_first_class_module value in
    description, cascade_result + local_total + height ctx.value

  let _ =
    let text, total = run {i} in
    ignore text;
    ignore total
end
"""
for i in range(1, MODULE_COUNT + 1):
    body = body_template.format(i=i)
    modules.append(textwrap.dedent(body))

with open("profiling/large_typecheck.ml", "w") as out:
    out.write("(* Automatically generated large OCaml file for profiling type checking *)\n\n")
    for mod_src in modules:
        out.write(mod_src)
        out.write("\n")
