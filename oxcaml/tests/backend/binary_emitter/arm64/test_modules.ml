(* Test modules and functors *)

(* Simple module *)
module IntSet = struct
  type t = int list

  let empty = []

  let rec mem x = function [] -> false | y :: ys -> x = y || mem x ys

  let add x s = if mem x s then s else x :: s

  let rec remove x = function
    | [] -> []
    | y :: ys -> if x = y then ys else y :: remove x ys

  let rec size = function [] -> 0 | _ :: xs -> 1 + size xs
end

(* Module with abstract type *)
module Stack : sig
  type 'a t

  val empty : 'a t

  val push : 'a -> 'a t -> 'a t

  val pop : 'a t -> ('a * 'a t) option

  val is_empty : 'a t -> bool
end = struct
  type 'a t = 'a list

  let empty = []

  let push x s = x :: s

  let pop = function [] -> None | x :: xs -> Some (x, xs)

  let is_empty = function [] -> true | _ -> false
end

(* Functor *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module MakeSet (Elt : COMPARABLE) = struct
  type elt = Elt.t

  type t = elt list

  let empty = []

  let rec mem x = function
    | [] -> false
    | y :: ys ->
      let c = Elt.compare x y in
      if c = 0 then true else if c < 0 then false else mem x ys

  let rec add x = function
    | [] -> [x]
    | y :: ys as s ->
      let c = Elt.compare x y in
      if c = 0 then s else if c < 0 then x :: s else y :: add x ys
end

module IntComparable = struct
  type t = int

  let compare = compare
end

module SortedIntSet = MakeSet (IntComparable)

(* First-class modules *)
let use_comparable (type a) (module M : COMPARABLE with type t = a) x y =
  M.compare x y

(* Local module *)
let local_module_example () =
  let module Local = struct
    let value = 42

    let double x = x * 2
  end in
  Local.double Local.value

(* Entry point *)
let () =
  let s = IntSet.empty in
  let s = IntSet.add 1 s in
  let s = IntSet.add 2 s in
  let s = IntSet.add 3 s in
  let _ = IntSet.mem 2 s in
  let _ = IntSet.mem 5 s in
  let _ = IntSet.size s in
  let s = IntSet.remove 2 s in
  let _ = IntSet.size s in
  let stack = Stack.empty in
  let stack = Stack.push 1 stack in
  let stack = Stack.push 2 stack in
  let _ = Stack.pop stack in
  let _ = Stack.is_empty Stack.empty in
  let ss = SortedIntSet.empty in
  let ss = SortedIntSet.add 3 ss in
  let ss = SortedIntSet.add 1 ss in
  let ss = SortedIntSet.add 2 ss in
  let _ = SortedIntSet.mem 2 ss in
  let _ = use_comparable (module IntComparable) 1 2 in
  let _ = local_module_example () in
  ()
