(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-simplify;
 check-fexpr-dump;
*)

module type Tree = sig
  type 'a t

  type 'a descr =
    | Empty
    | Leaf of int * 'a
    | Branch of int * 'a t * 'a t

  val descr : 'a t -> 'a descr
end

module Set0 = struct
  type 'a t =
    | Empty
    | Leaf : int -> unit t
    | Branch : int * unit t * unit t -> unit t

  type 'a descr =
    | Empty
    | Leaf of int * 'a
    | Branch of int * 'a t * 'a t

  (* In the [Branch] case, the block given as input is reused. *)
  let descr (type a) : a t -> a descr =
    function
    | Empty -> Empty
    | Leaf elt -> Leaf (elt, ())
    | Branch (a, b, c) -> Branch (a, b, c)
end

module _ : Tree = Set0

module Tree_operations (Tree : Tree) = struct
  open Tree

  let is_empty t =
    (* This function doesn't have a value_kind for [t], as it is abstract.
       When it gets inlined below, [descr] will be inlined as well, but
       it returns [t] in one case. At that point, we need to use the value_kind
       of [descr t] to be able to perform variant unboxing. *)
    match descr t with Empty -> true | Leaf _ | Branch _ -> false
end
[@@inline always]

module Set = struct
  include Tree_operations(Set0)
end