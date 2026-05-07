(** Test what symbols are generated on a variety of cases

    The names that could end up in a symbol should not contain any [F] for the
    normalisation to work properly (see the [dune] rule).

    Note that the symbols captured in [symbols.flat], ie using the flat
    name-mangling scheme, are {i before} the final escape step that happens
    while generating assembly. But post-processing assembly code is more fragile
    than using linear output and the result is already testing what we want. *)

let top_level_fun x = x

let ( let* ) x f = f x

let ( .%{} ) x i = Array.get x i

let some_fun x =
  let rec some_inner_fun a = 1 + some_inner_fun a in
  some_inner_fun x

let rec shadow_fun () = shadow_fun () + 1

let shadow_fun () = shadow_fun () + 2

let with_partially_applied_fun xs = List.map (( + ) 1) xs

let lazy_value = lazy (1 + 2)

let with_anonymous_fun xs = List.map (fun x -> x + 1) xs

let with_another_anonymous_fun () = [(fun x -> x + 1)]

let with_opt ?opt () = match opt with None -> 12 | Some x -> x + 1

let with_anonymous_with_opt xs = List.map (fun _ -> with_opt ()) xs

module Module_A = struct
  let nested_fun x = x

  module Module_B = struct
    let double_nested_fun x = x
  end
end

module Some_functor (X : Set.OrderedType) = struct
  let compare_refl x = X.compare x x
end

module SomeInstance = Some_functor (Int)

module type S = sig
  val some_intf_fun : int -> int
end

let module_list : (module S) list =
  [ (module struct
      let some_intf_fun x = x + 1
    end);
    (module struct
      let some_intf_fun x = x + 2
    end) ]

module Some_other_functor (X : S) = struct
  let some_functor_fun x = X.some_intf_fun (x + 3)
end

include Some_other_functor (struct
  let some_intf_fun x = x + 4
end)

class point =
  object
    val mutable x = 0

    method get_x = x

    method move d = x <- x + d
  end
