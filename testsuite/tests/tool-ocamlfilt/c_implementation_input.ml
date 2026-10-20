(* Symbol corpus for c_implementation.ml: a purpose-written unit that
   produces every kind of item of the structured scheme (and the flat
   data symbols of the same constructs), so that the comparison of the
   two demanglers does not depend on the other end-to-end tests, whose
   sources some CI configurations delete. [@inline never] and [@cold] keep
   the closures as symbols of their own; the forcing block at the end
   keeps every definition alive. *)

module Lib = C_implementation_lib

(* {1 Nested modules, functors and their applications} *)

module Outer = struct
  module Middle = struct
    module Inner = struct
      let[@inline never] greet name = "hello, " ^ name

      let[@inline never] shout name = String.uppercase_ascii (greet name)
    end

    let[@inline never] twice f x = f (f x)
  end

  let[@inline never] apply_twice x = Middle.twice (fun[@cold] y -> y + 1) x
end

module type Shape = sig
  type t

  val area : t -> float
end

module Make_pair (A : Shape) (B : Shape) = struct
  type t = A.t * B.t

  let[@inline never] area (a, b) = A.area a +. B.area b
end

module Square = struct
  type t = float

  let[@inline never] area s = s *. s
end

module Circle = struct
  type t = float

  let[@inline never] area r = 3.14159 *. r *. r
end

module Pair_shape = Make_pair (Square) (Circle)

module String_max = Lib.Max (String)

(* {1 Named and anonymous functions at several depths, lazies} *)

let[@inline never] outer_named x =
  let[@inline never] middle_named y =
    List.map
      (fun[@cold] z ->
        let[@inline never] inner_named w = x + y + z + w in
        List.map (fun[@cold] v -> inner_named v) [z; z + 1])
      [y; y + 1]
  in
  middle_named (x + 1)

let[@inline never] anonymous_siblings xs =
  let a = List.map (fun[@cold] x -> x + 1) xs in
  let b = List.filter (fun[@cold] x -> x > 2) a in
  List.fold_left (fun[@cold] acc x -> acc + x) 0 b

let[@inline never] thunks x =
  let first = lazy (Sys.opaque_identity (x * 2)) in
  let second = lazy (Sys.opaque_identity (x * 3)) in
  let adder = fun[@cold] y -> Lazy.force first + Lazy.force second + y in
  adder 1

let () = List.iter (fun[@cold] x -> ignore (Sys.opaque_identity x)) [1; 2]

(* {1 Anonymous and first-class modules} *)

let[@inline never] with_local_module x =
  let module M = struct
    let[@inline never] scale y = x * y
  end in
  M.scale 3

let[@inline never] pick flag : (module Shape with type t = float) =
  if flag
  then
    (module struct
      type t = float

      let[@inline never] area s = s +. 1.0
    end)
  else
    (module struct
      type t = float

      let[@inline never] area s = s -. 1.0
    end)

(* {1 Partial applications} *)

module Partial = struct
  let[@inline never] labelled ~first ~second ~third = first - second + third

  (* The frontend builds the wrapper: an earlier labelled argument is
     omitted while a later one is supplied. *)
  let[@inline never] omit_first x = labelled ~second:x

  (* The middle end builds the wrapper: a prefix of the arguments. *)
  let curried = labelled ~first:(Sys.opaque_identity 1)

  (* An eta-expanded primitive is not a partial application. *)
  let neg = ( ~- )

  let add_one_to_all = List.map (fun[@cold] x -> x + 1)
end

let[@inline never] omit_first_qualified x = Partial.labelled ~third:x

let[@inline never] omit_first_cross_unit x = Lib.labelled ~second:x

let add_ten = Lib.add (Sys.opaque_identity 10)

let plus_two = Lib.make_adder 2

let scale_by_three = Lib.make_scaler 3

let doubled = Lib.Pair.both (fun[@cold] x -> x * 2)

(* {1 Externals re-exported through a signature} *)

module Prims : sig
  val length : string -> int

  val id : 'a -> 'a
end = struct
  external length : string -> int = "%string_length"

  let[@inline never] id x = x
end

(* {1 Identifiers that need escaping} *)

module Ops = struct
  let[@inline never] ( >>= ) x f = match x with Some v -> f v | None -> None

  let ( let* ) = ( >>= )

  let[@inline never] ( .%() ) arr i = Array.get arr i

  let[@inline never] ( .%()<- ) arr i v = Array.set arr i v

  let[@inline never] func'prime' x = x + 1

  let[@inline never] with_let_star a b =
    let* x = a in
    let* y = b in
    Some (x + y)
end

(* {1 Classes and objects} *)

class point x_init =
  object (self)
    val mutable x = x_init

    method get_x = x

    method move d =
      x <- x + d;
      self#get_x
  end

class virtual shape =
  object
    method virtual area : float

    method describe = Printf.sprintf "area %f" (Sys.opaque_identity 1.0)
  end

class square s =
  object
    inherit shape

    method area = s *. s
  end

let make_counter () =
  object
    val mutable n = 0

    method incr =
      n <- n + 1;
      n
  end

(* {1 Forcing block} *)

let () =
  let module P = (val pick true : Shape with type t = float) in
  let open Ops in
  ignore
    (Sys.opaque_identity
       ( Outer.Middle.Inner.shout "x",
         Outer.apply_twice 1,
         Pair_shape.area (2.0, 3.0),
         String_max.max "a" "b",
         Lib.Int_max.max 1 2,
         outer_named 1,
         anonymous_siblings [1; 2; 3],
         thunks 4,
         with_local_module 5,
         P.area 1.0,
         Partial.omit_first 1 ~first:2 ~third:3,
         Partial.curried ~second:1 ~third:2,
         Partial.neg 3,
         Partial.add_one_to_all [1],
         omit_first_qualified 1 ~first:2 ~second:3,
         omit_first_cross_unit 1 ~first:2,
         add_ten 1,
         plus_two 1,
         scale_by_three [1; 2],
         doubled (1, 2),
         Lib.Pair.swap (1, 2),
         Prims.length "abc",
         Prims.id 1,
         Some 1 >>= (fun[@cold] x -> Some (x + 1)),
         [| 1 |].%(0),
         (let arr = [| 0 |] in
          arr.%(0) <- 1;
          arr),
         func'prime' 1,
         with_let_star (Some 1) (Some 2),
         (new point 1)#move 2,
         (new square 2.0)#describe,
         (make_counter ())#incr ))
