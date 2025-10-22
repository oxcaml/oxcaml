(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/first_class_modules.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"
type void : void
external void : unit -> void = "%unbox_unit"


let () = print_endline "Test: basic first-class mixed module"

module type S1 = sig
  val unboxed_float : float#
  val unboxed_int64 : int64#
  val boxed_int : int
  val boxed_string : string
end

module M1 : S1 = struct
  let boxed_int = 42
  let boxed_string = "hello"
  let unboxed_float = #3.14
  let unboxed_int64 = #100L
end

let packed = id (module M1 : S1)

let () =
  let module M = (val packed : S1) in
  print_endline "Expected: 42 hello 3.14 100";
  Printf.printf
    "Actual:   %d %s %.2f %d\n\n"
    (id M.boxed_int)
    (id M.boxed_string)
    (Float_u.to_float (id M.unboxed_float))
    (Int64_u.to_int (id M.unboxed_int64))
;;


let () = print_endline "Test: passing modules to functions"

let use_module m =
  let module M = (val m : S1) in
  let sum = M.boxed_int + Int64_u.to_int M.unboxed_int64 in
  let concat =
    M.boxed_string ^ " " ^ string_of_float (Float_u.to_float M.unboxed_float)
  in
  sum, concat
;;

let () =
  let sum, concat = use_module packed in
  print_endline "Expected: 142 hello 3.14";
  Printf.printf "Actual:   %d %s\n\n" (id sum) (id concat)
;;


let () = print_endline "Test: returning modules from functions"

let make_module n s =
  (module struct
    let boxed_int = n
    let boxed_string = s
    let unboxed_float = Float_u.of_float (float_of_int (n + 1))
    let unboxed_int64 = Int64_u.of_int64 (Int64.of_int (2 * n))
  end : S1)
;;

let m3 = make_module 10 "ten"

module M3 = (val m3 : S1)

let () =
  print_endline "Expected: 10 ten 11.0 20";
  Printf.printf
    "Actual:   %d %s %.1f %d\n\n"
    (id M3.boxed_int)
    (id M3.boxed_string)
    (Float_u.to_float (id M3.unboxed_float))
    (Int64_u.to_int (id M3.unboxed_int64))
;;


let () = print_endline "Test: list of first-class modules"

let modules = [
  (module struct
    let unboxed_int64 = #1L
    let boxed_string = "one"
    let boxed_int = 1
    let unboxed_float = #1.0
  end : S1);
  (module struct
    let unboxed_float = #2.0
    let unboxed_int64 = #2L
    let boxed_int = 2
    let boxed_string = "two"
  end : S1);
  (module struct
    let unboxed_float = #3.0
    let boxed_int = 3
    let boxed_string = "three"
    let unboxed_int64 = #3L
  end : S1);
]

let sum_ints mods =
  List.fold_left (fun acc m ->
    let module M = (val m : S1) in
    acc + (Int64_u.to_int M.unboxed_int64)
  ) 0 mods

let () =
  print_endline "Expected: 6";
  Printf.printf "Actual:   %d\n\n" (sum_ints (id modules))
;;


let () = print_endline "Test: conditional module selection"

let select_module b =
  if b then
    (module struct
      let unboxed_int64 = #111L
      let boxed_int = 111
      let boxed_string = "true branch"
      let unboxed_float = #111.111
    end : S1)
  else
    (module struct
      let boxed_string = "false branch"
      let unboxed_float = #222.222
      let boxed_int = 222
      let unboxed_int64 = #222L
    end : S1)
;;

module Selected = (val select_module (id false) : S1)

let () =
  print_endline "Expected: 222.222 false branch";
  Printf.printf
    "Actual:   %.3f %s\n"
    (Float_u.to_float (id Selected.unboxed_float))
    (id Selected.boxed_string)
;;

module Selected2 = (val select_module (id true) : S1)

let () =
  print_endline "Expected: 111.111 true branch";
  Printf.printf
    "Actual:   %.3f %s\n\n"
    (Float_u.to_float (id Selected2.unboxed_float))
    (id Selected2.boxed_string)
;;


let () =
  print_endline "Test: unboxed tuples and records in a first-class module"

module type S6 = sig
  val boxed_tuple : int * string
  val unboxed_tuple : #(float# * string * int64# * void)
  type unboxed_record = #{ x : float#; y : int64#; z : string; v : void }
  val unboxed_rec : unboxed_record
  val f : int -> float#
end

module M6 : S6 = struct
  let boxed_tuple = (42, "answer")
  let unboxed_tuple = #(#3.14, "foo", #271828L, void ())
  type unboxed_record = #{ x : float#; y : int64#; z : string; v : void }
  let unboxed_rec = #{ x = #2.718; y = #314159L; z = "bar"; v = void () }
  let f n = Float_u.of_float (float_of_int (n * n))
end

let packed6 = (module M6 : S6)

module M6_unpacked = (val id packed6 : S6)

let () =
  let n, s = id M6_unpacked.boxed_tuple in
  print_endline "Expected: 42 answer";
  Printf.printf "Actual:   %d %s\n" n s
;;

let () =
  let #(f, s, i, _v) = id M6_unpacked.unboxed_tuple in
  print_endline "Expected: 3.14 foo 271828";
  Printf.printf "Actual:   %.2f %s %d\n"
    (Float_u.to_float f) s (Int64_u.to_int i)
;;

let () =
  let #{ M6_unpacked.x; y; z; v = _ } = id M6_unpacked.unboxed_rec in
  print_endline "Expected: 2.718 314159 bar";
  Printf.printf "Actual:   %.3f %d %s\n\n"
    (Float_u.to_float x) (Int64_u.to_int y) z
;;


let () = print_endline "Test: functor producing first-class mixed modules"

module type Input = sig
  val n : int
  val base : float#
end

module MakeS1 (I : Input) : S1 = struct
  let boxed_int = I.n
  let boxed_string = "Made from " ^ string_of_int I.n
  let unboxed_float = Float_u.mul I.base #2.0
  let unboxed_int64 = Int64_u.of_int64 (Int64.of_int (I.n * 10))
end

let make_from_input n (base : float#) =
  let module I = struct
    let n = n
    let base = base
  end
  in
  (module MakeS1 (I) : S1)
;;

let m7 = make_from_input 5 #1.5

module M7 = (val m7 : S1)

let () =
  print_endline "Expected: 5 3.0";
  Printf.printf
    "Actual:   %d %.1f\n\n"
    (id M7.boxed_int)
    (Float_u.to_float (id M7.unboxed_float))
;;


let () = print_endline "Test: first-class functors"

module type S8 = sig
  val foo : float#
  val bar : string

  module F : functor (X : S1) -> S1

  module S : sig
    val baz : int64#
    val qux : string
  end
end

module type Transform = functor (X : S8) -> S8

let double_transform : (module Transform) =
  (module functor (X : S8) -> struct
    let foo = Float_u.mul X.foo #2.0
    let bar = X.bar ^ " (doubled)"
    module F = functor (Y : S1) -> struct
      module R = X.F(Y)
      let boxed_int = R.boxed_int * 2
      let boxed_string = R.boxed_string ^ " [*2]"
      let unboxed_float = Float_u.mul R.unboxed_float #2.0
      let unboxed_int64 = Int64_u.mul R.unboxed_int64 #2L
    end
    module S = struct
      let baz = Int64_u.mul X.S.baz #2L
      let qux = X.S.qux ^ " (*2)"
    end
  end)

let add_transform n (f : float#) : (module Transform) =
  (module functor (X : S8) -> struct
    let foo = Float_u.add X.foo f
    let bar = X.bar ^ " (+" ^ (Float_u.to_string f) ^ ")"
    module F = functor (Y : S1) -> struct
      module R = X.F(Y)
      let boxed_int = R.boxed_int + n
      let boxed_string = R.boxed_string ^ " [+" ^ string_of_int n ^ "]"
      let unboxed_float = Float_u.add R.unboxed_float f
      let unboxed_int64 =
        Int64_u.add R.unboxed_int64 (Int64_u.of_int64 (Int64.of_int n))
    end
    module S = struct
      let baz = Int64_u.add X.S.baz (Int64_u.of_int64 (Int64.of_int n))
      let qux = X.S.qux ^ " (+" ^ string_of_int n ^ ")"
    end
  end)

let apply_transform transform m =
  let module F = (val transform : Transform) in
  let module M = (val m : S8) in
  (module F(M) : S8)
;;

let m8_base = (module struct
  let foo = #5.0
  let bar = "5.0"
  module F = functor (X : S1) -> struct
    let boxed_int = X.boxed_int + 100
    let boxed_string = X.boxed_string ^ " [+100]"
    let unboxed_float = Float_u.add X.unboxed_float #100.0
    let unboxed_int64 = Int64_u.add X.unboxed_int64 #100L
  end
  module S = struct
    let baz = #20L
    let qux = "20"
  end
end : S8)

let m8_doubled = apply_transform (id double_transform) (id m8_base)
let m8_final = apply_transform (id (add_transform 3 #1.5)) (id m8_doubled)

module M8 = (val m8_final : S8)

let () =
  print_endline "Expected: 5.0 (doubled) (+1.5) = 11.5";
  Printf.printf "Actual:   %s = %.1f\n"
    (id M8.bar)
    (Float_u.to_float (id M8.foo))
;;

let () =
  print_endline "Expected: 20 (*2) (+3) = 43";
  Printf.printf "Actual:   %s = %d\n"
    (id M8.S.qux)
    (Int64_u.to_int (id M8.S.baz))
;;

let test_input = (module struct
    let boxed_int = 1
    let boxed_string = "1"
    let unboxed_float = #1.0
    let unboxed_int64 = #1L
end : S1)

module M8_F_Result = M8.F ((val test_input : S1))

let () =
  print_endline "Expected: 1 [+100] [*2] [+3] = 207";
  Printf.printf
    "Actual:   %s = %d\n\n"
    (id M8_F_Result.boxed_string)
    (id M8_F_Result.boxed_int)
;;


let () = print_endline "Test: subtype by forgetting abstract types"

module type S9_base = sig
  type t : any

  val x : int
  val y : float#
end

module type S9_extended = sig
  type t : any
  type u : any
  type v : any

  val x : int
  val y : float#
end

let m9_extended = (module struct
  type t = string
  type u = int64#
  type v = #(bool * float#)
  let x = 42
  let y = #3.14
end : S9_extended with type t = string and type u = int64#
                   and type v = #(bool * float#))

let m9_base = (m9_extended :> (module S9_base with type t = string))

module M9 = (val m9_base : S9_base with type t = string)

let () =
  print_endline "Expected: 42 3.14";
  Printf.printf "Actual:   %d %.2f\n\n"
    (id M9.x)
    (Float_u.to_float (id M9.y))
;;


let () = print_endline "Test: subtype by forgetting type alias"

module type S10_with_eq = sig
  type t
  type u = t * t

  val x : t -> int
  val y : float#
end

module type S10_fewer_types = sig
  type t

  val x : t -> int
  val y : float#
end

let m10_eq = (module struct
  type t = bool
  type u = t * t
  let x b = if b then 1 else 0
  let y = #2.718
end : S10_with_eq with type t = bool)

let m10_fewer = (m10_eq :> (module S10_fewer_types with type t = bool))

module M10 = (val m10_fewer : S10_fewer_types with type t = bool)

let () =
  print_endline "Expected: 1 2.718";
  Printf.printf "Actual:   %d %.3f\n\n"
    ((id M10.x) true)
    (Float_u.to_float (id M10.y))
;;


let () = print_endline "Test: subtype with unboxed type in forgotten types"

module type S11_many_unboxed = sig
  type t = float#
  type u = int64#
  type v

  val x : t
end

module type S11_one_unboxed = sig
  type t = float#

  val x : t
end

let m11_many = (module struct
  type t = float#
  type u = int64#
  type v = string
  let x = #99.9
end : S11_many_unboxed with type v = string)

let m11_one = (m11_many :> (module S11_one_unboxed))

module M11 = (val m11_one : S11_one_unboxed)

let () =
  print_endline "Expected: 99.9";
  Printf.printf "Actual:   %.1f\n" (Float_u.to_float (id M11.x))
;;
