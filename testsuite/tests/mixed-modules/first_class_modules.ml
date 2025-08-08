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

let _ = print_endline "Test 1: basic first-class mixed module"

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

let packed = (module M1 : S1)

let () =
  let module M = (val packed : S1) in
  print_int (id M.boxed_int);
  print_endline "";
  print_string (id M.boxed_string);
  print_endline "";
  print_float (Float_u.to_float (id M.unboxed_float));
  print_endline "";
  print_int (Int64_u.to_int (id M.unboxed_int64));
  print_endline ""


let _ = print_endline "Test 2: passing mixed modules to functions"

let use_mixed_module m =
  let module M = (val m : S1) in
  let sum = M.boxed_int + Int64_u.to_int M.unboxed_int64 in
  let concat = M.boxed_string ^ " " ^ string_of_float (Float_u.to_float M.unboxed_float) in
  (sum, concat)

let (sum, concat) = use_mixed_module packed
let _ = print_int (id sum)
let _ = print_endline ""
let _ = print_endline (id concat)


let _ = print_endline "Test 3: returning mixed modules from functions"

let make_mixed_module n s =
  (module struct
    let boxed_int = n
    let boxed_string = s
    let unboxed_float = Float_u.of_float (float_of_int n)
    let unboxed_int64 = Int64_u.of_int64 (Int64.of_int (n * 2))
  end : S1)

let m2 = make_mixed_module 10 "ten"
module M2 = (val m2 : S1)
let _ = print_int (id M2.boxed_int)
let _ = print_endline ""
let _ = print_string (id M2.boxed_string)
let _ = print_endline ""


let _ = print_endline "Test 4: list of first-class mixed modules"

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

let _ = print_int (sum_ints (id modules))
let _ = print_endline ""


let _ = print_endline "Test 5: conditional module selection"

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

module Selected = (val (select_module (id false)) : S1)
let _ = print_float (Float_u.to_float (id Selected.unboxed_float))
let _ = print_endline ""
let _ = print_string (id Selected.boxed_string)
let _ = print_endline ""

module Selected2 = (val (select_module (id true)) : S1)
let _ = print_float (Float_u.to_float (id Selected2.unboxed_float))
let _ = print_endline ""
let _ = print_string (id Selected2.boxed_string)
let _ = print_endline ""

let _ = print_endline "Test 6: mixed module with unboxed tuples and records"

module type S2 = sig
  val regular_tuple : int * string
  val unboxed_tuple : #(float# * int64#)
  type unboxed_record = #{ x : float#; y : int64# }
  val unboxed_rec : unboxed_record
  val mixed_fun : int -> float#
end

module MC : S2 = struct
  let regular_tuple = (42, "answer")
  let unboxed_tuple = #(#3.14, #271828L)
  type unboxed_record = #{ x : float#; y : int64# }
  let unboxed_rec = #{ x = #2.718; y = #314159L }
  let mixed_fun n = Float_u.of_float (float_of_int (n * n))
end

let packed_complex = (module MC : S2)
module MCC = (val (id packed_complex) : S2)

let _ =
  let (n, s) = MCC.regular_tuple in
  print_int (id n);
  print_endline "";
  print_string (id s);
  print_endline ""

let _ =
  let #(f, i) = MCC.unboxed_tuple in
  print_float (Float_u.to_float (id f));
  print_endline "";
  print_int (Int64_u.to_int (id i));
  print_endline ""

let _ =
  let #{ MCC.x; y } = MCC.unboxed_rec in
  print_float (Float_u.to_float (id x));
  print_endline "";
  print_int (Int64_u.to_int (id y));
  print_endline ""


let _ = print_endline "Test 7: functor producing first-class mixed modules"

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
  end in
  (module MakeS1(I) : S1)

let m3 = make_from_input 5 #1.5
module M3 = (val m3 : S1)
let _ = print_int (id M3.boxed_int)
let _ = print_endline ""
let _ = print_float (Float_u.to_float (id M3.unboxed_float))
let _ = print_endline ""
