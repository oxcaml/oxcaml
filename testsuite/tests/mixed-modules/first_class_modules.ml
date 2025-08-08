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

let _ = print_endline "Test 1: attempting first-class module with mixed content"

(* This demonstrates the current limitation: first-class modules 
   require all fields to have value layout *)

module type Mixed_attempt = sig
  val boxed_int : int
  val boxed_string : string
  (* These would cause errors if we tried to pack this module:
  val unboxed_float : float#
  val unboxed_int64 : int64#
  *)
end

(* Workaround: use boxed versions for first-class modules *)
module type Mixed_boxed = sig
  val boxed_int : int
  val boxed_string : string
  val boxed_float : float  (* boxed version *)
  val boxed_int64 : int64  (* boxed version *)
end

module M1 : Mixed_boxed = struct
  let boxed_int = 42
  let boxed_string = "hello"
  let boxed_float = 3.14
  let boxed_int64 = 100L
end

let packed = (module M1 : Mixed_boxed)
module M = (val packed : Mixed_boxed)

let _ = print_int (id M.boxed_int)
let _ = print_endline ""
let _ = print_float (id M.boxed_float)
let _ = print_endline ""


let _ = print_endline "Test 2: mixed module with conversion functions"

(* A pattern: keep unboxed internally, provide conversion functions *)
module type Convertible = sig
  val get_float : unit -> float
  val get_int64 : unit -> int64
  val name : string
  val count : int
end

module MakeConvertible (X : sig val f : float# val i : int64# end) : Convertible = struct
  let internal_float = X.f
  let internal_int64 = X.i
  let get_float () = Float_u.to_float internal_float
  let get_int64 () = Int64_u.to_int64 internal_int64
  let name = "convertible"
  let count = 123
end

let conv_mod = (module MakeConvertible(struct let f = #2.718 let i = #999L end) : Convertible)
module C = (val conv_mod : Convertible)

let _ = print_float (id (C.get_float ()))
let _ = print_endline ""
let _ = print_int (Int64.to_int (id (C.get_int64 ())))
let _ = print_endline ""


let _ = print_endline "Test 3: list of modules with mixed content via conversions"

let modules = [
  (module struct
    let internal_unboxed = #1.0
    let get_float () = Float_u.to_float internal_unboxed
    let get_int64 () = 1L
    let name = "one"
    let count = 1
  end : Convertible);
  (module struct
    let internal_unboxed = #2.0
    let get_float () = Float_u.to_float internal_unboxed
    let get_int64 () = 2L
    let name = "two"
    let count = 2
  end : Convertible);
]

let sum_floats mods =
  List.fold_left (fun acc m ->
    let module M = (val m : Convertible) in
    acc +. M.get_float ()
  ) 0.0 mods

let _ = print_float (id (sum_floats modules))
let _ = print_endline ""


let _ = print_endline "Test 4: functor producing mixed module (direct use, not packed)"

module type Input = sig
  val n : int
  val unboxed_base : float#
end

module MakeMixed (I : Input) = struct
  let boxed_value = I.n * 2
  let unboxed_float = Float_u.mul I.unboxed_base #2.0
  let description = Printf.sprintf "Made from %d" I.n
  let unboxed_int64 = Int64_u.of_int64 (Int64.of_int I.n)
  let boxed_string = "mixed"
end

module Mixed10 = MakeMixed(struct let n = 10 let unboxed_base = #5.5 end)

(* Direct access to mixed module works fine *)
let _ = print_int (id Mixed10.boxed_value)
let _ = print_endline ""
let _ = print_float (Float_u.to_float (id Mixed10.unboxed_float))
let _ = print_endline ""
let _ = print_int (Int64_u.to_int (id Mixed10.unboxed_int64))
let _ = print_endline ""


let _ = print_endline "Test 5: conditional selection with wrapper"

(* Wrapper to enable conditional selection of mixed modules *)
module type Wrapper = sig
  val get_int : unit -> int
  val get_unboxed_float : unit -> float#
  val get_name : unit -> string
end

let select_mixed b =
  if b then
    (module struct
      let internal_float = #111.0
      let internal_int = 111
      let get_int () = internal_int
      let get_unboxed_float () = internal_float
      let get_name () = "selected true"
    end : Wrapper)
  else
    (module struct
      let internal_float = #222.0
      let internal_int = 222
      let get_int () = internal_int
      let get_unboxed_float () = internal_float
      let get_name () = "selected false"
    end : Wrapper)

module W = (val (select_mixed false) : Wrapper)
let _ = print_int (id (W.get_int ()))
let _ = print_endline ""
let _ = print_float (Float_u.to_float (id (W.get_unboxed_float ())))
let _ = print_endline ""


let _ = print_endline "Test 6: higher-order function with mixed module access"

(* Pattern: pass accessors instead of first-class module *)
type mixed_accessors = {
  get_boxed : unit -> int;
  get_unboxed_f : unit -> float#;
  get_unboxed_i : unit -> int64#;
  get_string : unit -> string;
}

let make_accessors n f i s = {
  get_boxed = (fun () -> n);
  get_unboxed_f = (fun () -> f);
  get_unboxed_i = (fun () -> i);
  get_string = (fun () -> s);
}

let process_mixed acc =
  let n = acc.get_boxed () in
  let f = Float_u.to_float (acc.get_unboxed_f ()) in
  let i = Int64_u.to_int (acc.get_unboxed_i ()) in
  n + int_of_float f + i

let acc1 = make_accessors 10 #20.0 #30L "test"
let result = process_mixed acc1
let _ = print_int (id result)
let _ = print_endline ""


let _ = print_endline "Test 7: module with both boxed and unboxed, using records"

(* Another pattern: use records to group mixed data *)
type mixed_data = {
  boxed_part : int * string;
  unboxed_float : float#;
  unboxed_int64 : int64#;
}

module type WithMixedData = sig
  val data : mixed_data
  val name : string
end

(* This module type can't be packed, but we can work with it directly *)
module MixedImpl : WithMixedData = struct
  let data = {
    boxed_part = (42, "answer");
    unboxed_float = #3.14159;
    unboxed_int64 = #271828L;
  }
  let name = "mixed implementation"
end

let _ = 
  let (n, s) = MixedImpl.data.boxed_part in
  print_int (id n);
  print_endline "";
  print_float (Float_u.to_float (id MixedImpl.data.unboxed_float));
  print_endline "";
  print_int (Int64_u.to_int (id MixedImpl.data.unboxed_int64));
  print_endline ""