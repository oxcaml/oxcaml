(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/coercions.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test: mixed to mixed"

module Module_1 = struct
  let foo = "hello"
  let bar = #5L
  let baz = "world"
  let qux = 10
  let zil = #15.0
end

module Coerced_module_1 : sig
  val bar : int64#
  val qux : int
end = Module_1

let _ = print_int (Int64_u.to_int (id Coerced_module_1.bar))
let _ = print_endline ""
let _ = print_int (id Coerced_module_1.qux)
let _ = print_endline ""


let _ = print_endline "Test: mixed to value only"

module Coerced_module_2 : sig
  val qux : int
  val foo : string
end = Module_1

let _ = print_int (id Coerced_module_2.qux)
let _ = print_endline ""
let _ = print_endline (id Coerced_module_2.foo)


let _ = print_endline "Test: nested modules"

module Module_3 = struct
  module Numbers = struct
    let smallest_float_u = #0.0
    module Zero = struct
      let as_float_u = #0.0
      let as_float = 0.0
      let as_string = "0"
      let as_int64_u = #0L
    end
    module One = struct
      let as_string = "1"
      let as_float = 1.0
    end
    module Two = struct
      let as_float = 2.0
      let as_string = "2"
      let as_int64_u = #2L
      let as_float_u = #2.0
    end
    let biggest_float_u = #2.0
  end
end

module Coerced_module_3 : sig
  module Numbers : sig
    module One : sig
      val as_float : float
    end
    module Zero : sig
      val as_float_u : float#
      val as_float : float
    end
    val smallest_float_u : float#
  end
end = Module_3

let _ = print_float (id Coerced_module_3.Numbers.One.as_float)
let _ = print_string " "
let _ = print_float (id Coerced_module_3.Numbers.Zero.as_float)
let _ = print_string " "
let _ =
  print_float (Float_u.to_float (id Coerced_module_3.Numbers.Zero.as_float_u))
let _ = print_string " "
let _ =
  print_float (Float_u.to_float (id Coerced_module_3.Numbers.smallest_float_u))
let _ = print_endline ""


let _ = print_endline "Test: composed coercions"

module type S = sig
  module K : sig
    val x : float#
    val t : string
    val y : float#
    val s : string
  end
end

module type S' = sig
  module K : sig
    val y : float#
    val s : string
    val x : float#
  end
end

module M = struct
  module K : sig
    val s : string
    val y : float#
    val x : float#
    val t : string
  end = struct
    let x = #1.0
    let y = #2.0
    let s = "s"
    let t = "s"
    let z = #3.0
  end
end

module N = ((M : S) : S')

let _ = print_float (Float_u.to_float (id N.K.x))
let _ = print_string " "
let _ = print_float (Float_u.to_float (id N.K.y))
let _ = print_string " "
let _ = print_endline (id N.K.s)
