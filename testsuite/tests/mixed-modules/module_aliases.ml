(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/aliases.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test 1: Based on pr11186.ml"

module M_1 =
  (((struct
       module N = struct let x = #4.0 let s = "Hello" end
       module A = N
       module B = A
       module C = B
     end : sig
       module A : sig val x : float# val s : string end
       module B = A
       module C = B
     end) : sig
      module B : sig val x : float# val s : string end
      module C = B
    end) : sig
     module C : sig val x : float# val s : string end
   end)

let _ = print_float (Float_u.to_float (id M_1.C.x))
let _ = print_string " "
let _ = print_endline (id M_1.C.s)


let _ = print_endline "Test 2: Coercion with aliases, modules, and vals"

module type S_2 = sig
  val foo_2 : string
  val foo_3 : float#

  module Inner : sig
    val inner_1 : float#
    module Inner_inner : sig
      val inner_inner_1 : float#
      val inner_inner_2 : string
    end
  end
  module Other_inner : sig
    val inner_inner_2 : string
    val inner_inner_3 : float#
  end
end

module M_2 : S_2 = struct
  let foo_1 = #1.0
  let foo_2 = "Hello, world"
  module Inner = struct
    let inner_1 = #2.0
    let inner_2 = "Hello, world!"
    module Inner_inner = struct
      let inner_inner_1 = #3.0
      let inner_inner_2 = "Hello, world!!"
      let inner_inner_3 = #4.0
    end
  end
  let foo_3 = #5.0
  module Other_inner = Inner.Inner_inner
end

let _ = print_float (Float_u.to_float (id M_2.Inner.inner_1))
let _ = print_string " "
let _ = print_float (Float_u.to_float (id M_2.Inner.Inner_inner.inner_inner_1))
let _ = print_string " "
let _ = print_float (Float_u.to_float (id M_2.Other_inner.inner_inner_3))
let _ = print_string " "
let _ = print_float (Float_u.to_float (id M_2.foo_3))
let _ = print_endline ""
