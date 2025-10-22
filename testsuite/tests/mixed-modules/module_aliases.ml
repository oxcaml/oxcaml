(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/module_aliases.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test: Based on pr11186.ml (tests [wrap_id_pos_list])"

module M_1 =
  (((struct
       let y = #(#20.0, "world!", #30.0)
       module N = struct let x = #10.0 let s = "Hello" let y = #0.0 end
       module A = N
       let z = #(#5.0, "don't print", #6.0)
       module B = A
       module C = B
     end : sig
       module A : sig val x : float# val s : string end
       val y : #(float# * string * float#)
       module B = A
       module C = B
       val z : #(float# * string * float#)
     end) : sig
      module B : sig val s : string val x : float# end
      module C = B
      val y : #(float# * string * float#)
    end) : sig
     val y : #(float# * string * float#)
     module C : sig val x : float# val s : string end
   end)

let () =
  let #(f2, world, f3) = id M_1.y in
  print_endline "Expected: 10.0 20.0 30.0 Hello world!";
  Printf.printf
    "Actual:   %.1f %.1f %.1f %s %s\n\n"
    (Float_u.to_float (id M_1.C.x))
    (Float_u.to_float f2)
    (Float_u.to_float f3)
    (id M_1.C.s)
    world


let _ = print_endline "Test: Coercion with aliases, modules, and vals"

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
  let foo_1 = #50.0
  let foo_2 = "Hello, world"
  module Inner = struct
    module Inner_inner = struct
      let inner_inner_1 = #20.0
      let inner_inner_2 = "Hello, world!!"
      let inner_inner_3 = #30.0
    end
    let inner_1 = #10.0
    let inner_2 = "Hello, world!"
  end
  let foo_3 = #40.0
  module Other_inner = Inner.Inner_inner
end

let () =
  print_endline "Expected: 10.0 20.0 30.0 40.0";
  Printf.printf
    "Actual:   %.1f %.1f %.1f %.1f\n"
    (Float_u.to_float (id M_2.Inner.inner_1))
    (Float_u.to_float (id M_2.Inner.Inner_inner.inner_inner_1))
    (Float_u.to_float (id M_2.Other_inner.inner_inner_3))
    (Float_u.to_float (id M_2.foo_3))
