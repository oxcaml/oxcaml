(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/include.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test: include with module ident"

module M = struct
  let foo = #42.0
  let bar = "hello"
  let baz = 123
end

module M1 = struct
  include M
  let qux = #3.14

  let _ =
    Printf.printf "%.0f %s %d %.2f\n"
      (Float_u.to_float (id foo))
      (id bar)
      (id baz)
      (Float_u.to_float (id qux))
end


let _ = print_endline "Test: include with inline struct"

module M2 = struct
  include struct
    let foo = #42.0
    let bar = "hello"
    let baz = 123
  end
  let qux = #3.14

  let _ =
    Printf.printf "%.0f %s %d %.2f\n"
      (Float_u.to_float (id foo))
      (id bar)
      (id baz)
      (Float_u.to_float (id qux))
end


let _ = print_endline "Test: include with functor"

module Functor (X : sig end) = struct
  let foo = #42.0
  let bar = "hello"
  let baz = 123
end

module M3 = struct
  include Functor(struct end)
  let qux = #3.14

  let _ =
    Printf.printf "%.0f %s %d %.2f\n"
      (Float_u.to_float (id foo))
      (id bar)
      (id baz)
      (Float_u.to_float (id qux))
end


let _ = print_endline "Test: multiple includes"

module A = struct
  let a = #1.0
  let b = "from A"
end

module B = struct
  let c = #2.0
  let d = "from B"
end

module M4 = struct
  include A
  include B
  let e = #3.0

  let _ =
    Printf.printf "%.0f %s %.0f %s %.0f\n"
      (Float_u.to_float (id a)) (id b) (Float_u.to_float (id c))
      (id d) (Float_u.to_float (id e))
end


let _ = print_endline "Test: include shadowing include"

module Base = struct
  let x = #10.0
  let y = "original"
  let z = 100
end

module Override = struct
  let x = #20.0
  let y = "overridden"
end

module M5 = struct
  include Base
  include Override

  let _ =
    Printf.printf "%.0f %s %d\n" (Float_u.to_float (id x)) (id y) (id z)
end


let _ = print_endline "Test: include shadowing a val"

module M6 = struct
  let a = #5.0
  let b = "before include"
  let c = #0L

  include struct
    let b = "from include"
    let c = #7.0
  end

  let _ =
    Printf.printf "%.0f %s %.0f\n"
      (Float_u.to_float (id a)) (id b) (Float_u.to_float (id c))
end


let _ = print_endline "Test: val shadowing an include"

module M7 = struct
  include struct
    let p = #0L
    let q = "from include"
  end

  let p = #9.0
  let r = "after include"

  let _ =
    Printf.printf "%s %s %s\n"
      (Float_u.to_string (id p)) (id q) (id r)
end
