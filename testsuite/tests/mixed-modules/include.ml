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
    print_float (Float_u.to_float (id foo));
    print_string " ";
    print_string (id bar);
    print_string " ";
    print_int (id baz);
    print_string " ";
    print_float (Float_u.to_float (id qux));
    print_newline ()
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
    print_float (Float_u.to_float (id foo));
    print_string " ";
    print_string (id bar);
    print_string " ";
    print_int (id baz);
    print_string " ";
    print_float (Float_u.to_float (id qux));
    print_newline ()
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
    print_float (Float_u.to_float (id foo));
    print_string " ";
    print_string (id bar);
    print_string " ";
    print_int (id baz);
    print_string " ";
    print_float (Float_u.to_float (id qux));
    print_newline ()
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
    print_float (Float_u.to_float (id a));
    print_string " ";
    print_string (id b);
    print_string " ";
    print_float (Float_u.to_float (id c));
    print_string " ";
    print_string (id d);
    print_string " ";
    print_float (Float_u.to_float (id e));
    print_newline ()
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
    print_float (Float_u.to_float (id x));
    print_string " ";
    print_string (id y);
    print_string " ";
    print_int (id z);
    print_newline ()
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
    print_float (Float_u.to_float (id a));
    print_string " ";
    print_string (id b);
    print_string " ";
    print_float (Float_u.to_float (id c));
    print_newline ()
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
    print_float (Float_u.to_float (id p));
    print_string " ";
    print_string (id q);
    print_string " ";
    print_string (id r);
    print_newline ()
end
