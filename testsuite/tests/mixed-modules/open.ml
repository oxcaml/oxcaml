(* TEST
 include stdlib_upstream_compatible;
 reference = "${test_source_directory}/open.reference";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"


let _ = print_endline "Test 1: [let open] with module ident"

module M = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open M in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 2: [let open] with inline struct"

let _ =
  let open struct let foo = #42.0 let bar = "hello" end in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 3: [let open] with functor"

module Functor (X : sig end) = struct let foo = #42.0 let bar = "hello" end

let _ =
  let open Functor(struct end) in
  print_float (Float_u.to_float (id foo));
  print_string " ";
  print_endline (id bar)


let _ = print_endline "Test 4: Tests 1-3 with [open] instead of [let open]"

module M_4_1 = struct
  open M
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_2 = struct
  open struct let foo = #42.0 let bar = "hello" end
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end

module M_4_3 = struct
  open Functor(struct end)
  let _ = print_float (Float_u.to_float (id foo))
  let _ = print_string " "
  let _ = print_endline (id bar)
end


let _ = print_endline "Test 5: open shadowing open"

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
  open Base
  open Override

  let _ =
    print_float (Float_u.to_float (id x));
    print_string " ";
    print_string (id y);
    print_string " ";
    print_int (id z);
    print_newline ()
end


let _ = print_endline "Test 6: open shadowing a val"

module M6 = struct
  let a = #5.0
  let b = "before open"

  open struct
    let b = "from open"
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


let _ = print_endline "Test 7: val shadowing an open"

module M7 = struct
  open struct
    let p = #8.0
    let q = "from open"
  end

  let p = #9.0
  let r = "after open"

  let _ =
    print_float (Float_u.to_float (id p));
    print_string " ";
    print_string (id q);
    print_string " ";
    print_string (id r);
    print_newline ()
end
