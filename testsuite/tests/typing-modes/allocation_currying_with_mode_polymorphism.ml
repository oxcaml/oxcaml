(* TEST
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha";
 expect;
*)

module Test = struct
  type t = { mutable value : int }
  let make_make_t () () = { value = 42 }
  let make_t @ noalloc_strict = make_make_t ()
  let secretly_allocated : t @ global = make_t ()
end
[%%expect{|
Line 4, characters 24-42:
4 |   let make_make_t () () = { value = 42 }
                         ^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 4, characters 21-42,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]
