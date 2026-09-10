(* TEST
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha";
 expect;
*)

module Test = struct
  type box = { mutable value : int }
  let make_box_maker () () = { value = 0 }
  let make_box @ noalloc_strict = make_box_maker ()
  let heap_box : box @ global = make_box ()
end
[%%expect{|
Line 3, characters 24-42:
3 |   let make_box_maker () () = { value = 0 }
                            ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 21-42,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]
