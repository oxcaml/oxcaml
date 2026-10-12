(* TEST
   flat-float-array;
   flags += " -extension mode_alpha";
   flags += " -extension mode_polymorphism_alpha";
   flags += " -extension mode_polymorphism_printing";
   flags += " -extension layout_poly_alpha";
   expect.opt;
*)

module Matching_a_float_array_allocates : sig
  val head_or_42 : float array -> float @@ noalloc
end = struct
  let head_or_42 = function
    | [| f |] -> f
    | _ -> 42.0
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Matching_a_float_record_allocates : sig
  type r = { field1 : float; field2 : float }
  val field1 : r -> float @@ noalloc
end = struct
  type r = { field1 : float; field2 : float }
  let field1 = function
    | { field1; _ } -> field1
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Generic_arrays_might_be_float_arrays : sig
  val head_or_diverge : 'a array -> 'a @@ noalloc
end = struct
  let head_or_diverge = function
    | [| a |] -> a
    | _ -> while true do () done
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Unboxable_return_value : sig
  val forty_two : unit -> float @@ noalloc
end = struct
  let[@unboxable] forty_two () = 42.0
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Unboxable_argument : sig
  val id : float @ global -> float @ global @@ noalloc
end = struct
  let id (f [@unboxable] : float) = f
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Optional_argument_defaults_can_allocate : sig
  val f : ?u:unit -> unit -> unit
  val g : unit -> unit @@ noalloc
end = struct
  let f ?(u = (let (state @ global) = ref None in state := Some 42)) () = ()
  let g = f ?u:None
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Recursion_is_fine : sig
  val iter : f:('a -> unit) -> ('a list -> unit) @ local @@ noalloc
end = struct
  let rec iter ~f = function | [] -> () | head :: tail -> (f head; iter ~f tail)
end
[%%expect{|
module Recursion_is_fine :
  sig val iter : f:('a -> unit) -> ('a list -> unit) @ local @@ noalloc end
  @@ stateless noalloc_strict
|}]

module Local_but_allocates_on_the_heap : sig
  val stack_allowed : int -> (int -> int) @ local @@ noalloc
  val heap_required : int -> int (* implicitly [@ global] *)
end = struct
  let stack_allowed x y = x + y
  let heap_required = stack_allowed 42
end
[%%expect{|
Line 5, characters 22-31:
5 |   let stack_allowed x y = x + y
                          ^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 5, characters 20-31,
         which is "noalloc" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Float_ffi_needs_boxing : sig
  val exp_42 : unit -> float @@ noalloc
end = struct
  let exp_42 () = exp 42.0 (* standard library function *)
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Mixed_block_float_access_needs_box : sig
  type r = { u : float#; f : float; }
  val f : r -> float @@ noalloc
end = struct
  type r = { u : float#; f : float; }
  let f { f; _ } = f
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]

module Layout_polymorphic_record_field_access : sig
  type ('a : any) r = { a : 'a; f : float; }
  val poly_ f : 'a r -> float @@ noalloc
end = struct
  type ('a : any) r = { a : 'a; f : float; }
  let poly_ f { f; _ } = f
end
[%%expect{|
CR wsturgeon for wsturgeon: this has to fail
|}]
