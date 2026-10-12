(* TEST
   flags += " -extension mode_alpha";
   flags += " -extension mode_polymorphism_alpha";
   flags += " -extension mode_polymorphism_printing";
   expect.opt;
*)

module Allocation_can't_be_polymorphic = struct
  (* The partially-applied one-argument function is mode-polymorphic. *)
  let allocate_allocating_closure () = fun () ->
    let (state @ global) = ref None in
    state := Some 42
  (* Naively, we could abuse that mode polymorphism like so: *)
  let allocating_closure @ noalloc_strict = allocate_allocating_closure ()
end
[%%expect{|
Line 4, characters 27-30:
4 |     let (state @ global) = ref None in
                               ^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 3-5, characters 39-20,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Function_wrapper = struct
  type t = { f : unit -> unit }
  let wrap f = { f }
end
[%%expect{|
module Function_wrapper :
  sig
    type t = { f : unit -> unit; }
    val wrap :
      (unit @ [> aliased stateful dynamic alloc] ->
       unit @ [< global many read_write]) @ [< 'm mod aliased contended immutable & global] ->
      t @ [> 'm]
  end @@ stateless
|}]

let state : int option ref = ref None
[%%expect{|
val state : int option ref @@ stateless noalloc_strict = {contents = None}
|}]

module Wrapped_allocating_function : sig
  val wrapped : Function_wrapper.t (* implicitly [@@ alloc] *)
end = struct
  let wrapped = Function_wrapper.wrap (fun () -> state := Some 42)
end
[%%expect{|
module Wrapped_allocating_function : sig val wrapped : Function_wrapper.t end
|}]

module Wrapped_nonallocating_function : sig
  val wrapped : Function_wrapper.t @@ noalloc
end = struct
  let wrapped = Function_wrapper.wrap (fun () -> ())
end
[%%expect{|
module Wrapped_nonallocating_function :
  sig val wrapped : Function_wrapper.t @@ noalloc end @@ stateless
  noalloc_strict
|}]

module Wrapped_allocating_function_as_noalloc : sig
  val wrapped : Function_wrapper.t @@ noalloc
end = struct
  let wrapped = Function_wrapper.wrap (fun () -> state := Some 42)
end
[%%expect{|
Line 4, characters 58-65:
4 |   let wrapped = Function_wrapper.wrap (fun () -> state := Some 42)
                                                              ^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 4, characters 38-66,
         which is "noalloc" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]
