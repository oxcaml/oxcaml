(* TEST
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* Boxed record: creation allocates a block on the heap. *)
type t = { mutable value : int }
[%%expect{|
type t = { mutable value : int; }
|}]

let state : t list ref = ref []
[%%expect{|
val state : t list ref @@ stateless noalloc_strict = {contents = []}
|}]

module ModePolymorphic = struct
  let rec (iter @ noalloc_strict) (f : 'a -> unit) : 'a list -> unit = function
    | [] -> ()
    | head :: tail -> f head; iter f tail

  let rec (fold @ noalloc_strict) f (acc : 'acc) : 'a list -> 'acc =
    function
    | [] -> acc
    | head :: tail -> fold f (f acc head) tail
end
[%%expect{|
module ModePolymorphic :
  sig
    val iter :
      ('a -> unit) @ [< many] ->
      'a list @ [< global many read_write > aliased stateful dynamic alloc] ->
      unit @ [< global many read_write > aliased stateful dynamic alloc]
    val fold :
      ('acc @ [> 'o | 'n | dynamic] ->
       'a @ [> aliased stateful dynamic alloc] ->
       'acc @ [< 'm & 'n & global many read_write]) @ [< past('mm0) & past('q) & many > aliased] ->
      ('acc @ [< 'o & global many read_write > 'm | dynamic] ->
       ('a list @ [< global many read_write > aliased stateful dynamic alloc] ->
        'acc @ [< global many read_write > aliased stateful dynamic alloc]) @ [> close('o) | past('p) | past('mm0) | local stateful]) @ [< past('p) > past('q) | local]
  end @@ stateless noalloc_strict
|}, Principal{|
module ModePolymorphic :
  sig
    val iter :
      ('a -> unit) @ [< many > aliased] ->
      'a list @ [< global many read_write > aliased stateful dynamic alloc] ->
      unit @ [< global many read_write > aliased stateful dynamic alloc]
    val fold :
      ('acc @ [> 'o | 'n | dynamic] ->
       'a @ [> aliased stateful dynamic alloc] ->
       'acc @ [< 'm & 'n & global many read_write]) @ [< past('mm0) & past('q) & many > aliased] ->
      ('acc @ [< 'o & global many read_write > 'm | dynamic] ->
       ('a list @ [< global many read_write > aliased stateful dynamic alloc] ->
        'acc @ [< global many read_write > aliased stateful dynamic alloc]) @ [> close('o) | past('p) | past('mm0) | local stateful]) @ [< past('p) > past('q) | local]
  end @@ stateless noalloc_strict
|}]

module ModePolymorphismWorksWithAllocation = struct
  let has_to_be_local : unit =
    let (_iter_alloc @ alloc) : 'a list -> unit =
      ModePolymorphic.iter (fun value -> state := { value } :: !state)
    in
    let (_iter_noalloc @ noalloc_strict) : 'a list -> unit =
      ModePolymorphic.iter (fun _ -> ())
    in
    let (_fold_alloc @ alloc) : 'a list -> 'a list =
      ModePolymorphic.fold (fun tail head -> head :: tail) []
    in
    let (_fold_noalloc @ noalloc_strict) : 'a list -> unit =
      ModePolymorphic.fold (fun () _ -> ()) ()
    in
    ()
end
[%%expect{|
module ModePolymorphismWorksWithAllocation :
  sig val has_to_be_local : unit end @@ stateless noalloc_strict
|}]

module RejectAllocatingIter = struct
  let has_to_be_local : unit =
    let (_bad @ noalloc_strict) : 'a list -> unit =
      ModePolymorphic.iter (fun value -> state := { value } :: !state)
    in
    ()
end
[%%expect{|
Line 4, characters 50-69:
4 |       ModePolymorphic.iter (fun value -> state := { value } :: !state)
                                                      ^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 4, characters 27-70,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module RejectAllocatingFold = struct
  let has_to_be_local : unit =
    let (_bad @ noalloc_strict) : 'a list -> 'a list =
      ModePolymorphic.fold (fun tail head -> head :: tail) []
    in
    ()
end
[%%expect{|
Line 4, characters 6-61:
4 |       ModePolymorphic.fold (fun tail head -> head :: tail) []
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "alloc" but is expected to be "noalloc_strict".
|}]

(* Regression test:
   mode-polymorphic functions need to track *delayed* heap checks,
   even when the affected mode variables are copied for each instantiation,
   e.g. during partial application: *)
module CurriedNoalloc = struct
  (* A function returning (a function returning (a block on the heap)). *)
  let make_make () () = { value = 42 }
  (* Instantiate only the outer function, then abuse mode polymorphism. *)
  let make @ noalloc_strict = make_make ()
  (* Inexplicably pause trading. *)
  let secretly_allocated : t @ global = make ()
end
[%%expect{|
Line 3, characters 24-38:
3 |   let make_make () () = { value = 42 }
                            ^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 19-38,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* With `alloc` instead of `noalloc/_strict`, this ought to compile: *)
module CurriedAlloc = struct
  let make_make () () = { value = 42 }
  let make @ alloc = make_make ()
  let this_is_fine : t @ global = make ()
end
[%%expect{|
module CurriedAlloc :
  sig
    val make_make : unit @ 'o -> unit @ 'n -> t @ 'm
    val make : unit -> t @ 'm
    val this_is_fine : t @@ noalloc_strict
  end @@ stateless
|}, Principal{|
module CurriedAlloc :
  sig
    val make_make : unit @ [< global] -> unit @ 'n -> t @ 'm
    val make : unit -> t @ 'm
    val this_is_fine : t @@ noalloc_strict
  end @@ stateless
|}]

module OptionalDefaultNoalloc = struct
  let f ?(x = { value = 42 }) () = x
  let g @ noalloc_strict = f ?x:None
  let result : t @ global = g ()
end
[%%expect{|
Line 2, characters 14-28:
2 |   let f ?(x = { value = 42 }) () = x
                  ^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the expression at line 2, characters 14-28,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module OptionalDefaultAlloc = struct
  let f ?(x = { value = 42 }) () = x
  let g @ alloc = f ?x:None
  let result : t @ global = g ()
end
[%%expect{|
module OptionalDefaultAlloc :
  sig
    val f :
      ?x:t @ [< 'm & global] ->
      unit @ 'n ->
      t @ [> 'm mod many portable forkable unyielding stateless noalloc_strict]
      @@ stateless
    val g : unit -> t @ [> aliased]
    val result : t @@ stateless noalloc_strict
  end
|}, Principal{|
module OptionalDefaultAlloc :
  sig
    val f : ?x:t @ [< 'm & global] -> unit @ 'n -> t @ [> 'm] @@ stateless
    val g : unit -> t @ [> aliased]
    val result : t @@ stateless noalloc_strict
  end
|}]

module OptionalDefaultWithoutAllocation = struct
  let f ?(x = 42) () = x
  let g @ noalloc_strict = f ?x:None
  let result = g ()
end
[%%expect{|
module OptionalDefaultWithoutAllocation :
  sig
    val f : ?x:int @ [< global] -> unit @ 'n -> int @ 'm
    val g : unit -> int @ 'm @@ noalloc_strict
    val result : int @@ noalloc_strict
  end @@ stateless
|}, Principal{|
module OptionalDefaultWithoutAllocation :
  sig
    val f : ?x:int @ [< 'm & global] -> unit @ 'n -> int @ [> 'm] @@
      stateless
    val g : unit -> int @ [> aliased] @@ noalloc_strict
    val result : int @@ stateless noalloc_strict
  end
|}]
