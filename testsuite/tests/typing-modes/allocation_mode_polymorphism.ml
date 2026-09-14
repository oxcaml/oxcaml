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
  let rec iter (f : 'a -> unit) : 'a list -> unit = function
    | [] -> ()
    | head :: tail -> f head; iter f tail
end
[%%expect{|
module ModePolymorphic :
  sig
    val iter :
      ('a -> unit) @ [< past('n) & past('o) & global many] ->
      ('a list ->
      unit @ [< past('m) & global many read_write > aliased stateful dynamic alloc]) @ [> past('m) | past('n) | past('o) | stateful]
  end
|}]

module ModePolymorphismWorksWithAllocation = struct
  let (iter_alloc @ alloc) : int list -> unit =
    ModePolymorphic.iter (fun value -> (state := { value } :: !state))
  let (iter_noalloc @ noalloc) : 'a list -> unit =
    ModePolymorphic.iter (fun _ -> ())
end
[%%expect{|
module ModePolymorphismWorksWithAllocation :
  sig
    val iter_alloc : int list -> unit
    val iter_noalloc : 'a list -> unit @@ noalloc
  end
|}]

module ModePolymorphismIsStillSound = struct
  let (iter_bad @ noalloc) : int list -> unit =
    ModePolymorphic.iter (fun value -> (state := { value } :: !state))
end
[%%expect{|
CR wsturgeon for wsturgeon: `iter_bad` needs to be rejected
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
CR wsturgeon for wsturgeon: this needs to be rejected, but it's currently accepted
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
