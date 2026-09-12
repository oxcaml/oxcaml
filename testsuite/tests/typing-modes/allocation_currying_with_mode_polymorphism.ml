(* TEST
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

module CurriedNoalloc = struct
  (* Boxed record: creation allocates a block on the heap. *)
  type t = { mutable value : int }
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
  (* Boxed record: creation allocates a block on the heap. *)
  type t = { mutable value : int }
  (* A function returning (a function returning (a block on the heap)). *)
  let make_make () () = { value = 42 }
  (* Instantiate only the outer function, correctly annotating `alloc`. *)
  let make @ alloc = make_make ()
  (* Allocates, just like it said it could. *)
  let this_is_fine : t @ global = make ()
end
[%%expect{|
module CurriedAlloc :
  sig
    type t = { mutable value : int; }
    val make_make : unit @ [< global] -> unit @ 'n -> t @ 'm
    val make : unit -> t @ 'm
    val this_is_fine : t @@ noalloc_strict
  end @@ stateless
|}]

module CallbackExclaveNoalloc = struct
  type t = { mutable value : int }

  (* This function, once applied to `unit`, accepts a callback,
     then passes a newly allocated record to that callback
     and returns the result (of an arbitrary type). *)
  let make_pass_new_record_to_callback () =
    exclave_ fun (callback @ noalloc_strict) ->
      callback { value = 42 } [@nontail]

  (* This callback returns its argument unchanged.
     it's `noalloc` because it doesn't do any *additional* allocation,
     although its arguments may be allocated on the heap. *)
  let (do_nothing @ noalloc_strict) (unchanged : t @ global) : t @ global =
    unchanged

  let secretly_allocated =
    (* Abuse mode polymorphism on the partially applied function. *)
    let pass_new_record_to_callback @ local noalloc_strict =
      make_pass_new_record_to_callback ()
    in
    (* Inexplicably pause trading. *)
    pass_new_record_to_callback do_nothing [@nontail]
end
[%%expect{|
CR wsturgeon for wsturgeon: this needs to be rejected, but it's currently accepted
|}]

module CallbackExclaveAlloc = struct
  type t = { mutable value : int }

  (* This function, once applied to `unit`, accepts a callback,
     then passes a newly allocated record to that callback
     and returns the result (of an arbitrary type). *)
  let make_pass_new_record_to_callback () =
    exclave_ fun (callback @ noalloc_strict) ->
      callback { value = 42 } [@nontail]

  (* This callback returns its argument unchanged.
     it's `noalloc` because it doesn't do any *additional* allocation,
     although its arguments may be allocated on the heap. *)
  let (do_nothing @ noalloc_strict) (unchanged : t @ global) : t @ global =
    unchanged

  let this_is_fine =
    (* This is correctly `alloc`, but note that
        the two above are still (correctly) `noalloc_strict`! *)
    let pass_new_record_to_callback @ local alloc =
      make_pass_new_record_to_callback ()
    in
    (* Allocates, just like it said it could. *)
    pass_new_record_to_callback do_nothing [@nontail]
end
[%%expect{|
module CallbackExclaveAlloc :
  sig
    type t = { mutable value : int; }
    val make_pass_new_record_to_callback :
      unit @ 'o ->
      (t @ 'n -> 'a @ [< 'm & global]) @ [< noalloc_strict] ->
      'a @ [> 'm | dynamic]
    val do_nothing : t @ [< global] -> t @ [< global] @@ noalloc_strict
    val this_is_fine : t @@ noalloc_strict
  end @@ stateless
|}]

module CallbackNoalloc = struct
  type t = { mutable value : int }

  (* This function, once applied to `unit`, accepts a callback,
     then passes a newly allocated record to that callback
     and returns the result (of an arbitrary type). *)
  let make_pass_new_record_to_callback () (callback @ noalloc_strict) =
    callback { value = 42 } [@nontail]

  (* This callback returns its argument unchanged.
     it's `noalloc` because it doesn't do any *additional* allocation,
     although its arguments may be allocated on the heap. *)
  let (do_nothing @ noalloc_strict) (unchanged : t @ global) : t @ global =
    unchanged

  let secretly_allocated =
    (* Abuse mode polymorphism on the partially applied function. *)
    let pass_new_record_to_callback @ local noalloc_strict =
      make_pass_new_record_to_callback ()
    in
    (* Inexplicably pause trading. *)
    pass_new_record_to_callback do_nothing [@nontail]
end
[%%expect{|
CR wsturgeon for wsturgeon: this needs to be rejected, but it's currently accepted
|}]

module CallbackAlloc = struct
  type t = { mutable value : int }

  (* This function, once applied to `unit`, accepts a callback,
     then passes a newly allocated record to that callback
     and returns the result (of an arbitrary type). *)
  let make_pass_new_record_to_callback () (callback @ noalloc_strict) =
    callback { value = 42 } [@nontail]

  (* This callback returns its argument unchanged.
     it's `noalloc` because it doesn't do any *additional* allocation,
     although its arguments may be allocated on the heap. *)
  let (do_nothing @ noalloc_strict) (unchanged : t @ global) : t @ global =
    unchanged

  let this_is_fine =
    (* This is correctly `alloc`, but note that
        the two above are still (correctly) `noalloc_strict`! *)
    let pass_new_record_to_callback @ local alloc =
      make_pass_new_record_to_callback ()
    in
    (* Allocates, just like it said it could. *)
    pass_new_record_to_callback do_nothing [@nontail]
end
[%%expect{|
module CallbackAlloc :
  sig
    type t = { mutable value : int; }
    val make_pass_new_record_to_callback :
      unit @ [< global] ->
      (t @ 'n -> 'a @ [< 'm & global]) @ [< noalloc_strict] ->
      'a @ [> 'm | dynamic]
    val do_nothing : t @ [< global] -> t @ [< global] @@ noalloc_strict
    val this_is_fine : t @@ noalloc_strict
  end @@ stateless
|}]
