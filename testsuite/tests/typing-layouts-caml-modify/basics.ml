(* TEST
 modules = "replace_caml_modify.c";
 {
   not-macos;
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* This test verifies that external_ and external64 prevent calls to caml_modify when
   appropriate. This is done by utilizing the --wrap argument to the C compiler, which
   allows us to wrap caml_modify and caml_modify_local. replace_caml_modify.c defines
   these wrappers, which track whether caml_modify/caml_modify_local has been called.

   Note: caml_modify is always called in bytecode, so this test is only performed on
   native *)

external called_caml_modify : unit -> int = "replace_caml_modify_called_modify" [@@noalloc]
external reset : unit -> unit = "replace_caml_modify_reset" [@@noalloc]

(* Test whether executing f results in caml_modify being called *)
let test ~(call_pos : [%call_pos]) ~expect_caml_modifies f =
  reset ();
  f ();
  let actual_modifies = called_caml_modify () in
  if not (expect_caml_modifies = actual_modifies) then
    failwith @@
      Format.sprintf
        "On line %d, expected %d calls to caml_modify, but saw %d"
        call_pos.pos_lnum expect_caml_modifies actual_modifies

(* Validate testing technique *)

let () =
  test ~expect_caml_modifies:1 (fun () ->
    let foo = Array.make 1 "hello" in
    foo.(0) <- "world")

let () =
  test ~expect_caml_modifies:2 (fun () ->
    let foo = Array.make 1 "hello" in
    foo.(0) <- "world";
    foo.(0) <- "")

let () =
  test ~expect_caml_modifies:0 (fun () ->
    let foo = Array.make 1 0 in
    foo.(0) <- 0)

(* Some type definitions for below tests *)

type 'a boxed_variant = Boxed of 'a
type 'a unboxed_variant = Unboxed of 'a [@@unboxed]
module External_variant : sig
  type t : value mod external_
  val make : t
end = struct
  type t = External
  let make = External
end
type 'a unboxed_record = { unboxed : 'a } [@@unboxed]
type 'a internal_record = { boxed : 'a }

(* Internal values result in caml_modify calls *)
let () =
  let f (type t) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let expect_caml_modifies = 1 in
  test ~expect_caml_modifies (fun () -> f 10);
  test ~expect_caml_modifies (fun () -> f "hello");
  test ~expect_caml_modifies (fun () -> f true);
  test ~expect_caml_modifies (fun () -> f { boxed = 10 });
  test ~expect_caml_modifies (fun () -> f { boxed = "hello" });
  test ~expect_caml_modifies (fun () -> f { unboxed = 10 });
  test ~expect_caml_modifies (fun () -> f { unboxed = "hello" });
  test ~expect_caml_modifies (fun () -> f External_variant.make);
  test ~expect_caml_modifies (fun () -> f (Boxed 10));
  test ~expect_caml_modifies (fun () -> f (Boxed "hello"));
  test ~expect_caml_modifies (fun () -> f (Unboxed 10));
  test ~expect_caml_modifies (fun () -> f (Unboxed "hello"))

(* External values result in no caml_modify calls *)
let () =
  let f (type (t : value mod external_)) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let expect_caml_modifies = 0 in
  test ~expect_caml_modifies (fun () -> f 10);
  test ~expect_caml_modifies (fun () -> f true);
  test ~expect_caml_modifies (fun () -> f { unboxed = 10 });
  test ~expect_caml_modifies (fun () -> f External_variant.make);
  test ~expect_caml_modifies (fun () -> f (Unboxed 10))

(* Inlining a function that takes internal values should result in no caml_modify call
   for external values *)
let () =
  let[@inline always] f (type t) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let expect_caml_modifies = 1 in
  (* CR layouts v2.8: should have no caml_modify call *)
  test ~expect_caml_modifies (fun () -> f 10);
  test ~expect_caml_modifies (fun () -> f "hello");
  (* CR layouts v2.8: should have no caml_modify call *)
  test ~expect_caml_modifies (fun () -> f true);
  test ~expect_caml_modifies (fun () -> f { boxed = 10 });
  test ~expect_caml_modifies (fun () -> f { boxed = "hello" });
  (* CR layouts v2.8: should have no caml_modify call *)
  test ~expect_caml_modifies (fun () -> f { unboxed = 10 });
  test ~expect_caml_modifies (fun () -> f { unboxed = "hello" });
  (* CR layouts v2.8: should have no caml_modify call *)
  test ~expect_caml_modifies (fun () -> f External_variant.make);
  test ~expect_caml_modifies (fun () -> f (Boxed 10));
  test ~expect_caml_modifies (fun () -> f (Boxed "hello"));
  (* CR layouts v2.8: should have no caml_modify call *)
  test ~expect_caml_modifies (fun () -> f (Unboxed 10));
  test ~expect_caml_modifies (fun () -> f (Unboxed "hello"))

(* External64 values result in no caml_modify calls iff the system is 64 bit *)
let () =
  let f (type (t : value mod external_)) (x : t) =
    let foo = Array.make 1 x in
    foo.(0) <- x
  in
  let is_64_bit =
    match Sys.word_size with
    | 64 -> true
    | 32 ->
      (* This case is never excersized because native tests are never run on a 32-bit
         system *)
      false
    | _ -> failwith "Expected word size of 32 or 64"
  in
  let expect_caml_modifies = if is_64_bit then 0 else 1 in
  test ~expect_caml_modifies (fun () -> f 10);
  test ~expect_caml_modifies (fun () -> f true);
  test ~expect_caml_modifies (fun () -> f { unboxed = 10 });
  test ~expect_caml_modifies (fun () -> f External_variant.make);
  test ~expect_caml_modifies (fun () -> f (Unboxed 10))

(* Bug: Unboxed product updates in records result in one modify per product
   element, even if that element is external.

   To be fixed in a later commit.
*)

(* Record modification with shallow external product *)
let () =
  let open struct
    type outer = { mutable x : #(int * int) }
  end in
  test ~expect_caml_modifies:2 (* should be 0 *)
  (fun () ->
    let outer = { x = #(1, 2) } in
    outer.x <- #(3, 4);
    ignore (Sys.opaque_identity outer)
  )


let () =
  let open struct
    type inner = #{ a : int; b : int }
    type outer = { mutable x : inner }
  end in
  test ~expect_caml_modifies:2 (* should be 0 *)
  (fun () ->
    let outer = { x = #{ a = 1; b = 2 } } in
    outer.x <- #{ a = 3; b = 4 };
    ignore (Sys.opaque_identity outer)
  )

let () =
  let open struct
    type inner = { a : int; b : int }
    type outer = { mutable x : inner# }
  end in
  test ~expect_caml_modifies:2 (* should be 0 *)
  (fun () ->
    let outer = { x = #{ a = 1; b = 2 } } in
    outer.x <- #{ a = 3; b = 4 };
    ignore (Sys.opaque_identity outer)
  )

(* Record modification with nested external product *)
let () =
  let open struct
    type outer = { mutable x : #(int * #(int * bool)) }
  end in
  test ~expect_caml_modifies:3 (* should be 0 *)
  (fun () ->
    let outer = { x = #(1, #(2, true)) } in
    outer.x <- #(3, #(4, false));
    ignore (Sys.opaque_identity outer)
  )

let () =
  let open struct
    type inner1 = #{ a : int; b : int }
    type inner2 = #{ c : int; d : inner1 }
    type outer = { mutable x : inner2 }
  end in
  test ~expect_caml_modifies:3 (* should be 0 *)
  (fun () ->
    let outer = { x = #{ c = 1; d = #{ a = 2; b = 3 } } } in
    outer.x <- #{ c = 4; d = #{ a = 5; b = 6 } };
    ignore (Sys.opaque_identity outer)
  )

let () =
  let open struct
    type inner1 = { a : int; b : int }
    type inner2 = { c : int; d : inner1# }
    type outer = { mutable x : inner2#}
  end in
  test ~expect_caml_modifies:3 (* should be 0 *)
  (fun () ->
    let outer = { x = #{ c = 1; d = #{ a = 2; b = 3 } } } in
    outer.x <- #{ c = 4; d = #{ a = 5; b = 6 } };
    ignore (Sys.opaque_identity outer)
  )

(* Record modification with nested mixed product *)
let () =
  let open struct
    type outer =
      { mutable x : #(int * #(int * #(string * bool) * #(bool option * char))) }
  end in
  test ~expect_caml_modifies:6 (* should be 2 *)
  (fun () ->
    let outer = { x = #(1, #(2, #("a", true), #(Some true, 'a'))) } in
    outer.x <- #(3, #(4, #("b", false), #(None, 'b')));
    ignore (Sys.opaque_identity outer)
  )

let () =
  let open struct
    type inner1 = #{ a : string; b : bool }
    type inner2 = #{ c : bool option; d : char }
    type inner3 = #{ e : int; f : inner1; g : inner2 }
    type inner4 = #{ h : int; i : inner3 }
    type outer = { mutable x : inner4 }
  end in
  test ~expect_caml_modifies:6 (* should be 2 *)
  (fun () ->
    let outer =
      { x = #{ h = 1; i = #{ e = 2;
                             f = #{ a = "a"; b = true };
                             g = #{ c = Some true; d = 'a' } } } }
    in
    outer.x <- #{ h = 3; i = #{ e = 4;
                                f = #{ a = "b"; b = false };
                                g = #{ c = None; d = 'b' } } };
    ignore (Sys.opaque_identity outer)
  )

let () =
  let open struct
    type inner1 = { a : string; b : bool }
    type inner2 = { c : bool option; d : char }
    type inner3 = { e : int; f : inner1#; g : inner2# }
    type inner4 = { h : int; i : inner3# }
    type outer = { mutable x : inner4# }
  end in
  test ~expect_caml_modifies:6 (* should be 2 *)
  (fun () ->
    let outer =
      { x = #{ h = 1; i = #{ e = 2;
                             f = #{ a = "a"; b = true };
                             g = #{ c = Some true; d = 'a' } } } }
    in
    outer.x <- #{ h = 3; i = #{ e = 4;
                                f = #{ a = "b"; b = false };
                                g = #{ c = None; d = 'b' } } };
    ignore (Sys.opaque_identity outer)
  )
