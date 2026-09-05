(* TEST
 modules = "replace_caml_modify.c";
 {
   not-macos;
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* These tests verify that abstract types with layout value non_pointer don't
   unnecessarily call [caml_modify]. See [basics.ml] for an explanation. *)

external called_caml_modify : unit -> int
  = "replace_caml_modify_called_modify" [@@noalloc]
external reset : unit -> unit = "replace_caml_modify_reset" [@@noalloc]

let test ~(call_pos : [%call_pos]) ~expect_caml_modifies f =
  reset ();
  f ();
  let actual_modifies = called_caml_modify () in
  if not (expect_caml_modifies = actual_modifies) then
    failwith @@
      Format.sprintf
        "On line %d, expected %d calls to caml_modify, but saw %d"
        call_pos.pos_lnum expect_caml_modifies actual_modifies

external[@layout_poly] set :
  ('a : any mod separable).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  = "%array_safe_set"

external[@layout_poly] unsafe_set_ptr :
  'a ('b : any).
  (#('a * ('a, 'b) idx_mut)[@local_opt]) -> ('b[@local_opt]) -> unit
  = "%unsafe_set_ptr"


(* Mutating abstract types of kind value non_pointer should skip caml_modify *)

module Mnp : sig
  type t : value non_pointer (* not mod external_ *)
  val mk : int -> t
end = struct
  type t = int
  let mk x = x
end

let () =
  let open struct
    type t = { mutable x : Mnp.t }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let t = { x = Mnp.mk 6 } in
      t.x <- Mnp.mk 7;
      ignore (Sys.opaque_identity t))

let () =
  let open struct
    type t = { x : string ; mutable y : Mnp.t }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let t = { x = "a" ; y = Mnp.mk 1 } in
      let idx = (.y) in
      unsafe_set_ptr #(t, idx) (Mnp.mk 2);
      ignore (Sys.opaque_identity t))

let () =
  test ~expect_caml_modifies:0
    (fun () ->
      let arr = Array.make 1 (Mnp.mk 6) in
      arr.(0) <- Mnp.mk 7;
      ignore (Sys.opaque_identity arr))

(* A product containing only value non_pointers should skip all caml_modifies *)

module Mnpnp : sig
  type t : value non_pointer & value non_pointer
  val mk : int -> int -> t
end = struct
  type t = #{ x : int; y : int }
  let mk x y = #{ x ; y }
end

let () =
  let open struct
    type outer = { mutable x : Mnpnp.t }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let outer = { x = Mnpnp.mk 1 2 } in
      outer.x <- Mnpnp.mk 3 4;
      ignore (Sys.opaque_identity outer))

let () =
  let open struct
    type ('a : value non_pointer & value non_pointer) unboxed = { u : 'a } [@@unboxed]
    type outer = { mutable x : Mnpnp.t unboxed; }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let outer = { x = { u = Mnpnp.mk 1 2} } in
      outer.x <- { u = Mnpnp.mk 3 4 };
      ignore (Sys.opaque_identity outer))

let () =
  let open struct
    type inner = { a : int; b : Mnpnp.t }
    type outer = { mutable x : inner# }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let outer = { x = #{ a = 1; b = Mnpnp.mk 2 3 } } in
      outer.x <- #{ a = 4; b = Mnpnp.mk 5 6 };
      ignore (Sys.opaque_identity outer))

let () =
  let open struct
    type t = { x : string; mutable y : Mnpnp.t }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let t = { x = "x"; y = Mnpnp.mk 1 2 } in
      let idx = (.y) in
      unsafe_set_ptr #(t, idx) (Mnpnp.mk 3 4);
      ignore (Sys.opaque_identity t))

let () =
  test ~expect_caml_modifies:0
    (fun () ->
      let arr = [| (Mnpnp.mk 1 2) |] in
      set arr 0 (Mnpnp.mk 3 4);
      ignore (Sys.opaque_identity arr))

let () =
  let open struct
    type 'a t = { mutable x : 'a or_null }
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let t = { x = This 1 } in
      t.x <- This 2;
      ignore (Sys.opaque_identity t))

(* A product containing one value non_pointer component should skip caml_modify
   for that one component *)

module Mnpval : sig
  type t : value non_pointer & value
  val mk : int -> string -> t
end = struct
  type t = #{ x : int; y : string }
  let mk x y = #{ x ; y }
end

let () =
  let open struct
    type t = { mutable x : Mnpval.t }
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let outer = { x = Mnpval.mk 1 "a" } in
      outer.x <- Mnpval.mk 2 "b";
      ignore (Sys.opaque_identity outer))

let () =
  let open struct
    type inner = { a : string; b : Mnpval.t }
    type outer = { mutable x : inner# }
  end in
  test ~expect_caml_modifies:2
    (fun () ->
      let outer = { x = #{ a = "a"; b = Mnpval.mk 1 "b" } } in
      outer.x <- #{ a = "c"; b = Mnpval.mk 2 "d" };
      ignore (Sys.opaque_identity outer))

let () =
  let open struct
    type t = { x : string; mutable y : Mnpval.t }
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let t = { x = "x"; y = Mnpval.mk 1 "a" } in
      let idx = (.y) in
      unsafe_set_ptr #(t, idx) (Mnpval.mk 2 "b");
      ignore (Sys.opaque_identity t))

let () =
  test ~expect_caml_modifies:1
    (fun () ->
      let arr = [| (Mnpval.mk 1 "a") |] in
      set arr 0 (Mnpval.mk 2 "b");
      ignore (Sys.opaque_identity arr))

(* interaction with mixed modules *)
module type MT = sig
  type t : value non_pointer & value
  val t1 : t
  val t2 : t
end

let () =
  let module M : MT = struct
    type t = #{ x : int; y : string }
    let t1 = #{ x = 1; y = "a" }
    let t2 = #{ x = 2; y = "b" }
  end in
  let open struct
    type t = { mutable x : M.t }
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let outer = { x = M.t1 } in
      outer.x <- M.t2;
      ignore (Sys.opaque_identity outer))

let () =
  let m =
    (module struct
      type t = #{ x : int; y : string }
      let t1 = #{ x = 1; y = "a" }
      let t2 = #{ x = 2; y = "b" }
    end : MT)
  in
  let module M = (val m : MT) in
  let open struct
    type t = { mutable x : M.t }
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let outer = { x = M.t1 } in
      outer.x <- M.t2;
      ignore (Sys.opaque_identity outer))

(* Interaction with substitution *)

let () =
  test ~expect_caml_modifies:1
    (fun () ->
      let arr = [| (Mnpval.mk 1 "a") |] in
      set arr 0 (Mnpval.mk 2 "b");
      ignore (Sys.opaque_identity arr))

let () =
  let open struct
    module M : sig
      type t : value & value
      type r = { mutable t : t; i : int64# }
    end with type t := #(int * string) = struct
      type r = { mutable t : #(int * string); i : int64# }
    end
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let t = { M.t = #(1, "1"); M.i = #1L } in
      t.t <- #(2, "2");
      ignore (Sys.opaque_identity t))

let () =
  test ~expect_caml_modifies:1
    (fun () ->
      let arr = [| (Mnpval.mk 1 "a") |] in
      set arr 0 (Mnpval.mk 2 "b");
      ignore (Sys.opaque_identity arr))

(* CR layouts-scannable: Record representations can be stale after type
   substitution, causing an unnecessary caml_modify *)
let () =
  let open struct
    module IS : sig
      type t : value non_pointer & value
      val a : t
      val b : t
    end = struct
      type t = #(int * string)
      let a = #(1, "1")
      let b = #(2, "2")
    end

    module M : sig
      type r = { mutable t : IS.t }
    end = struct
      type r = { mutable t : IS.t }
    end

    module M_with_subst : sig
      type t : value & value
      (* When computing the record representation for [r], the compiler hasn't
         yet seen the sustitution yet, and it doesn't update the record
         representation upon substituting, so it thinks [r] is a mixed block
         containing a [value & value]. *)
      type r = { mutable t : t }
    end with type t := IS.t = struct
      type r = { mutable t : IS.t }
    end
  end in
  test ~expect_caml_modifies:1
    (fun () ->
      let t = { M.t = IS.a } in
      t.t <- IS.b;
      ignore (Sys.opaque_identity t));
  test ~expect_caml_modifies:2
    (fun () ->
      let t = { M_with_subst.t = IS.a } in
      t.t <- IS.b;
      ignore (Sys.opaque_identity t))

(* CR layouts-scananble: Test this once abstract kinds can be substituted for
   subkinds *)
(*
let () =
  let open struct
    module M : sig
      kind_ k = value
      type t : k
      val x : t
      val y : t
      type r = { mutable t : t; i : int64# }
    end with kind_ k := value non_pointer = struct
      type t = int
      type r = { mutable t : t; i : int64# }
      let x = 1
      let y = 2
    end
  end in
  test ~expect_caml_modifies:0
    (fun () ->
      let t = { M.t = M.x; M.i = #1L } in
      t.t <- M.y;
      ignore (Sys.opaque_identity t))
*)
