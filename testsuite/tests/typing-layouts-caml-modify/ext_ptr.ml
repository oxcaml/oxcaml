(* TEST
 modules = "replace_caml_modify.c";
 {
   not-macos;
   (* Remove layout_beta here when block indices are out of beta *)
   flags = "-extension layouts_beta \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* This test verifies that the ext_ptr primitives never call caml_modify. Like
   basics.ml, it uses the --wrap argument to the C compiler to wrap caml_modify
   and caml_modify_local (defined in replace_caml_modify.c) and track whether
   they have been called.

   Note: caml_modify is always called in bytecode, so this test is only
   performed on native. *)

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

(* The ext_ptr primitives behave as if the base were null, so they always store
   off-heap and don't call caml_modify. *)

module Int64_u = struct
  external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
  external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
  let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))
end

external addr_of_value : ('a : value_or_null). 'a @ local -> int64#
  = "" "caml_native_pointer_of_value"

external unsafe_set_ext_ptr : ('b : any).
  int64# -> ('b[@local_opt]) -> unit = "%unsafe_set_ext_ptr"
[@@layout_poly]

(* Immediate field: no write barrier needed regardless. *)
let () =
  let open struct
    type t = { x : string; mutable y : int }
  end in
  let t = { x = "x"; y = 0 } in
  let y_addr = Int64_u.add (addr_of_value t) #8L in
  test ~expect_caml_modifies:0
    (fun () -> unsafe_set_ext_ptr y_addr 1;
               ignore (Sys.opaque_identity t))

(* Non-scannable flat field. *)
let () =
  let open struct
    type t = { x : string; mutable y : int64# }
  end in
  let t = { x = "x"; y = #0L } in
  let y_addr = Int64_u.add (addr_of_value t) #8L in
  test ~expect_caml_modifies:0
    (fun () -> unsafe_set_ext_ptr y_addr #1L;
               ignore (Sys.opaque_identity t))

(* Scannable pointer field: the regular ptr primitives would call caml_modify
   here, but ext_ptr stores off-heap and must not. *)
let () =
  let open struct
    type t = { x : int; mutable y : string }
  end in
  let t = { x = 0; y = "a" } in
  let y_addr = Int64_u.add (addr_of_value t) #8L in
  test ~expect_caml_modifies:0
    (fun () -> unsafe_set_ext_ptr y_addr "b";
               ignore (Sys.opaque_identity t));
  assert (t.y = "b")

(* All-value product of scannable pointers (non-mixed, so a raw address works):
   two stores, neither via caml_modify. *)
let () =
  let open struct
    type pair = #{ a : string; b : string }
    type t = { x : int; mutable y : pair }
  end in
  let t = { x = 0; y = #{ a = "a"; b = "b" } } in
  let y_addr = Int64_u.add (addr_of_value t) #8L in
  test ~expect_caml_modifies:0
    (fun () -> unsafe_set_ext_ptr y_addr #{ a = "c"; b = "d" };
               ignore (Sys.opaque_identity t));
  let #{ a; b } = t.y in
  assert (a = "c" && b = "d")
