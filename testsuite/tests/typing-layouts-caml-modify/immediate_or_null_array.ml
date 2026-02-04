(* TEST
 modules = "replace_caml_modify.c";
 {
   not-macos;
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* This test verifies that immediate_or_null array operations don't call
   [[caml_modify]. See [basics.ml] for an explanation. *)

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

external[@layout_poly] make_any : ('a : any mod separable).
  int -> 'a -> 'a array = "%makearray_dynamic"

external[@layout_poly] set_any : ('a : any mod separable).
  'a array -> int -> 'a -> unit = "%array_safe_set"

external[@layout_poly] unsafe_set_any : ('a : any mod separable).
  'a array -> int -> 'a -> unit = "%array_unsafe_set"

module Immediate_or_null_variant : sig
  type t : immediate_or_null
  val x1 : t
  val x2 : t
end = struct
  type t = int or_null
  let x1 = Null
  let x2 = This 5
end

let () =
  let f (type t : immediate_or_null) (x1 : t) (x2 : t) =
    let xs = make_any 4 x1 in
    set_any xs 1 x2;
    set_any xs 2 x2
  in
  test ~expect_caml_modifies:0
    (fun () ->
      f Immediate_or_null_variant.x1 Immediate_or_null_variant.x2)

let () =
  let f (type t : immediate_or_null) (x1 : t) (x2 : t) =
    let xs = make_any 4 x1 in
    unsafe_set_any xs 1 x2;
    unsafe_set_any xs 2 x2
  in
  test ~expect_caml_modifies:0
    (fun () ->
      f Immediate_or_null_variant.x1 Immediate_or_null_variant.x2)
