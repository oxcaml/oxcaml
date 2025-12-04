(* TEST
 modules = "replace_caml_modify.c";
 {
   not-macos;
   flags = "-cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify \
            -cclib -Xlinker -cclib --wrap -cclib -Xlinker -cclib caml_modify_local";
   native;
 }
*)

(* This test verifies that setting an immediate_or_null field in a mixed record
   calls [caml_modify]. See [basics.ml] for an explanation. *)

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

type t =
  { mutable a : int or_null
  ; mutable b : int64#
  }

let set t ~a =
  t.a <- a

let () =
  let t = { a = Null; b = #0L } in
  test ~expect_caml_modifies:1
    (fun () -> set t ~a:(This 5))
