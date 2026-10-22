(* TEST
 flags = "-S -function-sections -frametable-sections";
 native-compiler;
 frametable_sections;
 unset OCAMLPARAM;
 native;
*)

(* OCAMLPARAM is unset in these tests because the aarch64 CI jobs set it to
   include verify-binary-emitter=1, which rejects -frametable-sections. *)

(* Check the shape of the assembly and object file emitted under
   -frametable-sections: every frame descriptor is in a caml_frametable piece
   linked (SHF_LINK_ORDER) to a function's text section, bracketed by the
   unit's marker pieces, and the unit's frametable symbol names a header. *)

exception Boom of int

let[@inline never] may_raise n = if n > 2 then raise (Boom n) else n * 2

let[@inline never] guarded n = try may_raise n with Boom m -> m + 100

let[@inline never] alloc n = List.init n (fun i -> string_of_int i)

let () =
  Printf.printf "%d %d %d\n" (guarded 1) (guarded 5)
    (List.length (alloc 10))
