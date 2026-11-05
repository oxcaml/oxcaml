(* TEST
 arch_amd64;
 not-macos;
 readonly_files = "lpoly_lib.mli lpoly_lib.ml user1.ml user2.ml user3.ml";
 setup-ocamlopt.byte-build-env;
 {
  flags = "-extension layout_poly_alpha -nocwd -Ix .";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  module = "user1.ml";
  ocamlopt.byte;
  module = "user2.ml";
  ocamlopt.byte;
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }{
  flags = "-extension layout_poly_alpha -nocwd -Ix . -Oclassic";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  module = "user1.ml";
  ocamlopt.byte;
  module = "user2.ml";
  ocamlopt.byte;
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }{
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g";
  module = "lpoly_lib.mli";
  ocamlopt.byte;
  module = "lpoly_lib.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g -O3";
  module = "user1.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g -Oclassic";
  module = "user2.ml";
  ocamlopt.byte;
  flags = "-extension layout_poly_alpha -nocwd -Ix . -g";
  module = "user3.ml";
  ocamlopt.byte;
  module = "comdat_dedup.ml";
  ocamlopt.byte;
  unset module;
  program = "${test_build_directory}/comdat_dedup.exe";
  all_modules = "lpoly_lib.cmx user1.cmx user2.cmx user3.cmx comdat_dedup.cmx";
  ocamlopt.byte;
  run;
 }
*)

(* [user1.ml] and [user2.ml] both instantiate [Lpoly_lib.lpoly_pair] at
   (float64, float64), so each object file carries a weak copy of the
   instance's code under the cohort's shared symbol. After linking, COMDAT
   deduplication must leave exactly one. [user3.ml] calls [User1]'s instance
   from outside [User1], which only links if that call also uses the shared
   symbol.

   [lpoly_call_between] has a frame descriptor, which must be discarded along
   with each losing copy of the code. The last block compiles [user1.ml] and
   [user2.ml] at different optimization levels, so that the copies differ.
   Calling both instances runs the GC through the surviving copy's frame.
   [lpoly_select] has a jump table, which likewise refers into the code.

   The test runs the linked program and then shells out to [nm] to inspect
   the symbol table, and to [readelf] to check that nothing outside a COMDAT
   group refers into it by section (which would dangle when a losing copy is
   discarded, whether or not the linker notices). *)

external box : float# -> float = "%box_float"

let () =
  let _ = Sys.opaque_identity User1.pair () in
  let _ = Sys.opaque_identity User2.pair () in
  let _ = Sys.opaque_identity User3.call_user1 () in
  let #(a, b) = User1.call_between () in
  let #(c, d) = User2.call_between () in
  assert (box a = 2.0 && box b = 1.0 && box c = 4.0 && box d = 3.0);
  let #(k1, e, f) = User1.select 3 5 in
  let #(k2, g, h) = User2.select 5 6 in
  assert (k1 = 5 lxor 99 && box e = 2.0 && box f = 1.0);
  assert (k2 = 36 && box g = 4.0 && box h = 3.0)

let expected_symbol_res =
  [ "camlLpoly_lib__Lpoly_lib_lpoly_pair_.*float64_float64__cohort_code$";
    "camlLpoly_lib__Lpoly_lib_lpoly_call_between_.*float64_float64__cohort_code$";
    "camlLpoly_lib__Lpoly_lib_lpoly_select_.*float64_float64__cohort_code$" ]

let () =
  let exe = Filename.quote Sys.executable_name in
  let ok = ref true in
  List.iter
    (fun re ->
      let check =
        Printf.sprintf "test $(nm %s | grep -c '%s') -eq 1" exe re
      in
      if Sys.command check <> 0
      then begin
        ok := false;
        Printf.eprintf
          "expected exactly one copy of weak symbol matching /%s/; \
           relevant nm output:\n%!"
          re;
        let dump = Printf.sprintf "nm %s | grep Lpoly_lib >&2 || true" exe in
        ignore (Sys.command dump)
      end)
    expected_symbol_res;
  if not !ok then exit 1

let output_lines cmd ~out =
  if Sys.command (Printf.sprintf "%s > %s" cmd (Filename.quote out)) <> 0
  then failwith ("command failed: " ^ cmd);
  In_channel.with_open_text out In_channel.input_lines

let words s = String.split_on_char ' ' s |> List.filter (fun w -> w <> "")

(* The offsets of the object's sections that are members of a COMDAT group,
   from the "G" flag in [readelf -SW] output. *)
let group_member_offsets obj =
  output_lines ("readelf -SW " ^ Filename.quote obj) ~out:(obj ^ ".sections")
  |> List.filter_map (fun line ->
         match String.index_opt line ']' with
         | None -> None
         | Some i -> (
           let fields =
             words (String.sub line (i + 1) (String.length line - i - 1))
           in
           match fields with
           | [_name; _type; _addr; off; _size; _es; flags; _lk; _inf; _al]
             when String.contains flags 'G' ->
             int_of_string_opt ("0x" ^ off)
           | _ -> None))

(* The sections of a weak function's group: its code and its jump tables. *)
let is_cohort_section_symbol w =
  String.starts_with ~prefix:".text." w
  && (String.ends_with ~suffix:"__cohort_code" w
     || String.ends_with ~suffix:"__cohort_code.jump_tables" w)

(* Relocations from outside any group against a section symbol of a cohort
   group. Linkers special-case unwind and debug information, which may. *)
let bad_relocations obj =
  let in_group = group_member_offsets obj in
  let lines =
    output_lines ("readelf -rW " ^ Filename.quote obj) ~out:(obj ^ ".relocs")
  in
  let _, bad =
    List.fold_left
      (fun (checking, bad) line ->
        match
          Scanf.sscanf_opt line "Relocation section '%s@' at offset %i"
            (fun name off -> name, off)
        with
        | Some (name, off) ->
          let allowed =
            String.equal name ".rela.eh_frame"
            || String.starts_with ~prefix:".rela.debug_" name
            || List.mem off in_group
          in
          (if allowed then None else Some name), bad
        | None -> (
          match checking with
          | Some name when List.exists is_cohort_section_symbol (words line) ->
            checking, (name ^ ":" ^ line) :: bad
          | Some _ | None -> checking, bad))
      (None, []) lines
  in
  List.rev bad

let () =
  let dir = Filename.dirname Sys.executable_name in
  let bad =
    List.concat_map
      (fun obj -> bad_relocations (Filename.concat dir obj))
      ["user1.o"; "user2.o"; "user3.o"]
  in
  if bad <> []
  then begin
    prerr_endline "relocations into a COMDAT group from outside it:";
    List.iter prerr_endline bad;
    exit 1
  end
