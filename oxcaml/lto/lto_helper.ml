(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* Helper for the link time optimisation rules in this directory: it runs the
   Reaper's rebuild phase over the units of one library and re-archives them.

   -reaper-rebuild writes its output next to its input, so the inputs are first
   copied out of the directories Dune owns. The rules know from a glob which
   .cmx files belong to a library, but not the order in which those units must
   be archived nor the C libraries the archive records; both are recovered here
   from the library's ordinary .cmxa. *)

let fatal fmt =
  Printf.ksprintf
    (fun msg ->
      prerr_endline ("lto_helper: " ^ msg);
      exit 2)
    fmt

(* The context asks for -support-lto through OCAMLPARAM; the compilations
   driven from here consume that information rather than producing more of it,
   so it is cleared for the children. *)
let run prog args =
  let cmd = "OCAMLPARAM= " ^ Filename.quote_command prog args in
  match Sys.command cmd with 0 -> () | n -> fatal "exited with %d:\n%s" n cmd

let rec remove_dir dir =
  Array.iter
    (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then remove_dir path else Sys.remove path)
    (Sys.readdir dir);
  Sys.rmdir dir

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let oc = open_out_bin dst in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          let len = 512 * 1024 in
          let buf = Bytes.create len in
          let rec loop () =
            match input ic buf 0 len with
            | 0 -> ()
            | n ->
              output oc buf 0 n;
              loop ()
          in
          loop ()))

let reaped name = Filename.remove_extension name ^ ".reaped.cmx"

(* Rebuild [members] against [ltosol] in a scratch directory of their own, and
   return the resulting .reaped.cmx files, in the same order. One invocation
   covers the whole batch: its members then share the compiler's caches. *)
let rebuild ~ocamlopt ~ltosol ~dir ~members ~f =
  if Sys.file_exists dir then remove_dir dir;
  Sys.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> remove_dir dir)
    (fun () ->
      let copies =
        List.map
          (fun cmx ->
            let dst = Filename.concat dir (Filename.basename cmx) in
            copy_file ~src:cmx ~dst;
            dst)
          members
      in
      run ocamlopt (("-reaper-rebuild" :: copies) @ [ltosol]);
      f (List.map reaped copies))

(* The name Dune gives the .cmx of a compilation unit: its module name with a
   lowercase initial. *)
let cmx_basename unit =
  String.uncapitalize_ascii (Compilation_unit.full_path_as_string unit) ^ ".cmx"

let rebuild_archive ~ocamlopt ~ltosol ~archive ~output ~cmxs =
  let infos = Compilenv.read_library_info archive in
  let by_basename =
    List.fold_left
      (fun acc cmx -> Misc.Stdlib.String.Map.add (Filename.basename cmx) cmx acc)
      Misc.Stdlib.String.Map.empty cmxs
  in
  let members =
    List.map
      (fun (unit : Cmx_format.lib_unit_info) ->
        let basename = cmx_basename unit.li_name in
        match Misc.Stdlib.String.Map.find_opt basename by_basename with
        | Some cmx -> cmx
        | None ->
          fatal "no %s among the %d .cmx files given for %s" basename
            (List.length cmxs) archive)
      infos.lib_units
  in
  rebuild ~ocamlopt ~ltosol ~dir:(output ^ ".rebuild") ~members ~f:(fun reaped ->
      let opt flag opts = List.concat_map (fun o -> [flag; o]) (List.rev opts) in
      run ocamlopt
        (["-a"; "-o"; output]
        @ reaped
        @ opt "-cclib" infos.lib_ccobjs
        @ opt "-ccopt" infos.lib_ccopts))

let rebuild_unit ~ocamlopt ~ltosol ~output ~cmxs =
  match cmxs with
  | [cmx] ->
    rebuild ~ocamlopt ~ltosol ~dir:(output ^ ".rebuild") ~members:[cmx]
      ~f:(fun reaped ->
        let reaped = List.hd reaped in
        let obj name = Filename.remove_extension name ^ ".o" in
        copy_file ~src:reaped ~dst:output;
        copy_file ~src:(obj reaped) ~dst:(obj output))
  | _ ->
    fatal "without -archive, exactly one .cmx is expected (got %d)"
      (List.length cmxs)

let () =
  let ocamlopt = ref "" and ltosol = ref "" and archive = ref "" in
  let output = ref "" and cmxs = ref [] in
  let usage =
    "Usage: lto_helper -ocamlopt <exe> -ltosol <file> [-archive <lib.cmxa>] \
     -o <output> <file>...\n\
     Anonymous arguments that are not .cmx files are ignored, so that the \
     rule may simply pass %{deps}."
  in
  let args =
    [ "-ocamlopt", Arg.Set_string ocamlopt, "<exe> The compiler to drive";
      ( "-ltosol",
        Arg.Set_string ltosol,
        "<file> The Reaper solution to rebuild against" );
      ( "-archive",
        Arg.Set_string archive,
        "<file> The library's ordinary .cmxa, which fixes the order of its \
         members and the C libraries it records; without it a single unit is \
         rebuilt and left as a .cmx" );
      "-o", Arg.Set_string output, "<file> The .cmxa or .cmx to produce" ]
  in
  Arg.parse args
    (fun file ->
      if Filename.check_suffix file ".cmx" then cmxs := file :: !cmxs)
    usage;
  List.iter
    (fun (name, value) -> if !value = "" then fatal "missing %s\n%s" name usage)
    ["-ocamlopt", ocamlopt; "-ltosol", ltosol; "-o", output];
  let cmxs = List.rev !cmxs in
  if !archive = ""
  then rebuild_unit ~ocamlopt:!ocamlopt ~ltosol:!ltosol ~output:!output ~cmxs
  else
    rebuild_archive ~ocamlopt:!ocamlopt ~ltosol:!ltosol ~archive:!archive
      ~output:!output ~cmxs
