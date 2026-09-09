(* TEST
 readonly_files = "participant_dce_dep.ml participant_external.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-opaque";
 compile_only = "true";
 all_modules = "participant_external.ml";
 ocamlopt.opt;

 flags = "-flambda2-reaper -support-lto -opaque";
 all_modules = "participant_dce_dep.ml";
 ocamlopt.opt;
 all_modules = "participant_dce.ml";
 ocamlopt.opt;

 script = "grep -a -q LTO_DEAD_PARTICIPANT_EXPORT participant_dce_dep.o";
 script;

 compile_only = "false";
 flags = "-reaper-solve participant_dce.cmr participant_dce_dep.cmr";
 last_flags = "-o participant_dce.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild participant_dce.cmr participant_dce.ltosol";
 last_flags = "";
 ocamlopt.opt;
 flags = "-reaper-rebuild participant_dce_dep.cmr participant_dce.ltosol";
 ocamlopt.opt;

 script = "sh -c 'grep -a -q LTO_DEAD_PARTICIPANT_EXPORT participant_dce_dep.reaped.o; test $? -eq 1'";
 script;

 flags = "";
 all_modules = "participant_external.cmx participant_dce_dep.reaped.cmx participant_dce.reaped.cmx";
 program = "${test_build_directory}/participant_dce.exe";
 ocamlopt.opt;
 run;
 check-program-output;
*)

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

(* Opaque compilation leaves cross-unit calls indirect. This checks graph
   joining independently of the forthcoming solve-time code metadata changes. *)
let () = Participant_external.run Participant_dce_dep.used
