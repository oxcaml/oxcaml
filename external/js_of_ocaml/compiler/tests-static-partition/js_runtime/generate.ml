(* Build a shared runtime for a fixed set of primitives, then check that invalid
   inputs fail. check.js runs the result. *)

open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
module Runtime = Jsoo_static_partition.Js_runtime

let () =
  Config.set_target `JavaScript;
  Config.set_effects_backend `Disabled;
  Config.Flag.disable "pretty";
  Config.Flag.disable "header";
  Config.Flag.enable "with-js-error";
  let runtime_files = [ "runtime_stubs.js"; "always.js" ] in
  let build primitives fs_files static_env =
    Runtime.build_exn ~primitives ~runtime_files ~fs_files ~static_env ~profile:Profile.O3
  in
  let main =
    StringSet.of_list
      [ "runtime_probe_main"
      ; "caml_get_global"
      ; "caml_jsstring_of_string"
      ; "caml_string_of_jsbytes"
      ; "caml_sys_getenv"
      ; "caml_read_file_content"
      ]
  in
  let lazy_ = StringSet.of_list [ "runtime_probe_lazy"; "runtime_probe_alias" ] in
  let result =
    build
      (StringSet.union main lazy_)
      [ "revision.txt:/static/hg_version.out"; "build_info.sexp:/static/build_info.sexp" ]
      [ "FORCE_DROP_INLINE_TEST", "true"; "FORCE_DROP_BENCH", "true" ]
  in
  Out_channel.with_open_bin "shared.runtime.js" (fun channel ->
      Out_channel.output_string channel result.javascript);
  List.iter
    [ "caml_register_global_predef"; "caml_set_static_env"; "jsoo_create_file" ]
    ~f:(fun primitive ->
      if not (StringSet.mem primitive result.primitives)
      then failwith ("Missing bootstrap primitive " ^ primitive));
  let must_fail f =
    match f () with
    | (_ : Runtime.t) -> failwith "Expected invalid runtime inputs to fail"
    | exception Failure _ -> ()
  in
  must_fail (fun () -> build (StringSet.singleton "runtime_probe_nonexistent") [] []);
  must_fail (fun () -> build StringSet.empty [ "nonexistent-file:/static/missing" ] [])
