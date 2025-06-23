open! Ocaml_ir_fiber
open! Sexplib.Std
open! Ocaml_ir_common.Std
open! Ocaml_ir_common
let find_shape ~filename ~output_prefix ~compiler_flags =
  let languages = Language.Set.singleton Typed_tree in
  Result.Fiber.Let_syntax.Let_syntax.bind
    (Compile.invoke_compiler ~filename ~output_prefix
       ~compiler_flags:("-dshape" :: "-dump-into-file" :: compiler_flags)
       ~languages ~setup_hooks:(fun () -> ()))
    ~f:(fun () ->
          (Fiber.Io.read_file ~path:(output_prefix ^ ".cmx.dump")) |>
            Result.Fiber.ok)
let shape ~filename ~output_prefix ~compiler_flags =
  let extension = Filename.extension filename in
  match File_type.from_extension extension with
  | Ok (Ml) -> find_shape ~filename ~compiler_flags ~output_prefix
  | Ok (Mli) | Error _ ->
      Fiber.return Result.unsupported_source_file_extension
