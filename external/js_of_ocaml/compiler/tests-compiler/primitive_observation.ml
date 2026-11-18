open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib
open Util

let with_input file f =
  let ic = open_in_bin (Filetype.path_of_cmo_file file) in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)

let parse file =
  with_input file (fun ic ->
      match Parse_bytecode.from_channel ic with
      | `Cmo unit -> Parse_bytecode.from_cmo unit ic
      | `Cma _ | `Exe -> assert false)

let compile_unit name source =
  Filetype.ocaml_text_of_string source
  |> Filetype.write_ocaml ~name
  |> compile_ocaml_to_cmo

let capture_javascript emit =
  let shortvar = Config.Flag.shortvar () in
  Config.Flag.set "shortvar" false;
  Fun.protect
    ~finally:(fun () -> Config.Flag.set "shortvar" shortvar)
    (fun () ->
      let buffer = Buffer.create 128 in
      let result =
        emit
          ~standalone:false
          ~profile:Profile.O3
          ~link:`No
          (Pretty_print.to_buffer buffer)
      in
      Buffer.contents buffer, result)

(* Compile fresh IR with [Driver.f] and with [Driver.f'], which reports no names, and
   check that collecting the names doesn't change the generated code or the variables
   allocated. Returns the JavaScript and the names [Driver.f] reports. *)
let javascript make_program =
  let compile emit =
    (* Both runs need fresh IR and identical variable numbering. *)
    Code.Var.reset ();
    Shape.State.reset ();
    let program = make_program () in
    let source, result =
      capture_javascript (fun ~standalone ~profile ~link formatter ->
          emit ~standalone ~profile ~link formatter program)
    in
    source, result, Code.Var.count ()
  in
  let source_f_prime, (), vars_f_prime =
    compile (fun ~standalone ~profile ~link formatter program ->
        Driver.f' ~standalone ~profile ~link formatter program)
  in
  let source_f, { Driver.primitives; _ }, vars_f =
    compile (fun ~standalone ~profile ~link formatter program ->
        Driver.f ~standalone ~profile ~link ~source_map:false ~formatter program)
  in
  if not (String.equal source_f source_f_prime)
  then failwith "Driver.f and Driver.f' emitted different JavaScript";
  if vars_f <> vars_f_prime
  then failwith "Driver.f and Driver.f' allocated different numbers of variables";
  source_f, primitives

let setup () =
  Config.set_target `JavaScript;
  Config.set_effects_backend `Disabled

let%expect_test "primitive names resolve aliases" =
  with_temp_dir ~f:(fun () ->
      setup ();
      Primitive.register "caml_collector_probe" `Mutator None (Some 1);
      Primitive.alias "caml_collector_alias" "caml_collector_probe";
      let unit =
        compile_unit
          "alias.ml"
          {ocaml|
external probe : int -> unit = "caml_collector_alias"
let () = probe 1; probe 2
|ocaml}
      in
      let _, names = javascript (fun () -> (parse unit).code) in
      print_endline "Reported names (the alias is resolved):";
      StringSet.iter print_endline names;
      [%expect {|
        Reported names (the alias is resolved):
        caml_collector_probe
        caml_register_global
        |}])

let%expect_test "each compilation reports only its own primitives" =
  with_temp_dir ~f:(fun () ->
      setup ();
      let first_unit =
        compile_unit
          "first.ml"
          {ocaml|
external first : int -> unit = "caml_first_compilation"
let () = first 1
|ocaml}
      in
      let second_unit =
        compile_unit
          "second.ml"
          {ocaml|
external second : int -> unit = "caml_second_compilation"
let () = second 2
|ocaml}
      in
      let _, first = javascript (fun () -> (parse first_unit).code) in
      let _, second = javascript (fun () -> (parse second_unit).code) in
      print_endline "First compilation:";
      StringSet.iter print_endline first;
      print_endline "Second compilation:";
      StringSet.iter print_endline second;
      [%expect {|
        First compilation:
        caml_first_compilation
        caml_register_global
        Second compilation:
        caml_register_global
        caml_second_compilation
        |}])

let%expect_test "primitive names include generated helpers and runtime values" =
  with_temp_dir ~f:(fun () ->
      setup ();
      Primitive.register "caml_collector_probe" `Mutator None (Some 1);
      Primitive.alias "caml_collector_alias" "caml_collector_probe";
      let unit =
        compile_unit
          "generated_helpers.ml"
          {ocaml|
external publish : 'a -> unit = "caml_collector_publish"
external choose : unit -> (int -> int) = "caml_collector_choose"
external runtime_value : string -> (int -> unit) = "caml_jsoo_runtime_value"
external probe : int -> unit = "caml_collector_alias"

(* Constants need [string] and [int64] construction helpers. *)
let () = publish ("collector-string", 1L)

(* Calling an unknown function needs [caml_call_gen]. *)
let () = publish ((choose ()) 3)

(* Runtime lookup reports the referenced primitive, resolving its alias. *)
let () = publish (runtime_value "caml_collector_alias")

(* A primitive inside a generated wrapper must also be reported. *)
let () = publish (fun x -> probe x)
|ocaml}
      in
      let _, names = javascript (fun () -> (parse unit).code) in
      StringSet.iter print_endline names;
      [%expect {|
        caml_call_gen
        caml_collector_choose
        caml_collector_probe
        caml_collector_publish
        caml_int64_create_lo_mi_hi
        caml_register_global
        caml_string_of_jsbytes
        |}])

let%expect_test "primitive names include trampoline helpers" =
  with_temp_dir ~f:(fun () ->
      setup ();
      let unit =
        compile_unit
          "trampolines.ml"
          {ocaml|
(* Mutually recursive tail calls require generated trampoline helpers. *)
let rec odd x = if x = 0 then false else even (x - 1)
and even x = if x = 0 then true else odd (x - 1)
|ocaml}
      in
      let _, names = javascript (fun () -> (parse unit).code) in
      Printf.printf "trampoline: %b\n" (StringSet.mem "caml_trampoline" names);
      Printf.printf
        "trampoline return: %b\n"
        (StringSet.mem "caml_trampoline_return" names);
      [%expect {|
        trampoline: true
        trampoline return: true
        |}])
