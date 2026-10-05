open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib
open Util

let javascript program =
  let shortvar = Config.Flag.shortvar () in
  let use_js_string = Config.Flag.use_js_string () in
  Config.Flag.set "shortvar" false;
  Config.Flag.set "use-js-string" true;
  Fun.protect
    ~finally:(fun () ->
      Config.Flag.set "shortvar" shortvar;
      Config.Flag.set "use-js-string" use_js_string)
    (fun () ->
      (* [Driver.f'] would specialize switches again, including in the baseline. *)
      let deadcode_sentinel = Code.Var.fresh () in
      let program, live_vars = Deadcode.f (Pure_fun.f program) program in
      let js =
        Generate.f
          program
          ~exported_runtime:true
          ~live_vars
          ~trampolined_calls:Code.Var.Set.empty
          ~in_cps:Code.Var.Set.empty
          ~should_export:false
          ~warn_on_unhandled_effect:false
          ~deadcode_sentinel
        |> Driver.link_and_pack ~standalone:false ~link:`No
        |> Driver.name_variables
      in
      let buffer = Buffer.create 128 in
      let (_ : Source_map.info) =
        Js_output.program (Pretty_print.to_buffer buffer) js
      in
      Buffer.contents buffer)

let setup () =
  Config.set_target `JavaScript;
  Config.set_effects_backend `Disabled;
  Code.Var.reset ();
  Shape.State.reset ();
;;

let%expect_test "source match preserves arguments after dead-code cleanup" =
  with_temp_dir ~f:(fun () ->
      setup ();
      let file =
        Filetype.ocaml_text_of_string
          {ocaml|
type choice = A | B | C | D
let f choice =
  let value =
    (* This regression requires 2 things:
       1. Different value output from each branch
       2. Shared code after the match that receives the output value *)
    match choice with
    | A -> "a"
    | B -> "b"
    | C -> "c"
    | D -> "d"
  in
  "[" ^ value ^ "]"
|ocaml}
        |> Filetype.write_ocaml ~name:"switch_source.ml"
        |> compile_ocaml_to_cmo
      in
      let ic = open_in_bin (Filetype.path_of_cmo_file file) in
      let parsed =
        Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
            match Parse_bytecode.from_channel ic with
            | `Cmo unit -> (Parse_bytecode.from_cmo unit ic).code
            | _ -> assert false)
      in
      (* Model respecializing a program after an earlier optimization pass.
         String concatenation keeps a shared continuation after the match.
         Dead-code cleanup bypasses the empty case blocks, passing a different
         string constant to that continuation for each constructor. *)
      let cleaned, _ = Deadcode.f (Pure_fun.f parsed) parsed in
      (* Supply the real Stdlib and runtime for [( ^ )], compiled separately so
         the regression's baseline IR does not go through switch specialization. *)
      let runtime =
        Filetype.ocaml_text_of_string "let concat = ( ^ )"
        |> Filetype.write_ocaml ~name:"switch_runtime.ml"
        |> compile_ocaml_to_bc
        |> compile_bc_to_javascript
             ~flags:[ "--linkall" ]
             ~use_js_string:true
             ~sourcemap:false
        |> Filetype.read_js
        |> Filetype.string_of_js_text
      in
      let run program =
        let source =
          Printf.sprintf
            {js|%s
globalThis.jsoo_runtime.caml_register_global = moduleValue => {
  // The block tag is at index 0; the compilation unit exports only f.
  const f = moduleValue[1];
  ["A", "B", "C", "D"].forEach((constructor, index) => {
    console.log(`f ${constructor} -> ${f(index)}`);
  });
  return 0;
};
%s
|js}
            runtime
            (javascript program)
        in
        Filetype.js_text_of_string source
        |> Filetype.write_js ~name:"switch_source.js"
        |> run_javascript
        |> print_string
      in
      print_endline "Before specialization:";
      Code.Print.program Format.std_formatter (fun _ _ -> "") cleaned;
      [%expect {|
        Before specialization:
        Entry point: 0

        ==== 0 () ====
          v3{cst_a} = CONST{"a"}
          v13{cst_b} = CONST{"b"}
          v14{cst_c} = CONST{"c"}
          v15{cst_d} = CONST{"d"}
          v6{cst} = CONST{"]"}
          v10{cst} = CONST{"["}
          v7{Stdlib} = "caml_get_global"("Stdlib"j)
          branch 7 ()

        ==== 1 () ====
          switch v2 {int 0 -> 6 (v3{cst_a}); int 1 -> 6 (v13{cst_b}); int 2 -> 6 (v14{cst_c}); int 3 -> 6 (v15{cst_d}); }

        ==== 6 (v5) ====
          v8 = v7{Stdlib}[27]
          v9 = v8~(v5, v6{cst})
          v11 = v7{Stdlib}[27]
          v12 = v11~(v10{cst}, v9)
          return v12

        ==== 7 () ====
          v1 = fun(v2){1 ()}
          v16{Switch_source} = imm{tag=0; 0 = v1}
          v18 = "caml_register_global"(v16{Switch_source}, "Switch_source"j)
          stop
        |}];
      run cleaned;
      [%expect {|
        f A -> [a]
        f B -> [b]
        f C -> [c]
        f D -> [d]
        |}];
      let specialized = Specialize.switches cleaned in
      print_endline "After specialization:";
      Code.Print.program Format.std_formatter (fun _ _ -> "") specialized;
      [%expect {|
        After specialization:
        Entry point: 0

        ==== 0 () ====
          v3{cst_a} = CONST{"a"}
          v13{cst_b} = CONST{"b"}
          v14{cst_c} = CONST{"c"}
          v15{cst_d} = CONST{"d"}
          v6{cst} = CONST{"]"}
          v10{cst} = CONST{"["}
          v7{Stdlib} = "caml_get_global"("Stdlib"j)
          branch 7 ()

        ==== 1 () ====
          switch v2 {int 0 -> 6 (v3{cst_a}); int 1 -> 6 (v13{cst_b}); int 2 -> 6 (v14{cst_c}); int 3 -> 6 (v15{cst_d}); }

        ==== 6 (v5) ====
          v8 = v7{Stdlib}[27]
          v9 = v8~(v5, v6{cst})
          v11 = v7{Stdlib}[27]
          v12 = v11~(v10{cst}, v9)
          return v12

        ==== 7 () ====
          v1 = fun(v2){1 ()}
          v16{Switch_source} = imm{tag=0; 0 = v1}
          v18 = "caml_register_global"(v16{Switch_source}, "Switch_source"j)
          stop
        |}];
      run specialized;
      [%expect {|
        f A -> [a]
        f B -> [b]
        f C -> [c]
        f D -> [d]
        |}])
