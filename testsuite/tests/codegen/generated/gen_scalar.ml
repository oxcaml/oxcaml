(* TEST
  include ocamlcommon;
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
  {
    output = "static_cast_generated.ml";
    arguments = "static_cast";
    run;

    unset stdout; unset stderr; unset libraries;
    flags = " -O3 -I ocamlopt.opt";
    flags += " -cfg-prologue-shrink-wrap";
    flags += " -x86-peephole-optimize";
    flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
    flags += " -regalloc-param AFFINITY:on -regalloc irc";

    test_file = "static_cast_generated.ml";
    run-expectnat;
    reference = "${test_source_directory}/static_cast.ml";
    check-program-output;
  }{
    output = "int_add_generated.ml";
    arguments = "int_add";
    run;

    unset stdout; unset stderr; unset libraries;
    flags = " -O3 -I ocamlopt.opt";
    flags += " -cfg-prologue-shrink-wrap";
    flags += " -x86-peephole-optimize";
    flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
    flags += " -regalloc-param AFFINITY:on -regalloc irc";

    test_file = "int_add_generated.ml";
    run-expectnat;
    reference = "${test_source_directory}/int_add.ml";
    check-program-output;
  }
*)

let type_declaration_of_operation (op : _ Scalar.Operation.t) =
  match op with
  | Unary (Integral (t, _)) ->
    Printf.sprintf "%s -> %s"
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
  | Unary (Floating (t, _)) ->
    Printf.sprintf "%s -> %s"
      (Scalar.Floating.to_string t)
      (Scalar.Floating.to_string t)
  | Unary (Static_cast { src; dst }) ->
    Printf.sprintf "%s -> %s"
      (Scalar.to_string src)
      (Scalar.to_string dst)
  | Binary (Integral (t, _)) ->
    Printf.sprintf "%s -> %s -> %s"
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
  | Binary (Shift (t, _, Int)) ->
    Printf.sprintf "%s -> int -> %s"
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
  | Binary (Floating (t, _)) ->
    Printf.sprintf "%s -> %s -> %s"
      (Scalar.Floating.to_string t)
      (Scalar.Floating.to_string t)
      (Scalar.Floating.to_string t)
  | Binary
      (Icmp
         (t, (Ceq | Cne | Clt | Cgt | Cle | Cge | Cult | Cugt | Cule | Cuge)))
    ->
    Printf.sprintf "%s -> %s -> bool"
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
  | Binary
      (Fcmp
         ( t,
           ( CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge
           | CFnge ) )) ->
    Printf.sprintf "%s -> %s -> bool"
      (Scalar.Floating.to_string t)
      (Scalar.Floating.to_string t)
  | Binary (Three_way_compare_int (_, t)) ->
    Printf.sprintf "%s -> %s -> bool"
      (Scalar.Integral.to_string t)
      (Scalar.Integral.to_string t)
  | Binary (Three_way_compare_float t) ->
    Printf.sprintf "%s -> %s -> int"
      (Scalar.Floating.to_string t)
      (Scalar.Floating.to_string t)

let params_of_operation (op : _ Scalar.Operation.t) =
  match op with
  | Unary _ -> "x"
  | Binary _ -> "x y"

let test_of_operation operation =
  let mangle_sigils s = String.split_on_char '#' s |> String.concat "_u" in
  let val_name = Scalar.Operation.to_string operation |> mangle_sigils in
  Printf.sprintf
    "external %s : %s = \"%s\"\n\
     let %s %s = %s %s\n\
     [%%%%expect_asm X86_64{||}]\n"
    val_name
    (type_declaration_of_operation operation)
    (Scalar.Operation.With_percent_prefix.to_string operation)
    val_name
    (params_of_operation operation)
    val_name
    (params_of_operation operation)

let description_of_operation (op : _ Scalar.Operation.t) =
  (* CR jrayman: split _u *)
  match op with
  | Unary (Integral (_, (Neg | Succ | Pred | Bswap as op))) ->
    "int_" ^ Scalar.Operation.Unary.Int_op.to_string op
  | Unary (Floating (_, (Neg | Abs as op))) ->
    "float_" ^ Scalar.Operation.Unary.Float_op.to_string op
  | Unary (Static_cast _) -> "static_cast"
    (* CR jrayman: split casts *)
  | Binary (Integral (_, (Add | Sub | Mul | And | Or | Xor as op))) ->
    "int_" ^ Scalar.Operation.Binary.Int_op.to_string op
  | Binary (Integral (_, Div _)) -> "int_div"
  | Binary (Integral (_, Mod _)) -> "int_mod"
  | Binary (Shift _) -> "shift"
    (* CR jrayman: split shifts *)
  | Binary (Floating (_, (Add | Sub | Mul | Div as op))) ->
    "float_" ^ Scalar.Operation.Binary.Float_op.to_string op
  | Binary (Icmp _) -> "int_cmp"
  | Binary (Fcmp _) -> "float_cmp"
  | Binary (Three_way_compare_int _) -> "int_compare"
  | Binary (Three_way_compare_float _) -> "float_compare"

let () =
  let desc_to_test = Sys.argv.(1) in
  let ops_to_test =
    List.filter
      (fun op -> String.equal (description_of_operation op) desc_to_test)
      Scalar.Operation.all
  in
  if List.is_empty ops_to_test
  then failwith ("Invalid description " ^ desc_to_test);
  let sorted_ops_to_test =
    List.sort
      (fun op1 op2 ->
         String.compare
           (Scalar.Operation.to_string op1)
           (Scalar.Operation.to_string op2))
      ops_to_test
  in
  print_string
    (String.concat "\n"
      (List.map test_of_operation sorted_ops_to_test))
