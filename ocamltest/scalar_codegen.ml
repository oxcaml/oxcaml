let type_declaration_of_operation : _ Scalar.Operation.t -> string =
  function
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

let params_of_operation : _ Scalar.Operation.t -> string =
  function Unary _ -> "x" | Binary _ -> "x y"

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
