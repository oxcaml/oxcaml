(* Deterministic IR fixtures with expectations independent of either emitter. *)
open Ir

let int_ty = NumberTy.boxed Int

let ty = Ty.Number int_ty

let name = Name.of_string

let constant n = Expr.Const { number = Number.Int n; unboxed = false }

let read n = Expr.Read (Place.Variable (name n))

let binding n ty = Binding.{ name = name n; ty; is_mutable = true }

let binary op lhs rhs = Expr.Bin_op { ty; op; lhs; rhs }

let empty = Statement.Seq []

let swarm assertions =
  { (Config.Swarm.create (Random.State.make [| 0 |])) with
    floats = true;
    unboxed_numbers = true;
    arrays = true;
    assertions
  }

let program ?(assertions = Config.Disabled) ?(records = []) ?(variants = [])
    ?(exceptions = []) ?(functions = []) declarations statement =
  Program.create ~swarm:(swarm assertions) ~record_types:records ~exceptions
    ~variant_types:variants ~functions ~toplevel_decls:declarations
    ~toplevel_statement:statement

let numbers () =
  let declarations = ref [] in
  let expected = Buffer.create 1000 in
  let add nty expr output =
    let n = Printf.sprintf "n%d" (List.length !declarations) in
    declarations := (binding n (Ty.Number nty), expr) :: !declarations;
    Buffer.add_string expected (output ^ " ")
  in
  List.iter
    (fun (nty : NumberTy.t) ->
      let literal n =
        let number =
          match nty.base with
          | Float -> Number.Float (Int64.bits_of_float (Int64.to_float n))
          | Float32 -> Number.Float32 (Int32.bits_of_float (Int64.to_float n))
          | Int | Nativeint | Int64 | Int32 | Int16 | Int8 ->
            Number.of_integral_bits nty.base n
        in
        Expr.Const { number; unboxed = nty.unboxed }
      in
      (if NumberTy.is_floating_point nty
       then (
         List.iter
           (fun (op, input, output) ->
             add nty
               (Expr.Un_op { ty = Number nty; op; arg = literal input })
               output)
           Un_op.[Sqrt, 4L, "0x1p+1"; Neg, 0L, "-0x0p+0"; Round, -1L, "-0x1p+0"];
         List.iter
           (fun (op, lhs, rhs, output) ->
             add nty
               (Expr.Bin_op
                  { ty = Number nty; op; lhs = literal lhs; rhs = literal rhs })
               output)
           Bin_op.
             [ Div, 1L, 0L, "infinity";
               Div, -1L, 0L, "-infinity";
               Div, 0L, 0L, "nan";
               Rem, 7L, 2L, "0x1p+0" ])
       else
         let min, max =
           match nty.base with
           | Int -> Int64.of_int min_int, Int64.of_int max_int
           | Nativeint | Int64 -> Int64.min_int, Int64.max_int
           | Int32 -> -2147483648L, 2147483647L
           | Int16 -> -32768L, 32767L
           | Int8 -> -128L, 127L
           | Float | Float32 -> assert false
         in
         List.iter
           (fun (op, lhs, rhs, output) ->
             add nty
               (Expr.Bin_op
                  { ty = Number nty; op; lhs = literal lhs; rhs = literal rhs })
               (Int64.to_string output))
           Bin_op.
             [ Add, max, 1L, min;
               Sub, min, 1L, max;
               Mul, max, 2L, -2L;
               Div, min, -1L, min;
               Rem, min, -1L, 0L ];
         add int_ty
           (Expr.Bin_op
              { ty = Number nty;
                op = Unsigned_compare;
                lhs = literal (-1L);
                rhs = literal 0L
              })
           "1";
         add nty
           (Expr.Bin_op
              { ty = Number nty;
                op = Shift_right;
                lhs = literal min;
                rhs = constant 1
              })
           (Int64.to_string (Int64.div min 2L)));
      List.iter
        (fun (to_ : NumberTy.t) ->
          let output =
            if NumberTy.is_floating_point to_ then "-0x1p+0" else "-1"
          in
          add to_
            (Expr.Convert { expr = literal (-1L); from = nty; to_ })
            output)
        NumberTy.all)
    NumberTy.all;
  program !declarations empty, Buffer.contents expected

let strings () =
  let bytes = String.init 256 Char.chr in
  let value = Expr.String_const bytes in
  let concat =
    Expr.Bounded_string_concat
      { lhs = value; rhs = value; result_name = name "concat"; max_length = 64 }
  in
  ( program
      [binding "bytes" String, value; binding "prefix" String, concat]
      empty,
    Printf.sprintf "%S %S " (String.sub bytes 0 64) bytes )

let field = Ty.{ index = 0; ty; is_mutable = true }

let record = Ty.{ id = 0; fields = [field]; unboxed = false }

let unboxed = { record with unboxed = true }

let record_value n = Expr.Record (record, [constant n])

let project n = Place.Field (Variable (name n), record, field)

let mark =
  let state = binding "state" (Ty.Record record) in
  let marker = binding "marker" ty in
  let target = Place.Field (Variable state.name, record, field) in
  Function.
    { name = name "mark";
      params = [state; marker];
      inline = Default;
      body =
        Statement.Assign
          ( target,
            binary Add
              (binary Mul (Expr.Read target) (constant 10))
              (read "marker") );
      return_ty = ty;
      result = read "marker";
      raises = []
    }

let call n =
  Expr.Call_toplevel { fun_name = mark.name; args = [read "state"; constant n] }

let order () =
  let constructor = Ty.{ constructor_index = 0; args = [ty; ty; ty] } in
  let variant = Ty.{ variant_id = 0; constructors = [constructor] } in
  let declarations =
    [ binding "state" (Record record), record_value 0;
      ( binding "tuple" (Unboxed_tuple [ty; ty; ty]),
        Expr.Unboxed_tuple [call 1; call 2; call 3] );
      ( binding "variant" (Variant variant),
        Expr.Construct
          { sum = Variant variant;
            constructor;
            args = [call 4; call 5; call 6]
          } ) ]
  in
  ( program ~records:[record] ~variants:[variant] ~functions:[mark] declarations
      empty,
    "Variant_0_C0 4 5 6 1 2 3 321654 " )

let aliases () =
  let array_ty = Ty.Array (ty, [2; 2]) in
  let declarations =
    [ binding "record" (Record record), record_value 3;
      binding "alias" (Record record), read "record";
      ( binding "snapshot" (Record unboxed),
        Expr.Record_convert
          { from = record;
            to_unboxed = true;
            source_name = name "source";
            expr = read "record"
          } );
      ( binding "copy" (Record record),
        Expr.Record_update (record, read "record", field, constant 4) );
      ( binding "array" array_ty,
        Expr.Array_make
          { dimensions = [2; 2]; init_name = name "initial"; init = constant 5 }
      );
      binding "array_alias" array_ty, read "array" ]
  in
  let statement =
    Statement.Seq
      [ Assign (project "alias", constant 9);
        Assign
          ( Element (Variable (name "array_alias"), [constant 1; constant 0]),
            constant 7 ) ]
  in
  ( program ~records:[record; unboxed] declarations statement,
    "5 5 7 5 5 5 7 5 4 3 9 9 " )

let exceptions () =
  let exn = Exn.{ id = 0; payload = Some ty } in
  let payload = binding "payload" ty in
  let raise_ =
    Expr.Raise { exn; payload = Some (constant 42); notrace = true }
  in
  let handler =
    Handler.
      { pattern = Exn { exn; payload = Some payload };
        body = binary Add (read "payload") (constant 1)
      }
  in
  let catch = Expr.Try { ty; body = raise_; handlers = [handler] } in
  let short =
    Expr.Bin_op
      { ty = Bool; op = And; lhs = Bool_const false; rhs = Assert_false 99 }
  in
  let catch_all =
    Expr.Try
      { ty;
        body = Assert_false 88;
        handlers = [Handler.{ pattern = Catch_all; body = constant 17 }]
      }
  in
  ( program ~assertions:May_raise ~exceptions:[exn]
      [ binding "caught" ty, catch;
        binding "short" Bool, short;
        binding "all" ty, catch_all ]
      empty,
    "returned\n17 false 43 " )

let assertion () =
  ( program ~assertions:May_raise ~records:[record] ~functions:[mark]
      [ binding "state" (Record record), record_value 0;
        ( binding "uninitialized" (Unboxed_tuple [ty; ty; ty]),
          Expr.Unboxed_tuple [Assert_false 17; call 2; call 3] ) ]
      empty,
    "assertion 17:0\n32 " )

let control () =
  let first = Exn.{ id = 0; payload = Some ty } in
  let second = Exn.{ id = 1; payload = Some ty } in
  let p = binding "payload" ty in
  let raise_ exn payload =
    Expr.Raise { exn; payload = Some payload; notrace = false }
  in
  let handler exn body =
    Handler.{ pattern = Exn { exn; payload = Some p }; body }
  in
  let nested =
    Expr.Try
      { ty;
        body =
          Expr.Try
            { ty;
              body = raise_ first (constant 4);
              handlers = [handler first (raise_ second (read "payload"))]
            };
        handlers = [handler second (read "payload")]
      }
  in
  let sum = Sum.Or_null Ty.Bool in
  let constructors = Sum.constructors sum in
  let nullable =
    Expr.Construct
      { sum;
        constructor = List.nth constructors 1;
        args = [Expr.Bool_const true]
      }
  in
  let boolean = binding "boolean" Ty.Bool in
  let matched =
    Expr.Match
      { sum;
        scrutinee = read "nullable";
        cases =
          [ Expr.
              { constructor = List.nth constructors 0;
                bindings = [];
                body = constant 0
              };
            Expr.
              { constructor = List.nth constructors 1;
                bindings = [boolean];
                body =
                  If
                    { condition = read "boolean";
                      if_true = constant 1;
                      if_false = constant 0
                    }
              } ]
      }
  in
  let target = Place.Variable (name "result") in
  let increment value =
    Statement.Assign (target, binary Add (read "result") value)
  in
  let statements =
    Statement.Seq
      [ Bounded_loop
          { var = name "i";
            init = constant 3;
            bound = constant 1;
            stride = -1;
            body = increment (read "i")
          };
        Try
          { body =
              Seq
                [ increment (constant 10);
                  Raise
                    { exn = first;
                      payload = Some (constant 3);
                      notrace = false
                    } ];
            handlers = [handler first (increment (read "payload"))]
          } ]
  in
  ( program ~exceptions:[first; second]
      [ binding "result" ty, constant 0;
        binding "nested" ty, nested;
        binding "nullable" (Or_null Bool), nullable;
        binding "matched" ty, matched ]
      statements,
    "1 This true 4 19 " )

let record_copies () =
  let fields = List.init 3 (fun index -> Ty.{ index; ty; is_mutable = true }) in
  let record = Ty.{ id = 1; fields; unboxed = false } in
  let parameter = binding "r" (Ty.Record record) in
  let mutate =
    Function.
      { name = name "mutate";
        params = [parameter];
        inline = Never;
        body =
          Statement.Seq
            (List.map
               (fun field ->
                 Statement.Assign
                   ( Place.Field (Variable parameter.name, record, field),
                     constant 1 ))
               fields);
        return_ty = ty;
        result = constant 7;
        raises = []
      }
  in
  let declarations =
    List.concat_map
      (fun index ->
        let source = "source" ^ string_of_int index in
        [ ( binding source (Ty.Record record),
            Expr.Record (record, [constant 0; constant 0; constant 0]) );
          ( binding ("copy" ^ string_of_int index) (Ty.Record record),
            Expr.Record_update
              ( record,
                read source,
                List.nth fields index,
                Call_toplevel { fun_name = mutate.name; args = [read source] }
              ) ) ])
      [0; 1; 2]
  in
  ( program ~records:[record] ~functions:[mutate] declarations empty,
    "1 1 7 1 1 1 1 7 0 1 1 1 7 0 0 1 1 1 " )

let () =
  let directory = Sys.argv.(1) in
  List.iter
    (fun (label, make) ->
      let program, expected = make () in
      List.iter
        (fun (suffix, content) ->
          Out_channel.with_open_bin
            (Filename.concat directory (label ^ suffix))
            (fun channel -> output_string channel content))
        [ ".ml", Program.to_code program;
          ".cpp", Cpp.to_code program;
          ".expected", expected ])
    [ "numbers", numbers;
      "strings", strings;
      "order", order;
      "aliases", aliases;
      "exceptions", exceptions;
      "assertion", assertion;
      "control", control;
      "record_copies", record_copies ]
