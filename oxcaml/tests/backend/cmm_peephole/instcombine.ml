open Cmm

let dbg = Debuginfo.none

let int n = Cconst_int (n, dbg)

let natint n = Cconst_natint (n, dbg)

let binop op left right = Cop (op, [left; right], dbg)

let x = Cvar (Backend_var.create_local "x")

let and_int left right = Cmm_helpers.and_int left right dbg

let or_int left right = Cmm_helpers.or_int left right dbg

let xor_int left right = Cmm_helpers.xor_int left right dbg

let lsl_int left right = Cmm_helpers.lsl_int left right dbg

let lsr_int left right = Cmm_helpers.lsr_int left right dbg

let show_result name before after =
  Format.printf "%s:@.  %a@.  => %a@." name Printcmm.expression before
    Printcmm.expression after

let show name op simplify left right =
  show_result name (binop op left right) (simplify left right)

let show_masks () =
  show "or/and disjoint" Cand and_int (binop Cor x (int 12)) (int 3);
  show "or/and covered" Cand and_int (binop Cor x (int 15)) (int 3);
  show "xor/and disjoint" Cand and_int (binop Cxor x (int 12)) (int 3);
  show "and/or covering" Cor or_int (binop Cand x (int (-4))) (int 3);
  show "and/or absorbed" Cor or_int (binop Cand x (int 3)) (int 15);
  show "xor/or absorbed" Cor or_int (binop Cxor x (int 3)) (int 15);
  show "or/xor same mask" Cxor xor_int (binop Cor x (int 12)) (int 12);
  show "and/xor complementary mask" Cxor xor_int
    (binop Cand x (int 12))
    (int (-13));
  show "or/and overlapping" Cand and_int (binop Cor x (int 6)) (int 3);
  show "xor/and overlapping" Cand and_int (binop Cxor x (int 6)) (int 3);
  show "and/or neither covering nor absorbed" Cor or_int
    (binop Cand x (int 6))
    (int 3);
  show "xor/or not absorbed" Cor or_int (binop Cxor x (int 6)) (int 3);
  show "or/xor different mask" Cxor xor_int (binop Cor x (int 12)) (int 6);
  show "and/xor noncomplementary mask" Cxor xor_int
    (binop Cand x (int 12))
    (int (-14));
  show "left constant, and" Cand and_int (int 3) (binop Cor x (int 12));
  show "left constant, or" Cor or_int (int 3) (binop Cand x (int (-4)));
  show "left constant, xor" Cxor xor_int (int 12) (binop Cor x (int 12));
  show "nativeint sign bit disjoint" Cand and_int
    (binop Cor x (natint Nativeint.min_int))
    (natint Nativeint.max_int);
  show "nativeint sign bit absorbed" Cor or_int
    (binop Cxor x (natint Nativeint.min_int))
    (natint Nativeint.min_int);
  show "nativeint complementary masks" Cxor xor_int
    (binop Cand x (natint Nativeint.max_int))
    (natint Nativeint.min_int);
  show "mixed constant representations" Cand and_int
    (binop Cxor x (natint 12n))
    (int 3);
  show "zero mask" Cxor xor_int (binop Cor x (int 0)) (int 0);
  show "all bits mask" Cxor xor_int
    (binop Cor x (natint Nativeint.minus_one))
    (int (-1))

let show_shifts () =
  let pairs =
    [ "lsr/lsl", Clsr, Clsl, lsl_int;
      "asr/lsl", Casr, Clsl, lsl_int;
      "lsl/lsr", Clsl, Clsr, lsr_int ]
  in
  List.iter
    (fun (name, inner, outer, simplify) ->
      List.iter
        (fun shift ->
          show
            (Printf.sprintf "%s by %d" name shift)
            outer simplify
            (binop inner x (int shift))
            (int shift))
        [ 0;
          1;
          30;
          31;
          32;
          33;
          Cmm_helpers.arch_bits - 1;
          -1;
          Cmm_helpers.arch_bits ];
      show (name ^ " unequal shifts") outer simplify
        (binop inner x (int 3))
        (int 2);
      show
        (name ^ " invalid inner shift")
        outer simplify
        (binop inner x (int (-1)))
        (int 2))
    pairs;
  show_result "nested logical shifts"
    (binop Clsr (binop Clsr (binop Clsl x (int 8)) (int 8)) (int 9))
    (lsr_int (lsr_int (lsl_int x (int 8)) (int 8)) (int 9))

let show_arithmetic () =
  List.iter
    (fun constant ->
      show "complement of addition" Cxor xor_int (binop Caddi x constant)
        (int (-1));
      show "complement of subtraction" Cxor xor_int (binop Csubi constant x)
        (int (-1)))
    [int 5; natint Nativeint.min_int; natint Nativeint.max_int];
  show "addition with partial xor" Cxor xor_int (binop Caddi x (int 5)) (int 7);
  show "subtraction with partial xor" Cxor xor_int
    (binop Csubi (int 5) x)
    (int 7)

let show_effects_and_debug_bindings () =
  let call name =
    Cop
      ( Cextcall
          { func = name;
            ty = typ_int;
            ty_args = [];
            alloc = false;
            builtin = false;
            returns = true;
            effects = Arbitrary_effects;
            coeffects = Has_coeffects
          },
        [],
        dbg )
  in
  show "call or/xor all bits" Cxor xor_int
    (binop Cor (call "observe") (int (-1)))
    (int (-1));
  show "call and/xor zero mask" Cxor xor_int
    (binop Cand (call "observe") (int 0))
    (int (-1));
  let load =
    Cop
      ( Cload
          { memory_chunk = Word_int;
            mutability = Asttypes.Mutable;
            is_atomic = false
          },
        [int 4096],
        dbg )
  in
  List.iter
    (fun (name, value) ->
      show
        (name ^ " with constant and result")
        Cand and_int
        (binop Cor value (int 15))
        (int 3);
      show
        (name ^ " with constant or result")
        Cor or_int
        (binop Cand value (int 3))
        (int 15);
      show
        (name ^ " in inverse shifts")
        Clsl lsl_int
        (binop Clsr value (int 3))
        (int 3))
    [ "call", call "observe";
      "mutable load", load;
      "sequence", Csequence (call "observe", x);
      "sequence of calls", Csequence (call "first", call "second") ];
  let phantom name body =
    let var =
      Backend_var.With_provenance.create (Backend_var.create_local name)
    in
    Cphantom_let (var, None, body)
  in
  show "phantom bindings around operation and constants" Cand and_int
    (phantom "operation" (binop Cor x (phantom "mask" (int 12))))
    (phantom "outer_mask" (int 3));
  show "phantom binding around effectful operand" Cand and_int
    (binop Cor (phantom "operand" (call "observe")) (int 15))
    (int 3)

let () =
  Clflags.unique_ids := false;
  show_masks ();
  show_shifts ();
  show_arithmetic ();
  show_effects_and_debug_bindings ()
