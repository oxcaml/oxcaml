(* Tests for the arm64 assembly-level peephole optimizer. Each case builds a
   stream of instructions and directives (as the emitter would), runs the
   optimizer over it, and prints the stream before and after. *)

[@@@ocaml.warning "+a-40-41-42"]

open Arm64_ast.Ast
module D = Asm_targets.Asm_directives
module DLL = Doubly_linked_list
module U = Arm64_peephole.Peephole_utils

let current : U.asm_line DLL.t ref = ref (DLL.make_empty ())

(* Route directives (labels, [.loc], ...) into the current stream, exactly as
   the emitter routes them into its per-function buffer. *)
let () =
  D.initialize ~big_endian:false ~emit_assembly_comments:false ~emit:(fun d ->
      DLL.add_end !current (U.Directive d));
  (* [define_label] requires a current section; the [Section] directive goes to
     the scratch stream above and is discarded. *)
  D.switch_to_section Asm_targets.Asm_section.Text;
  current := DLL.make_empty ()

let ins name operands =
  DLL.add_end !current (U.Ins (Instruction.I { name; operands }))

let label n = D.define_label (Asm_targets.Asm_label.create_int Text n)

let loc ~line = D.loc ~file_num:1 ~line ~col:0 ()

let label_target n =
  DSL.symbol
    (Symbol.create_label Same_section_and_unit
       (Asm_targets.Asm_label.create_int Text n))

let mem ~base ~offset =
  match DSL.mem_offset ~base ~scale:8 ~offset with
  | Ok operand -> operand
  | Offset_out_of_range ->
    failwith (Printf.sprintf "unencodable test offset %d" offset)

(* Shift aliases, encoded as the emitter encodes them (cf.
   [Acc.ins_lsl_immediate] and friends). *)
let lsl_ ~rd ~rn ~amount =
  ins UBFM
    (Quad
       ( DSL.reg_x rd,
         DSL.reg_x rn,
         DSL.imm_six ((64 - amount) mod 64),
         DSL.imm_six (63 - amount) ))

let lsr_ ~rd ~rn ~amount =
  ins UBFM
    (Quad (DSL.reg_x rd, DSL.reg_x rn, DSL.imm_six amount, DSL.imm_six 63))

let asr_ ~rd ~rn ~amount =
  ins SBFM
    (Quad (DSL.reg_x rd, DSL.reg_x rn, DSL.imm_six amount, DSL.imm_six 63))

let add_imm ~rd ~rn ~imm =
  ins ADD_immediate (Quad (rd, rn, DSL.imm imm, DSL.optional_none))

let sub_imm ~rd ~rn ~imm =
  ins SUB_immediate (Quad (rd, rn, DSL.imm imm, DSL.optional_none))

let cmp_imm ~zr ~rn ~imm =
  ins SUBS_immediate (Quad (zr, rn, DSL.imm imm, DSL.optional_none))

let cmp_reg ~rn ~rm =
  ins SUBS_shifted_register (Quad (DSL.xzr, rn, rm, DSL.optional_none))

let print_stream stream =
  DLL.iter stream ~f:(function
    | U.Ins i -> Format.printf "\t%a@." Instruction.print i
    | U.Directive d ->
      let buffer = Buffer.create 80 in
      D.Directive.print buffer d;
      Format.printf "%s@." (Buffer.contents buffer))

let run_test name build =
  let stream = DLL.make_empty () in
  current := stream;
  build ();
  Format.printf "==== %s@." name;
  Format.printf "---- before@.";
  print_stream stream;
  Arm64_peephole.Peephole_optimize.optimize stream;
  Format.printf "---- after@.";
  print_stream stream

(* ---- fuse_memory_pairs ---- *)

let () =
  run_test "str pair, ascending offsets (allocation-init shape)" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:(-8)));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:0)))

let () =
  run_test "ldr pair, descending sp offsets (reload shape)" (fun () ->
      ins LDR (Pair (DSL.reg_x 0, mem ~base:Reg.sp ~offset:16));
      ins LDR (Pair (DSL.reg_x 1, mem ~base:Reg.sp ~offset:8)))

let () =
  run_test "run of four stores fuses pairwise" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:8));
      ins STR (Pair (DSL.reg_x 3, mem ~base:(Reg.reg_x 0) ~offset:16));
      ins STR (Pair (DSL.reg_x 4, mem ~base:(Reg.reg_x 0) ~offset:24)))

let () =
  run_test "loc between the pair is dropped with the second instruction"
    (fun () ->
      loc ~line:1;
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      loc ~line:2;
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "str pair involving lr" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:Reg.sp ~offset:0));
      ins STR (Pair (DSL.lr, mem ~base:Reg.sp ~offset:8)))

let () =
  run_test "no fuse: label between" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      label 100;
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "no fuse: offset gap of 16" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:16)))

let () =
  run_test "boundary: fused offset 504 is in range" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:504));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:512)))

let () =
  run_test "no fuse: fused offset 512 is out of range" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:512));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:520)))

let () =
  run_test "no fuse: unaligned offsets" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:4));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:12)))

let () =
  run_test "no fuse: w registers" (fun () ->
      ins STR (Pair (DSL.reg_w 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins STR (Pair (DSL.reg_w 2, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "no fuse: pre-indexed addressing" (fun () ->
      ins STR (Pair (DSL.reg_x 1, DSL.mem_pre ~base:(Reg.reg_x 0) ~offset:0));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "no fuse: different bases" (fun () ->
      ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 3) ~offset:8)))

let () =
  run_test "no fuse: first load destination is the base" (fun () ->
      ins LDR (Pair (DSL.reg_x 0, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins LDR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "fuse: second load destination is the base" (fun () ->
      ins LDR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins LDR (Pair (DSL.reg_x 0, mem ~base:(Reg.reg_x 0) ~offset:8)))

let () =
  run_test "no fuse: equal load destinations" (fun () ->
      ins LDR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
      ins LDR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:8)))

(* ---- merge_add_immediates ---- *)

let () =
  run_test "add/add merge (comballoc shape)" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:16)

let () =
  run_test "chain of three adds folds fully" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:16;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:32)

let () =
  run_test "add/sub merge with negative net" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      sub_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:24)

let () =
  run_test "add/sub merge with zero net" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      sub_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:8)

let () =
  run_test "add with sp source" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:DSL.sp ~imm:8;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:8)

let () =
  run_test "add with fp source and positive net" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:DSL.fp ~imm:8;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:8)

let () =
  run_test "no merge: fp source with negative net (no sub fp form)" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:DSL.fp ~imm:8;
      sub_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:24)

let () =
  run_test "merge: combined immediate 4096 encodable as shifted" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:4095;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:1)

let () =
  run_test "no merge: combined immediate 4097 not encodable" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:4095;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:2)

let () =
  run_test "no merge: second source is not the first destination" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      add_imm ~rd:(DSL.reg_x 3) ~rn:(DSL.reg_x 3) ~imm:16)

let () =
  run_test "no merge: destinations differ" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      add_imm ~rd:(DSL.reg_x 3) ~rn:(DSL.reg_x 2) ~imm:16)

let () =
  run_test "no merge: label between (gc-return label shape)" (fun () ->
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 27) ~imm:8;
      label 101;
      add_imm ~rd:(DSL.reg_x 2) ~rn:(DSL.reg_x 2) ~imm:16)

let () =
  run_test "no merge: destination is sp" (fun () ->
      ins ADD_immediate (Quad (DSL.sp, DSL.sp, DSL.imm 16, DSL.optional_none));
      ins ADD_immediate (Quad (DSL.sp, DSL.sp, DSL.imm 16, DSL.optional_none)))

(* ---- remove_redundant_cmp ---- *)

let () =
  run_test "redundant immediate compare across b.cond and store" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins (B_cond (Int NE)) (Singleton (label_target 900));
      ins ORR_immediate (Triple (DSL.reg_x 1, DSL.xzr, DSL.bitmask 3n));
      ins STR (Pair (DSL.reg_x 1, mem ~base:Reg.sp ~offset:64));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "redundant register compare" (fun () ->
      cmp_reg ~rn:(DSL.reg_x 0) ~rm:(DSL.reg_x 1);
      ins ORR_immediate (Triple (DSL.reg_x 2, DSL.xzr, DSL.bitmask 7n));
      cmp_reg ~rn:(DSL.reg_x 0) ~rm:(DSL.reg_x 1))

let () =
  run_test "two redundant compares are both removed" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins ORR_immediate (Triple (DSL.reg_x 1, DSL.xzr, DSL.bitmask 3n));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins ORR_immediate (Triple (DSL.reg_x 2, DSL.xzr, DSL.bitmask 7n));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: compared register written in between" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      add_imm ~rd:(DSL.reg_x 0) ~rn:(DSL.reg_x 0) ~imm:8;
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: w0 written between compares on x0 (aliasing)" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins MOVZ
        (Triple
           ( DSL.reg_w 0,
             DSL.imm_sixteen 5,
             DSL.optional_lsl_by_multiple_of_16_bits_w 0 ));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: flag writer in between" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins ADDS (Quad (DSL.reg_x 1, DSL.reg_x 2, DSL.imm 0, DSL.optional_none));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: label in between" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      label 102;
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: call in between" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins BL (Singleton (label_target 901));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: width mismatch" (fun () ->
      ins SUBS_immediate
        (Quad (DSL.wzr, DSL.reg_w 0, DSL.imm 1, DSL.optional_none));
      ins ORR_immediate (Triple (DSL.reg_x 1, DSL.xzr, DSL.bitmask 3n));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1)

let () =
  run_test "no removal: different immediate" (fun () ->
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:1;
      ins ORR_immediate (Triple (DSL.reg_x 1, DSL.xzr, DSL.bitmask 3n));
      cmp_imm ~zr:DSL.xzr ~rn:(DSL.reg_x 0) ~imm:3)

(* ---- compose_shift_pairs ---- *)

let () =
  run_test "lsl 8 then lsr 18 (string-length shape)" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:8;
      lsr_ ~rd:2 ~rn:2 ~amount:18)

let () =
  run_test "lsl 8 then lsr 17 (mixed-block shape)" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:8;
      lsr_ ~rd:2 ~rn:2 ~amount:17)

let () =
  run_test "lsl 31 then asr 32" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:31;
      asr_ ~rd:2 ~rn:2 ~amount:32)

let () =
  run_test "lsr 18 then lsl 18 becomes a mask" (fun () ->
      lsr_ ~rd:2 ~rn:2 ~amount:18;
      lsl_ ~rd:2 ~rn:2 ~amount:18)

let () =
  run_test "lsl 3 then lsl 4 add up" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:3;
      lsl_ ~rd:2 ~rn:2 ~amount:4)

let () =
  run_test "lsr 3 then lsr 4 add up" (fun () ->
      lsr_ ~rd:2 ~rn:2 ~amount:3;
      lsr_ ~rd:2 ~rn:2 ~amount:4)

let () =
  run_test "asr chain saturates at 63" (fun () ->
      asr_ ~rd:2 ~rn:2 ~amount:40;
      asr_ ~rd:2 ~rn:2 ~amount:40)

let () =
  run_test "first shift may have a distinct source" (fun () ->
      lsl_ ~rd:2 ~rn:1 ~amount:8;
      lsr_ ~rd:2 ~rn:2 ~amount:18)

let () =
  run_test "no compose: lsr then lsl with different amounts" (fun () ->
      lsr_ ~rd:2 ~rn:2 ~amount:18;
      lsl_ ~rd:2 ~rn:2 ~amount:3)

let () =
  run_test "no compose: different registers" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:8;
      lsr_ ~rd:3 ~rn:3 ~amount:18)

let () =
  run_test "no compose: unclassifiable bitfield move (uxtb shape)" (fun () ->
      ins UBFM (Quad (DSL.reg_x 2, DSL.reg_x 2, DSL.imm_six 0, DSL.imm_six 7));
      lsr_ ~rd:2 ~rn:2 ~amount:18)

let () =
  run_test "no compose: over-shifted lsl chain" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:40;
      lsl_ ~rd:2 ~rn:2 ~amount:30)

let () =
  run_test "no compose: w-width shifts" (fun () ->
      ins UBFM (Quad (DSL.reg_w 2, DSL.reg_w 2, DSL.imm_six 18, DSL.imm_six 63));
      ins UBFM (Quad (DSL.reg_w 2, DSL.reg_w 2, DSL.imm_six 18, DSL.imm_six 63)))

let () =
  run_test "no compose: label between" (fun () ->
      lsl_ ~rd:2 ~rn:2 ~amount:8;
      label 103;
      lsr_ ~rd:2 ~rn:2 ~amount:18)

(* ---- driver / flags ---- *)

let () =
  Oxcaml_flags.arm64_peephole_fuse_memory_pairs := false;
  Fun.protect
    ~finally:(fun () -> Oxcaml_flags.arm64_peephole_fuse_memory_pairs := true)
    (fun () ->
      run_test "rule disabled by its flag" (fun () ->
          ins STR (Pair (DSL.reg_x 1, mem ~base:(Reg.reg_x 0) ~offset:0));
          ins STR (Pair (DSL.reg_x 2, mem ~base:(Reg.reg_x 0) ~offset:8))))
