(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "-fragile-match"]

(* The sizes in this file are estimates of the number of machine instructions
   that the backend will emit for each Flambda construct. One unit is one
   instruction (roughly four bytes on x86-64; exactly four bytes on arm64).
   Entries in jump tables are four bytes and are also counted as one unit.

   Estimates are computed for both x86-64 and arm64 at the same time and both
   are stored in the terms, so that dumps of Flambda terms do not depend on the
   architecture of the compiler; comparisons and inlining decisions use the
   component for the target architecture (see [target]).

   The estimates are derived by following the translation to Cmm ([To_cmm_expr],
   [To_cmm_primitive], [Cmm_helpers]) and then the instruction selection and
   emission in [backend/amd64] and [backend/arm64]. Where the two architectures
   differ, both sequences are given in the comments. The main systematic
   differences are:

   - on x86-64, address arithmetic of the form [base + index * scale + constant]
   is folded into a single load or store, whereas arm64 only folds an immediate
   offset and needs a separate [add] for an index;

   - tagging ([lsl 1; or 1]) and tagged addition are a single [lea] on x86-64
   but two instructions on arm64;

   - arm64 has single-instruction sign extensions ([sxtb], [sxth], [sxtw]) where
   x86-64 needs [shl; sar] except for the 32-bit case ([movsxd]);

   - storing a constant needs the constant moved into a register first on arm64,
   and loading the address of a symbol takes two instructions there ([adrp; add]
   or [adrp; ldr]);

   - comparisons whose result feeds directly into a conditional branch are
   emitted as a compare-and-branch pair on both architectures rather than being
   materialised;

   - atomic operations other than loads are not supported natively on arm64 and
   become external calls.

   Each size also records whether the code needs a stack frame, i.e. whether it
   contains a call, an allocation or a poll (see
   [Cfg.basic_block_contains_calls]); the prologue and epilogue of a function
   are added by [add_function_frame] when the size of a whole function body is
   known.

   The [machine_width] parameters are accepted for interface stability only.
   Native code is always generated for 64-bit targets; 32-bit machine widths
   only arise for the JavaScript backend, where these sizes are not used for
   inlining decisions.

   Sizes do not include register moves needed to marshal arguments, nor the
   spills and reloads around calls, except where noted for calls themselves. *)

(* Consecutive allocations of the same mode in a basic block are combined into
   one by [Cfg_comballoc], as long as nothing that is a GC safepoint or that can
   raise comes between them (in practice: calls, including external calls such
   as [caml_modify], polls, trap handlers and, for local allocations, the
   beginning or end of a region). Branches end basic blocks and so also stop the
   combination. To model this, sizes record which kind of allocation, if any,
   can be combined with code placed immediately before or after them; see
   [seq]. *)
type alloc_kind =
  | Heap_alloc
  | Local_alloc

type size =
  { instructions : int;
    needs_frame : bool;
        (* The code contains a call, an allocation or a poll, so the enclosing
           function needs a stack frame. *)
    calls_ocaml : bool;
        (* The code contains a non-tail call to an OCaml function, so the
           enclosing function needs a stack check (see [Cfg_stack_checks]). *)
    first_alloc : alloc_kind option;
        (* The first allocation in the code, if nothing that stops the
           combination of allocations comes before it. *)
    last_alloc : alloc_kind option;
        (* The last allocation in the code, if nothing that stops the
           combination of allocations comes after it. *)
    straight_line : bool
        (* Nothing in the code stops the combination of allocations. *)
  }

(* Frame requirements and allocation barriers can differ between targets, e.g.
   an atomic operation is native on x86-64 but a call on arm64. *)
type t =
  { x86_64 : size;
    arm64 : size
  }

let zero_size =
  { instructions = 0;
    needs_frame = false;
    calls_ocaml = false;
    first_alloc = None;
    last_alloc = None;
    straight_line = true
  }

let zero = { x86_64 = zero_size; arm64 = zero_size }

let map t ~f = { x86_64 = f t.x86_64; arm64 = f t.arm64 }

let map2 a b ~f = { x86_64 = f a.x86_64 b.x86_64; arm64 = f a.arm64 b.arm64 }

let equal_alloc_kind k1 k2 =
  match k1, k2 with
  | Heap_alloc, Heap_alloc | Local_alloc, Local_alloc -> true
  | (Heap_alloc | Local_alloc), _ -> false

let equal_size
    { instructions = n1;
      needs_frame = f1;
      calls_ocaml = c1;
      first_alloc = fa1;
      last_alloc = la1;
      straight_line = s1
    }
    { instructions = n2;
      needs_frame = f2;
      calls_ocaml = c2;
      first_alloc = fa2;
      last_alloc = la2;
      straight_line = s2
    } =
  Int.equal n1 n2 && Bool.equal f1 f2 && Bool.equal c1 c2
  && Option.equal equal_alloc_kind fa1 fa2
  && Option.equal equal_alloc_kind la1 la2
  && Bool.equal s1 s2

let equal a b = equal_size a.x86_64 b.x86_64 && equal_size a.arm64 b.arm64

(* The size of two pieces of code whose relative placement is unknown (for
   example the arms of a switch), so that neither may have its allocations
   combined with code outside them. Use [seq] for code placed one after the
   other. *)
let ( + ) a b =
  map2 a b ~f:(fun a b ->
      { instructions = Int.add a.instructions b.instructions;
        needs_frame = a.needs_frame || b.needs_frame;
        calls_ocaml = a.calls_ocaml || b.calls_ocaml;
        first_alloc = None;
        last_alloc = None;
        straight_line = a.straight_line && b.straight_line
      })

let ( - ) a b =
  map2 a b ~f:(fun a b ->
      { a with instructions = Int.sub a.instructions b.instructions })

(* The same number of instructions on both architectures. *)
let both n =
  let size = { zero_size with instructions = n } in
  { x86_64 = size; arm64 = size }

let per_arch ~x86_64 ~arm64 =
  { x86_64 = { zero_size with instructions = x86_64 };
    arm64 = { zero_size with instructions = arm64 }
  }

(* Code, other than an allocation, that does not stop the combination of
   allocations around it. *)
let transparent_size t =
  { t with first_alloc = None; last_alloc = None; straight_line = true }

let transparent t = map t ~f:transparent_size

(* Code that stops the combination of allocations. *)
let barrier_size t =
  { t with first_alloc = None; last_alloc = None; straight_line = false }

let barrier t = map t ~f:barrier_size

(* An allocation, together with code that does not stop combinations (such as
   the initialisation of its fields). *)
let allocation kind t =
  map t ~f:(fun t ->
      { t with
        first_alloc = Some kind;
        last_alloc = Some kind;
        straight_line = true
      })

let scale k t =
  map t ~f:(fun t -> { t with instructions = Int.mul k t.instructions })

(* Code that contains a call, an allocation or a poll, and hence requires the
   enclosing function to have a stack frame. *)
let calls t = map t ~f:(fun t -> { t with needs_frame = true })

(* Code that contains a non-tail call to an OCaml function. *)
let calls_ocaml t =
  map t ~f:(fun t -> { t with needs_frame = true; calls_ocaml = true })

type arch =
  | X86_64
  | Arm64

(* The lookup is lazy since [Target_system.architecture] fails on unknown
   configurations and the target only matters when generating native code. Only
   x86-64 and arm64 are supported by [To_cmm]; other architectures are treated
   like arm64 as the closer approximation (fixed-length RISC instructions). *)
let arch =
  lazy
    (match Target_system.architecture () with
    | X86_64 | IA32 -> X86_64
    | AArch64 | ARM | POWER | Z | Riscv -> Arm64)

let target t =
  match Lazy.force arch with
  | X86_64 -> t.x86_64.instructions
  | Arm64 -> t.arm64.instructions

let of_int n = both n

let to_int t = target t

let create ~x86_64 ~arm64 = per_arch ~x86_64 ~arm64

let x86_64 t = t.x86_64.instructions

let arm64 t = t.arm64.instructions

let print ppf t =
  Format.fprintf ppf "%d (x86-64) / %d (arm64)" (x86_64 t) (arm64 t)

(* Allocation on the OCaml heap (fast path). On x86-64 (see [Lop (Alloc { mode =
   Heap })] in [amd64/emit.ml]):

   sub $n, %r15; cmp young_limit(%r14), %r15; jb; lea 8(%r15), %res

   followed by the store of the header word ([movq $imm32, -8(%res)]) and with
   an out-of-line [call caml_call_gc; jmp] stub emitted at the end of the
   function for each allocation site: 7 in total. On arm64
   ([assembly_code_for_fast_heap_allocation]):

   ldr tmp, young_limit; sub x27, x27, n; cmp x27, tmp; b.lo; add res, x27, 8

   then [mov tmp, hdr; str tmp, [res, -8]] and the [bl caml_call_gc; b] stub: 9
   in total. The stores of the fields are counted separately (one per field, see
   [field_store_size]). *)
let heap_alloc_size = calls (per_arch ~x86_64:7 ~arm64:9)

(* Allocation in a local region ([Lop (Alloc { mode = Local })]). x86-64:

   mov local_sp, %r; sub $n, %r; mov %r, local_sp; cmp local_limit, %r; jl; add
   local_top, %r; add $8, %r

   plus the header store and an out-of-line [call caml_call_local_realloc; jmp]
   stub. arm64 additionally needs loads of the limit and of the top of the
   region into a temporary, and two instructions for the header. *)
let local_alloc_size = calls (per_arch ~x86_64:10 ~arm64:13)

let alloc_size_for_mode (mode : Alloc_mode.For_allocations.t) =
  match mode with Heap _ -> heap_alloc_size | Local _ -> local_alloc_size

let alloc_kind_of_mode (mode : Alloc_mode.For_allocations.t) =
  match mode with Heap _ -> Heap_alloc | Local _ -> Local_alloc

(* When allocations are combined (see [alloc_kind]), the first one reserves the
   space for all of them and is followed by an [add] making its result point at
   the last block. Each of the others then only needs the address of its block
   computed from that of the next one ([lea -n(%r), %res] / [sub res, r, n]) and
   its header stored ([movq $hdr, -8(%res)] / [mov tmp, hdr; str tmp, [res,
   -8]]). The saving for each allocation combined with the previous one is thus
   the size of an allocation less these two (arm64: three) instructions and the
   [add], which is only needed once per group but is charged to each combined
   allocation since groups of two are the most common. *)
let combined_alloc_saving kind =
  let alloc =
    match kind with
    | Heap_alloc -> heap_alloc_size
    | Local_alloc -> local_alloc_size
  in
  per_arch ~x86_64:(Int.sub (x86_64 alloc) 3) ~arm64:(Int.sub (arm64 alloc) 4)

let seq_size a b ~saving_for_arch =
  let saving =
    match a.last_alloc, b.first_alloc with
    | Some kind_a, Some kind_b when equal_alloc_kind kind_a kind_b ->
      saving_for_arch (combined_alloc_saving kind_a)
    | (None | Some _), _ -> 0
  in
  { instructions = Int.sub (Int.add a.instructions b.instructions) saving;
    needs_frame = a.needs_frame || b.needs_frame;
    calls_ocaml = a.calls_ocaml || b.calls_ocaml;
    first_alloc =
      (match a.first_alloc with
      | Some _ -> a.first_alloc
      | None -> if a.straight_line then b.first_alloc else None);
    last_alloc =
      (match b.last_alloc with
      | Some _ -> b.last_alloc
      | None -> if b.straight_line then a.last_alloc else None);
    straight_line = a.straight_line && b.straight_line
  }

let seq a b =
  { x86_64 = seq_size a.x86_64 b.x86_64 ~saving_for_arch:x86_64;
    arm64 = seq_size a.arm64 b.arm64 ~saving_for_arch:arm64
  }

let with_out_of_line t ~out_of_line =
  map2 t (t + out_of_line) ~f:(fun t sum ->
      { sum with
        first_alloc = t.first_alloc;
        last_alloc = t.last_alloc;
        straight_line = t.straight_line
      })

(* Direct call to a known function: a single [call] / [bl] instruction, plus an
   allowance for spilling and reloading values that are live across the call
   (all registers are destroyed). The moves of the arguments into place are
   charged separately, see [apply]; tail calls are cheaper (a single jump). *)
let direct_call_size = calls_ocaml (both 2)

(* Indirect call: a load of the code pointer from the closure, followed by [call
   *%reg] / [blr], with the same allowance as above. Calls of unknown arity go
   through [caml_applyN], which is a direct call, but with the closure passed as
   an extra argument (and the arity check is not inlined). *)
let indirect_call_size = calls_ocaml (both 4)

(* Loading the address of a symbol into a register: [lea sym(%rip)] on x86-64;
   [adrp; add] (or [adrp; ldr] via the GOT) on arm64. *)
let symbol_address_size = per_arch ~x86_64:1 ~arm64:2

(* External (C) calls, see [Lcall_op (Lextcall _)]. When the callee may
   allocate, the address of the callee is loaded and [caml_c_call] is called;
   otherwise (stack checks are enabled in the default configuration) the stack
   pointer is switched to the C stack around the call:

   mov %rsp, %r13; mov c_stack(%r14), %rsp; call sym; mov %r13, %rsp

   (arm64: [mov x19, sp; ldr tmp, c_stack; mov sp, tmp; bl; mov sp, x19]). In
   both cases nearly all registers are destroyed. On x86-64 the arguments must
   also be moved from the OCaml argument registers to the C ones; on arm64 the
   first eight arguments are already in place. *)
let c_call_size = calls (per_arch ~x86_64:6 ~arm64:6)

(* [caml_modify], [caml_modify_local] and [caml_initialize] are external calls
   that do not allocate; the address of the field is computed first ([lea] /
   [add]) unless it is the first field. *)
let caml_modify_size = c_call_size + both 1

(* Initialisation, in the module initialiser, of a field of a statically
   allocated block whose value is only known at runtime: [caml_initialize] on
   the address of the field for values, or a plain store for unboxed numbers. *)
let static_field_initialization ~pointer =
  if pointer
  then barrier (caml_modify_size + symbol_address_size)
  else transparent (both 1 + symbol_address_size)

(* Compare-and-branch: [cmp]/[test] followed by a conditional jump (arm64: [cmp;
   b.cond], or a single [cbz]/[tbz]). When the scrutinee is a comparison the
   compare is already charged to that primitive. *)
let if_then_else_size = both 2

(* Jump table (see [Lswitch]). x86-64:

   lea table(%rip), %rax; movsxd (%rax,%idx,4), %rdx; add %rdx, %rax; jmp *%rax

   followed by one 32-bit entry per value in the range of the switch. arm64:
   [adr; add tmp, tmp, idx, lsl 2; br] followed by one [b] per entry. *)
let jump_table_size = per_arch ~x86_64:4 ~arm64:3

(* Comparisons producing a boolean, whether integer or float. Almost always
   consumed by a branch, in which case the compare is fused with the jump (which
   is charged to the switch), leaving just the [cmp]; when the result is needed
   as a value there is also a [setcc; movzx] (arm64: [cset]). *)
let comparison_size = both 1

(* arm64 emits a [dmb ishld] barrier before every word-sized store that is an
   assignment (as opposed to an initialisation); see [Lop (Store _)] in
   [arm64/emit.ml]. *)
let assignment_store_barrier = per_arch ~x86_64:0 ~arm64:1

(* Moving a [Simple] into a register, e.g. as an argument: constants need a [mov
   $imm] and symbols need their address loaded. Arguments of calls must be
   placed in specific registers, which typically needs a move (or a spill and
   reload) per variable; arguments of jumps to continuations are more often
   already in place. *)
let move_size ~for_call simple =
  Simple.pattern_match simple
    ~const:(fun _ -> both 1)
    ~name:(fun name ~coercion:_ ->
      Name.pattern_match name
        ~var:(fun _ -> if for_call then both 1 else zero)
        ~symbol:(fun _ -> symbol_address_size))

let moves_size ~for_call simples =
  List.fold_left
    (fun size simple -> size + move_size ~for_call simple)
    zero simples

(* Tagging an integer: [lea 1(%r,%r)] on x86-64; [lsl; orr] on arm64. *)
let tag_size = per_arch ~x86_64:1 ~arm64:2

(* Untagging: [sar $1] / [asr]. *)
let untag_size = both 1

(* Extra cost of a memory access whose address involves a scaled index or a
   register offset: none on x86-64, where it is folded into the addressing mode;
   one [add] (with a shifted register) on arm64. *)
let indexed_access_extra = per_arch ~x86_64:0 ~arm64:1

(* Extra cost of storing a constant: none on x86-64 ([movq $imm32, mem]); a
   [mov] into a register on arm64. *)
let constant_store_extra = per_arch ~x86_64:0 ~arm64:1

(* Extra cost of materialising the value to be stored. *)
let stored_value_extra simple =
  Simple.pattern_match simple
    ~const:(fun _ -> constant_store_extra)
    ~name:(fun name ~coercion:_ ->
      Name.pattern_match name
        ~var:(fun _ -> zero)
        ~symbol:(fun _ -> symbol_address_size))

(* Sign extension of an n-bit value held in a register: [movsxd] for 32-bit
   values and [shl; sar] otherwise on x86-64; [sxtb]/[sxth]/[sxtw]/[sbfx] on
   arm64. *)
let sign_extension_size (kind : Flambda_kind.Standard_int.t) =
  match kind with
  | Naked_int32 -> both 1
  | Naked_int8 | Naked_int16 | Naked_immediate -> per_arch ~x86_64:2 ~arm64:1
  | Naked_int64 | Naked_nativeint | Tagged_immediate -> zero

(* Prologue and epilogue of a function that needs a stack frame, i.e. one that
   contains a call, an allocation or a poll (see [needs_frame]). x86-64: [sub
   $n, %rsp] and [add $n, %rsp] (three more with frame pointers); arm64: [sub
   sp, sp, n; str lr, [sp, ...]] and [ldr lr, [sp, ...]; add sp, sp, n]. Leaf
   functions need neither. The [ret] itself is charged to the jump to the return
   continuation. *)
let function_frame_size = per_arch ~x86_64:2 ~arm64:4

(* Functions containing a non-tail call to an OCaml function (or with a large
   frame, which is not modelled) also get a stack check at entry (see
   [Cfg_stack_checks]). x86-64: [lea -n(%rsp), %r10; cmp current_stack(%r14),
   %r10; jb] plus an out-of-line [push $n; call caml_call_realloc_stack; add $8,
   %rsp; jmp] stub; arm64: [ldr; add; cmp; b.lo] plus [mov; stp; bl; ldp; b].

   Stack checks are disabled by default on x86-64 (see [--enable-stack-checks]
   in [configure.ac]) but always enabled on arm64. *)
let stack_check_size =
  per_arch ~x86_64:(if Config.no_stack_checks then 0 else 7) ~arm64:9

let add_function_frame t =
  let finish size ~frame ~stack_check =
    let instructions =
      Int.add size.instructions
        (Int.add
           (if size.needs_frame then frame else 0)
           (if size.calls_ocaml then stack_check else 0))
    in
    (* A completed function contributes its instructions, not its frame or
       allocation context, when included in another function's metrics. *)
    { zero_size with instructions }
  in
  { x86_64 =
      finish t.x86_64
        ~frame:(x86_64 function_frame_size)
        ~stack_check:(x86_64 stack_check_size);
    arm64 =
      finish t.arm64
        ~frame:(arm64 function_frame_size)
        ~stack_check:(arm64 stack_check_size)
  }

(* Storing one field of a freshly allocated block: one store, plus the extra
   instructions needed to materialise constants and symbol addresses. *)
let field_store_size simple =
  Simple.pattern_match simple
    ~const:(fun _ -> both 1 + constant_store_extra)
    ~name:(fun name ~coercion:_ ->
      Name.pattern_match name
        ~var:(fun _ -> both 1)
        ~symbol:(fun _ -> both 1 + symbol_address_size))

(* Helper functions for computing sizes of primitives *)

let unary_int_prim_size kind op =
  (* 16-bit swaps are [xchg %ah, %al; movzx %ax, %rax] on x86-64 and [rev16] on
     arm64; 32- and 64-bit swaps are a single [bswap] / [rev], with a sign
     extension for the 32-bit case. *)
  let swap16 = per_arch ~x86_64:2 ~arm64:1 in
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.unary_int_arith_op) )
  with
  | Tagged_immediate, Swap_byte_endianness ->
    (* Untag, swap, mask to 16 bits, tag. *)
    untag_size + swap16 + both 1 + tag_size
  | Naked_immediate, Swap_byte_endianness ->
    (* Sign extension to 16 bits, swap, mask. *)
    sign_extension_size Naked_int16 + swap16 + both 1
  | Naked_int8, Swap_byte_endianness -> zero
  | Naked_int16, Swap_byte_endianness ->
    (* Swap, then sign extension. *)
    swap16 + sign_extension_size Naked_int16
  | Naked_int32, Swap_byte_endianness ->
    both 1 + sign_extension_size Naked_int32
  | (Naked_int64 | Naked_nativeint), Swap_byte_endianness -> both 1

let arith_conversion_size src dst =
  match
    ( (src : Flambda_kind.Standard_int_or_float.t),
      (dst : Flambda_kind.Standard_int_or_float.t) )
  with
  | Naked_float, Naked_float
  | Naked_float32, Naked_float32
  | Tagged_immediate, Tagged_immediate ->
    zero
  (* [cvtsd2ss] / [cvtss2sd] / [fcvt] *)
  | Naked_float, Naked_float32 | Naked_float32, Naked_float -> both 1
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Tagged_immediate ) ->
    tag_size
  (* Untagging, possibly followed by a narrowing sign extension (which is two
     shifts on both architectures since the untagging shift is merged into
     it). *)
  | Tagged_immediate, (Naked_immediate | Naked_int64 | Naked_nativeint) ->
    untag_size
  | Tagged_immediate, (Naked_int8 | Naked_int16 | Naked_int32) -> both 2
  (* Widening conversions are no-ops: values are kept sign-extended in
     registers. *)
  | ( Naked_int8,
      ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ) )
  | ( Naked_int16,
      ( Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ) )
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int64 | Naked_nativeint)
  | Naked_nativeint, (Naked_int64 | Naked_nativeint)
  | Naked_immediate, (Naked_int64 | Naked_nativeint | Naked_immediate) ->
    zero
  (* Narrowing conversions: see [sign_extension_size]. Naked immediates are
     63-bit values and so are narrowing targets for 64-bit kinds. *)
  | (Naked_int64 | Naked_nativeint), Naked_immediate ->
    sign_extension_size Naked_immediate
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Naked_int32 ->
    sign_extension_size Naked_int32
  | ( (Naked_int64 | Naked_nativeint | Naked_immediate | Naked_int32),
      (Naked_int8 | Naked_int16) )
  | Naked_int16, Naked_int8 ->
    sign_extension_size Naked_int8
  (* Integer to float: [cvtsi2sd] / [scvtf], after untagging if needed. *)
  | ( ( Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
      | Naked_nativeint ),
      (Naked_float | Naked_float32) ) ->
    both 1
  | Tagged_immediate, (Naked_float | Naked_float32) -> untag_size + both 1
  (* Float to integer: [cvttsd2si] / [fcvtzs], then narrowing or tagging as
     above. *)
  | (Naked_float | Naked_float32), (Naked_int64 | Naked_nativeint) -> both 1
  | (Naked_float | Naked_float32), Tagged_immediate -> both 1 + tag_size
  | (Naked_float | Naked_float32), Naked_int32 ->
    both 1 + sign_extension_size Naked_int32
  | (Naked_float | Naked_float32), (Naked_immediate | Naked_int8 | Naked_int16)
    ->
    both 1 + sign_extension_size Naked_int8

let unbox_number kind =
  (* Box/unbox are identities in JSIR *)
  if !Clflags.jsir
  then zero
  else
    match (kind : Flambda_kind.Boxable_number.t) with
    (* A single load; the field offset is folded into the addressing mode. *)
    | Naked_float | Naked_float32 | Naked_vec128 | Naked_vec256 | Naked_vec512
    | Naked_mask | Naked_int32 | Naked_int64 | Naked_nativeint ->
      both 1

let box_number0 ~alloc_size kind =
  (* Box/unbox are identities in JSIR *)
  if !Clflags.jsir
  then zero
  else
    match (kind : Flambda_kind.Boxable_number.t) with
    (* Allocation plus a single store of the payload. *)
    | Naked_float | Naked_vec128 | Naked_vec256 | Naked_vec512 | Naked_mask ->
      alloc_size + both 1
    (* Custom blocks: the address of the custom operations table must be loaded
       and stored, then the payload stored. *)
    | Naked_float32 | Naked_int64 | Naked_nativeint ->
      alloc_size + symbol_address_size + both 2
    (* As above, with a sign extension of the payload beforehand. *)
    | Naked_int32 ->
      alloc_size + symbol_address_size + both 2
      + sign_extension_size Naked_int32

let block_load (kind : Flambda_primitive.Block_access_kind.t) =
  (* A single load with the field offset folded into the addressing mode. *)
  match kind with
  | Values _ | Naked_floats _ | Mixed _ -> both 1

let array_load (kind : Flambda_primitive.Array_load_kind.t) =
  match kind with
  (* One load (see [indexed_access_extra] for the address computation). *)
  | Immediates | Gc_ignorable_values | Values | Naked_floats | Naked_float32s
  | Naked_ints | Naked_int16s | Naked_int32s | Naked_int64s | Naked_nativeints
    ->
    both 1 + indexed_access_extra
  (* Byte arrays need the index untagged first, since the scale is one. *)
  | Naked_int8s -> untag_size + both 1 + indexed_access_extra
  (* Vector loads need the index scaled explicitly, and masks need a move
     between register classes. *)
  | Naked_vec128s | Naked_vec256s | Naked_vec512s | Naked_masks ->
    both 2 + indexed_access_extra

let block_set (kind : Flambda_primitive.Block_access_kind.t)
    (init : Flambda_primitive.Init_or_assign.t) ~new_value =
  let barrier =
    match init with
    | Assignment (Heap | Local) -> assignment_store_barrier
    | Initialization -> zero
  in
  match kind, init with
  (* Stores of values that might be pointers go through [caml_modify],
     [caml_modify_local] or [caml_initialize] (see [Cmm_helpers.setfield]),
     according to the mode. *)
  | ( ( Values { field_kind = Any_value; _ }
      | Mixed { field_kind = Value_prefix Any_value; _ } ),
      (Assignment (Heap | Local) | Initialization) ) ->
    caml_modify_size + stored_value_extra new_value
  (* Word-sized stores, with a barrier on arm64 when assigning. *)
  | ( ( Values { field_kind = Immediate; _ }
      | Mixed { field_kind = Value_prefix Immediate; _ }
      | Mixed
          { field_kind =
              Flat_suffix (Naked_int64 | Naked_nativeint | Naked_immediate);
            _
          } ),
      (Assignment (Heap | Local) | Initialization) ) ->
    both 1 + barrier + stored_value_extra new_value
  (* Narrower and floating-point stores: no barrier. *)
  | ( ( Mixed
          { field_kind =
              Flat_suffix
                ( Naked_float | Naked_float32 | Naked_int8 | Naked_int16
                | Naked_int32 | Naked_vec128 | Naked_vec256 | Naked_vec512
                | Naked_mask );
            _
          }
      | Naked_floats _ ),
      (Assignment (Heap | Local) | Initialization) ) ->
    both 1 + stored_value_extra new_value

let array_set (kind : Flambda_primitive.Array_set_kind.t) ~new_value =
  match kind with
  (* See [block_set]; the address computation involves the index. *)
  | Values (Assignment (Heap | Local) | Initialization) ->
    caml_modify_size + indexed_access_extra + stored_value_extra new_value
  (* Word-sized stores (always assignments): see [array_load] for the addressing
     and [assignment_store_barrier]. *)
  | Immediates | Gc_ignorable_values | Naked_ints | Naked_int64s
  | Naked_nativeints ->
    both 1 + indexed_access_extra + assignment_store_barrier
    + stored_value_extra new_value
  | Naked_floats | Naked_float32s | Naked_int16s | Naked_int32s ->
    both 1 + indexed_access_extra + stored_value_extra new_value
  | Naked_int8s ->
    untag_size + both 1 + indexed_access_extra + stored_value_extra new_value
  | Naked_vec128s | Naked_vec256s | Naked_vec512s | Naked_masks ->
    both 2 + indexed_access_extra + stored_value_extra new_value

let string_or_bigstring_load kind width =
  let start_address_load =
    match (kind : Flambda_primitive.string_like_value) with
    | String | Bytes -> zero
    (* Load of the data pointer from the bigarray header. *)
    | Bigstring -> both 1
  in
  (* The index is already untagged; unaligned accesses are permitted on x86-64
     and arm64, and sign extensions are folded into the loads. So every width is
     a single load, plus the address computation. *)
  let elt_load =
    match (width : Flambda_primitive.string_accessor_width) with
    | Eight | Eight_signed | Sixteen | Sixteen_signed | Thirty_two | Single
    | Sixty_four | One_twenty_eight _ | Two_fifty_six _ | Five_twelve _ | Mask
      ->
      both 1 + indexed_access_extra
  in
  start_address_load + elt_load

(* Stores have the same size as the corresponding loads. *)
let bytes_like_set kind width =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes -> string_or_bigstring_load Bytes width
  | Bigstring -> string_or_bigstring_load Bigstring width

(* [cqo; idiv] (or [xor %edx, %edx; div]) on x86-64, which also yields the
   remainder; [sdiv] / [udiv] on arm64, with a further [msub] for the
   remainder. *)
let division_size = per_arch ~x86_64:2 ~arm64:1

let modulo_size = both 2

let naked_div_or_mod ~signed ~is_mod (kind : Flambda_kind.Standard_int.t) =
  let operation = if is_mod then modulo_size else division_size in
  (* On x86-64, [Cmm_helpers.make_safe_divmod] guards against [min_int / -1]
     when the dividend might be [min_int], which is only possible for
     register-width kinds. arm64 division does not trap on overflow:

     cmp $-1, %divisor; jne; neg %dividend (or xor); jmp *)
  let overflow_check =
    match kind with
    | (Naked_int64 | Naked_nativeint) when signed -> per_arch ~x86_64:4 ~arm64:0
    | Naked_int64 | Naked_nativeint | Naked_immediate | Naked_int8 | Naked_int16
    | Naked_int32 | Tagged_immediate ->
      zero
  in
  (* Unsigned operations on kinds narrower than a register need both operands
     zero-extended first, since values are kept sign-extended. *)
  let operand_zero_extension =
    match kind with
    | (Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32) when not signed
      ->
      both 2
    | Naked_int64 | Naked_nativeint | Naked_immediate | Naked_int8 | Naked_int16
    | Naked_int32 | Tagged_immediate ->
      zero
  in
  operand_zero_extension + operation + overflow_check + sign_extension_size kind

let binary_int_arith_primitive kind op =
  match
    ( (kind : Flambda_kind.Standard_int.t),
      (op : Flambda_primitive.binary_int_arith_op) )
  with
  (* Tagged integers. Addition is a single [lea -1(%a,%b)] on x86-64 and [add;
     sub 1] on arm64; subtraction is [sub; inc] / [sub; add 1]. *)
  | Tagged_immediate, Add -> per_arch ~x86_64:1 ~arm64:2
  | Tagged_immediate, Sub -> both 2
  (* [lea -1(%a); sar $1, %b; imul; lea 1(%r)] (arm64 similar). *)
  | Tagged_immediate, Mul -> both 4
  (* Untag both operands, divide, tag the result. *)
  | Tagged_immediate, Div (Signed | Unsigned) ->
    scale 2 untag_size + division_size + tag_size
  | Tagged_immediate, Mod (Signed | Unsigned) ->
    scale 2 untag_size + modulo_size + tag_size
  | Tagged_immediate, (And | Or) -> both 1
  (* [xor; or $1] *)
  | Tagged_immediate, Xor -> both 2
  (* Naked integers: one instruction, plus a sign extension of the result for
     kinds narrower than a register. *)
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      (Add | Sub | Mul | And | Or | Xor) ) ->
    both 1 + sign_extension_size kind
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Div Signed ) ->
    naked_div_or_mod ~signed:true ~is_mod:false kind
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Mod Signed ) ->
    naked_div_or_mod ~signed:true ~is_mod:true kind
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Div Unsigned ) ->
    naked_div_or_mod ~signed:false ~is_mod:false kind
  | ( ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64 | Naked_nativeint
      | Naked_immediate ),
      Mod Unsigned ) ->
    naked_div_or_mod ~signed:false ~is_mod:true kind

(* The value of a constant shift amount, if any. *)
let constant_shift_amount shift =
  Simple.pattern_match shift
    ~const:(fun const ->
      match Reg_width_const.descr const with
      | Naked_immediate i -> Target_ocaml_int.to_int_option i
      | Tagged_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int8 _
      | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
      | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Naked_mask _ | Null
      | Poison _ ->
        None)
    ~name:(fun _ ~coercion:_ -> None)

let binary_int_shift_primitive kind op ~shift =
  match
    (kind : Flambda_kind.Standard_int.t), (op : Flambda_primitive.int_shift_op)
  with
  (* Tagged integers: [lea -1; shl %cl; lea 1] and [shr/sar %cl; or $1] (arm64:
     [sub 1; lsl; add 1] and [lsr/asr; orr 1]). *)
  | Tagged_immediate, Lsl -> (
    (* [Cmm_helpers] rewrites [((x - 1) lsl c) + 1] to [(x lsl c) + k]; for a
       constant [c] this is a single [lea] on x86-64 when [c <= 3] (and [shl;
       add] otherwise), and on arm64 the shift usually folds into a following
       shifted-register [add] (or is [lsl; sub] on its own). *)
    match constant_shift_amount shift with
    | Some c when c <= 3 -> both 1
    | Some _ -> both 2
    | None -> both 3)
  | Tagged_immediate, (Lsr | Asr) -> both 2
  (* Register-width naked integers: a single shift. *)
  | (Naked_int64 | Naked_nativeint), (Lsl | Lsr | Asr) -> both 1
  (* Narrower kinds: left shifts need the result sign-extended; logical right
     shifts need the operand zero-extended first; arithmetic right shifts need
     nothing extra. *)
  | (Naked_int8 | Naked_int16 | Naked_int32 | Naked_immediate), Lsl ->
    both 1 + sign_extension_size kind
  | (Naked_int8 | Naked_int16 | Naked_int32 | Naked_immediate), Lsr -> both 2
  | (Naked_int8 | Naked_int16 | Naked_int32 | Naked_immediate), Asr -> both 1

let binary_int_comp_primitive (kind : Flambda_kind.Standard_int.t)
    (_cmp : Flambda_primitive.signed_or_unsigned Flambda_primitive.comparison) =
  match kind with
  | Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32
  | Naked_int64 | Naked_nativeint ->
    comparison_size

let int_comparison_like_compare_functions (kind : Flambda_kind.Standard_int.t)
    (_signedness : Flambda_primitive.signed_or_unsigned) =
  (* [Cmm_helpers.mk_compare_ints_untagged] produces [csel (a >= b) (a > b)
     (-1)], which on x86-64 is:

     cmp; setg; movzx; mov $-1, %res; cmp; cmovge

     and on arm64 [cmp; cset; mov; cmp; csel]. *)
  match kind with
  | Tagged_immediate | Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32
  | Naked_int64 | Naked_nativeint ->
    per_arch ~x86_64:6 ~arm64:5

(* [addsd] / [fadd] etc. *)
let binary_float_arith_primitive _width _op = both 1

let binary_float_comp_primitive _width _op = comparison_size

(* [Cmm_helpers.mk_compare_floats_gen] computes four comparisons (each [cmpsd;
   movq; neg] when materialised on x86-64; [fcmp; cset] on arm64) and combines
   them with two subtractions and an addition. *)
let float_comparison_like_compare_functions _width =
  per_arch ~x86_64:12 ~arm64:8 + both 3

let bigarray_access_size (kind : Flambda_primitive.Bigarray_kind.t) =
  (* Load of the data pointer, then the access itself. *)
  let base = both 2 + indexed_access_extra in
  match kind with
  | Complex32 | Complex64 ->
    (* Two accesses, and for loads an allocation of the boxed complex. *)
    base + both 1
  | Float16 ->
    (* Conversion between half and double precision is an external call (see
       [Cmm_helpers.float_of_float16]). *)
    base + c_call_size
  | Float32 | Float32_t | Float64 | Sint8 | Uint8 | Sint16 | Uint16 | Int32
  | Int64 | Int_width_int | Targetint_width_int ->
    base

(* Primitives sizes *)

let nullary_prim_size prim =
  match (prim : Flambda_primitive.nullary_primitive) with
  | Invalid _ -> zero
  | Optimised_out _ -> zero
  (* Load of the 16-bit semaphore (via the address of the symbol on arm64), [cmp
     $0], [setne], [movzx] / [cset]. *)
  | Probe_is_enabled { name = _; enabled_at_init = _ } ->
    per_arch ~x86_64:4 ~arm64:5
  | Enter_inlined_apply _ -> zero
  (* One load from the domain state. *)
  | Dls_get | Tls_get | Domain_index -> both 1
  (* [cmp young_limit(%r14), %r15; jbe] (arm64: [ldr; cmp; b.ls]) plus an
     out-of-line [call caml_call_gc; jmp] stub. *)
  | Poll -> calls (per_arch ~x86_64:4 ~arm64:5)
  (* [pause] / [yield] (with a poll as well unless poll insertion is enabled,
     which it is by default). *)
  | Cpu_relax -> both 1

let unary_prim_size prim =
  match (prim : Flambda_primitive.unary_primitive) with
  | Block_load { kind; _ } -> block_load kind
  (* External calls to [caml_obj_dup]. *)
  | Duplicate_array _ | Duplicate_block _ | Obj_dup _ -> c_call_size
  (* [and $1] / [test $1] and [cmp $0] respectively. *)
  | Is_int _ | Is_null -> both 1
  (* [movzbl -8(%r)] / [ldurb] *)
  | Get_tag -> both 1
  | Array_length array_kind -> (
    match array_kind with
    (* Load of the header, shift, [or $1]. *)
    | Array_kind
        ( Immediates | Values | Gc_ignorable_values | Naked_floats
        | Unboxed_product _ )
    | Float_array_opt_dynamic ->
      both 3
    (* Load of the header, shift, tag. *)
    | Array_kind (Naked_ints | Naked_int64s | Naked_nativeints | Naked_masks) ->
      both 2 + tag_size
    (* As above, with an extra shift to divide by the vector width. *)
    | Array_kind (Naked_vec128s | Naked_vec256s | Naked_vec512s) ->
      both 3 + tag_size
    (* Packed arrays: the number of elements in the last word is computed from
       the tag: header load, shift, tag load, [shl], [and], [sub], tag (see
       [Cmm_helpers.unboxed_or_untagged_packed_array_length]). *)
    | Array_kind (Naked_int8s | Naked_int16s | Naked_int32s | Naked_float32s) ->
      both 6 + tag_size)
  (* A single load with the field offset folded into the addressing mode. *)
  | Bigarray_length _ -> both 1
  (* Header load, shift, [shl $3], [sub $1], load of the padding byte (indexed),
     [sub] (see [Cmm_helpers.string_length]). *)
  | String_length _ -> both 6 + indexed_access_extra
  (* [lea -1(%r)] / [sub] *)
  | Int_as_pointer _ -> both 1
  | Opaque_identity _ -> zero
  | Int_arith (kind, op) -> unary_int_prim_size kind op
  (* [xorpd] / [andpd] with a constant mask operand in memory; [fneg] /
     [fabs]. *)
  | Float_arith _ -> both 1
  | Num_conv { src; dst } -> arith_conversion_size src dst
  (* [xor $2] / [eor] *)
  | Boolean_not -> both 1
  | Reinterpret_boxed_vector -> zero
  | Reinterpret_64_bit_word reinterpret -> (
    match reinterpret with
    | Tagged_int63_as_unboxed_int64 -> zero
    | Unboxed_int64_as_tagged_int63 -> (* Needs a logical OR. *) both 1
    | Unboxed_int64_as_unboxed_float64 | Unboxed_float64_as_unboxed_int64 ->
      (* Needs a move between register classes. *) both 1)
  | Unbox_number k -> unbox_number k
  | Untag_immediate ->
    if !Clflags.jsir
    then zero (* Numbers are not tagged in JSIR *)
    else untag_size
  | Box_number (k, alloc_mode) ->
    box_number0 ~alloc_size:(alloc_size_for_mode alloc_mode) k
  | Tag_immediate ->
    if !Clflags.jsir
    then zero
    (* Numbers are not tagged in JSIR *)
    else tag_size
  (* [lea n(%r)] / [add] *)
  | Project_function_slot _ -> both 1
  (* A single load. *)
  | Project_value_slot _ -> both 1
  (* [test $1; jne; test; je; movzbl -8(%r); cmp $253; sete; movzx] (arm64:
     [tbnz; cbz; ldurb; cmp; cset] plus a jump). *)
  | Is_boxed_float -> per_arch ~x86_64:8 ~arm64:7
  (* [movzbl -8(%r); cmp $254] and either a branch or [sete; movzx]. *)
  | Is_flat_float_array -> both 3
  (* A store to the domain state. *)
  | End_region { ghost } | End_try_region { ghost } ->
    if ghost then zero else both 1
  (* A single load. *)
  | Get_header -> both 1
  | Peek _ -> both 1
  (* Allocation of a one-field block. *)
  | Make_lazy _ -> heap_alloc_size + both 1

let binary_prim_size prim ~arg2 =
  match (prim : Flambda_primitive.binary_primitive) with
  | Block_set { kind; init; _ } -> block_set kind init ~new_value:arg2
  | Array_load (_kind, load_kind, _mut) -> array_load load_kind
  | String_or_bigstring_load (kind, width) ->
    string_or_bigstring_load kind width
  | Bigarray_load (_dims, ((Complex32 | Complex64) as kind), _layout) ->
    (* The result is boxed. *)
    bigarray_access_size kind + heap_alloc_size + both 2
  | Bigarray_load (_dims, kind, _layout) -> bigarray_access_size kind
  | Phys_equal _op -> comparison_size
  | Int_arith (kind, op) -> binary_int_arith_primitive kind op
  | Int_shift (kind, op) -> binary_int_shift_primitive kind op ~shift:arg2
  | Int_comp (kind, Yielding_bool cmp) -> binary_int_comp_primitive kind cmp
  | Int_comp (kind, Yielding_int_like_compare_functions signedness) ->
    int_comparison_like_compare_functions kind signedness
  | Float_arith (width, op) -> binary_float_arith_primitive width op
  | Float_comp (width, Yielding_bool cmp) ->
    binary_float_comp_primitive width cmp
  | Float_comp (width, Yielding_int_like_compare_functions ()) ->
    float_comparison_like_compare_functions width
  (* Load of the data pointer, [add], [and]. *)
  | Bigarray_get_alignment _ -> both 3
  (* A plain [mov] on x86-64; [dmb ishld; ldar] on arm64, where the address must
     also be computed into a register ([add]) when the offset is non-zero. *)
  | Atomic_load_field _ ->
    let address =
      Simple.pattern_match arg2
        ~const:(fun const ->
          match Reg_width_const.descr const with
          | Tagged_immediate i
            when Option.equal Int.equal
                   (Target_ocaml_int.to_int_option i)
                   (Some 0) ->
            0
          | Tagged_immediate _ | Naked_immediate _ | Naked_float _
          | Naked_float32 _ | Naked_int8 _ | Naked_int16 _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _ | Naked_vec256 _
          | Naked_vec512 _ | Naked_mask _ | Null | Poison _ ->
            1)
        ~name:(fun _ ~coercion:_ -> 2)
    in
    per_arch ~x86_64:1 ~arm64:(Int.add 2 address)
  (* A store to a computed address; word-sized ones get the barrier. *)
  | Poke kind -> (
    let store = both 1 + stored_value_extra arg2 in
    match kind with
    | Naked_int64 | Naked_nativeint | Naked_immediate | Tagged_immediate ->
      store + assignment_store_barrier
    | Naked_int8 | Naked_int16 | Naked_int32 | Naked_float | Naked_float32 ->
      store)
  (* A single load from a computed address. *)
  | Read_offset _ -> both 1 + indexed_access_extra

(* Atomic operations other than loads are only supported natively on x86-64 (see
   [Proc.operation_supported]); elsewhere they are external calls. *)
let native_atomic ~x86_64 =
  { x86_64 = { zero_size with instructions = x86_64 };
    arm64 = c_call_size.arm64
  }

let ternary_prim_size prim ~arg3 =
  match (prim : Flambda_primitive.ternary_primitive) with
  | Array_set (_kind, set_kind) -> array_set set_kind ~new_value:arg3
  | Bytes_or_bigstring_set (kind, width) -> bytes_like_set kind width
  | Bigarray_set (_dims, ((Complex32 | Complex64) as kind), _layout) ->
    (* The two components of the new value must be loaded first. *)
    bigarray_access_size kind + both 2
  | Bigarray_set (_dims, kind, _layout) -> bigarray_access_size kind
  (* Untagging of the operand, then e.g. [lock xadd]. *)
  | Atomic_field_int_arith _ -> native_atomic ~x86_64:2
  (* [xchg]; values that might be pointers go through the runtime. *)
  | Atomic_set_field (Immediate, (Heap | Local))
  | Atomic_exchange_field (Immediate, (Heap | Local)) ->
    native_atomic ~x86_64:1
  | Atomic_set_field (Any_value, (Heap | Local))
  | Atomic_exchange_field (Any_value, (Heap | Local)) ->
    c_call_size
  | Write_offset (write_offset_kind, kind, (Heap | Local)) ->
    if Flambda_kind.With_subkind.must_be_gc_scannable kind
    then
      (* [caml_modify] on the computed address, or [caml_modify_local] with the
         byte offset converted to a field index. *)
      match write_offset_kind with
      | Into_block -> caml_modify_size
      (* [test %base; je; mov; jmp] around the above. *)
      | Into_block_or_off_heap -> caml_modify_size + both 4
    else
      (* A store to a computed address (a word-sized one when the kind is an
         integer of full width). *)
      let barrier =
        match Flambda_kind.With_subkind.kind kind with
        | Value | Naked_number (Naked_int64 | Naked_nativeint | Naked_immediate)
          ->
          assignment_store_barrier
        | Naked_number
            ( Naked_int8 | Naked_int16 | Naked_int32 | Naked_float
            | Naked_float32 | Naked_vec128 | Naked_vec256 | Naked_vec512
            | Naked_mask )
        | Region | Rec_info ->
          zero
      in
      both 1 + indexed_access_extra + barrier + stored_value_extra arg3

let quaternary_prim_size prim =
  match (prim : Flambda_primitive.quaternary_primitive) with
  (* [mov %old, %rax; lock cmpxchg; sete; movzx], then tagging. *)
  | Atomic_compare_and_set_field (Immediate, (Heap | Local)) ->
    native_atomic ~x86_64:5
  (* [mov %old, %rax; lock cmpxchg] *)
  | Atomic_compare_exchange_field
      { atomic_kind = _; args_kind = Immediate; mode = Heap | Local } ->
    native_atomic ~x86_64:2
  | Atomic_compare_and_set_field (Any_value, (Heap | Local))
  | Atomic_compare_exchange_field
      { atomic_kind = _; args_kind = Any_value; mode = Heap | Local } ->
    c_call_size

(* [Cmm_helpers.make_alloc_generic] uses an external allocation followed by
   field initialisation above [max_young_wosize]. Packed and naked-number arrays
   use [Calloc] directly instead; this layout only describes the
   generic-allocation path. *)
type generic_allocation_layout =
  { num_words : int;
    num_scannable_fields : int;
    num_alloc_args : int
  }

let generic_allocation_layout (prim : Flambda_primitive.variadic_primitive)
    ~num_fields =
  let regular ~scannable =
    { num_words = num_fields;
      num_scannable_fields = (if scannable then num_fields else 0);
      num_alloc_args = 2
    }
  in
  match prim with
  | Make_block (Values _, _, mode)
  | Make_array ((Immediates | Values | Gc_ignorable_values), _, mode) ->
    Some (mode, regular ~scannable:true)
  | Make_block (Naked_floats, _, mode) | Make_array (Naked_floats, _, mode) ->
    Some (mode, regular ~scannable:false)
  | Make_block (Mixed (_, shape), _, mode) ->
    Some
      ( mode,
        { num_words = Flambda_kind.Mixed_block_shape.size_in_words shape;
          num_scannable_fields =
            Flambda_kind.Mixed_block_shape.value_prefix_size shape;
          num_alloc_args = 3
        } )
  | Make_array ((Unboxed_product _ as kind), _, mode) ->
    if Flambda_primitive.Array_kind.must_be_gc_scannable kind
    then Some (mode, regular ~scannable:true)
    else
      let element_kinds = Flambda_primitive.Array_kind.element_kinds kind in
      let words_per_element =
        List.fold_left
          (fun words kind ->
            let width =
              match Flambda_kind.With_subkind.kind kind with
              | Naked_number Naked_vec128 -> 2
              | Naked_number Naked_vec256 -> 4
              | Naked_number Naked_vec512 -> 8
              | Value | Naked_number _ | Region | Rec_info -> 1
            in
            Int.add words width)
          0 element_kinds
      in
      Some
        ( mode,
          { num_words =
              Int.mul (num_fields / List.length element_kinds) words_per_element;
            num_scannable_fields = 0;
            num_alloc_args = 3
          } )
  | Make_array _ | Begin_region _ | Begin_try_region _ -> None

let is_major_allocation (mode : Alloc_mode.For_allocations.t) layout =
  match mode with
  | Heap _ -> layout.num_words > Config.max_young_wosize
  | Local _ -> false

let major_allocation_size layout args =
  let initialize_field arg =
    (* Field address, argument move and [caml_initialize]. Noalloc calls also
       switch stacks on arm64 and on x86-64 with stack checks enabled. *)
    calls
      (per_arch ~x86_64:(if Config.no_stack_checks then 2 else 5) ~arm64:6
      + move_size ~for_call:true arg)
  in
  let _, fields =
    List.fold_left
      (fun (index, size) arg ->
        let field =
          if index < layout.num_scannable_fields
          then initialize_field arg
          else field_store_size arg
        in
        Int.succ index, size + field)
      (0, zero) args
  in
  c_call_size + both layout.num_alloc_args + fields

let variadic_prim_size prim args =
  let field_stores args =
    List.fold_left (fun size arg -> size + field_store_size arg) zero args
  in
  match generic_allocation_layout prim ~num_fields:(List.length args) with
  | Some (mode, layout) when is_major_allocation mode layout ->
    major_allocation_size layout args
  | Some _ | None -> (
    match (prim : Flambda_primitive.variadic_primitive) with
    (* A load from the domain state. *)
    | Begin_region { ghost } -> if ghost then zero else both 1
    | Begin_try_region { ghost } -> if ghost then zero else both 1
    (* Allocation plus one store per field. *)
    | Make_block (_, _mut, alloc_mode) ->
      alloc_size_for_mode alloc_mode + field_stores args
    | Make_array (kind, _mut, alloc_mode) -> (
      let num_elements = List.length args in
      let alloc_size = alloc_size_for_mode alloc_mode in
      match kind with
      | Immediates | Values | Gc_ignorable_values | Naked_floats | Naked_ints
      | Naked_int64s | Naked_nativeints | Naked_vec128s | Naked_vec256s
      | Naked_vec512s | Naked_masks | Unboxed_product _ ->
        alloc_size + field_stores args
      (* Packed arrays: elements must be combined into words first with [and],
         [shl] and [or] (see [Cmm_helpers.pack_small_ints_into_word]), or
         [unpcklps] / [zip1] for float32 pairs. *)
      | Naked_int8s | Naked_int16s -> alloc_size + both (Int.mul 3 num_elements)
      | Naked_int32s -> alloc_size + both (Int.mul 2 num_elements)
      | Naked_float32s -> alloc_size + both num_elements))

(* The kind of the allocation performed by a primitive, if any. *)
let prim_allocation (prim : Flambda_primitive.t) =
  match prim with
  | Unary (Box_number (_, mode), _) ->
    if !Clflags.jsir then None else Some (alloc_kind_of_mode mode)
  | Unary (Make_lazy _, _)
  | Binary (Bigarray_load (_, (Complex32 | Complex64), _), _, _) ->
    Some Heap_alloc
  | Variadic
      ( ((Make_block (_, _, mode) | Make_array (_, _, mode)) as prim),
        (_ :: _ as args) ) -> (
    match generic_allocation_layout prim ~num_fields:(List.length args) with
    | Some (mode, layout) when is_major_allocation mode layout -> None
    | Some _ | None -> Some (alloc_kind_of_mode mode))
  | Nullary _ | Unary _ | Binary _ | Ternary _ | Quaternary _ | Variadic _ ->
    None

(* Whether a primitive that does not allocate stops the combination of
   allocations around it: calls (including those of external functions, e.g.
   [caml_modify]) and polls, which are the primitives needing a frame, and the
   boundaries of regions. The latter only stop the combination of local
   allocations but are treated as stopping all of them. *)
let prim_is_barrier (prim : Flambda_primitive.t) size =
  size.needs_frame
  ||
  match prim with
  | Unary ((End_region { ghost } | End_try_region { ghost }), _)
  | Variadic ((Begin_region { ghost } | Begin_try_region { ghost }), _) ->
    not ghost
  | Nullary _ | Unary _ | Binary _ | Ternary _ | Quaternary _ | Variadic _ ->
    false

let prim ~machine_width:_ (prim : Flambda_primitive.t) =
  let size =
    match prim with
    | Nullary p -> nullary_prim_size p
    | Unary (p, _) -> unary_prim_size p
    | Binary (p, _, arg2) -> binary_prim_size p ~arg2
    | Ternary (p, _, _, arg3) -> ternary_prim_size p ~arg3
    | Quaternary (p, _, _, _, _) -> quaternary_prim_size p
    | Variadic (p, args) -> variadic_prim_size p args
  in
  match prim_allocation prim with
  | Some kind -> allocation kind size
  | None ->
    map size ~f:(fun size ->
        if prim_is_barrier prim size
        then barrier_size size
        else transparent_size size)

let box_number ~machine_width:_ kind =
  box_number0 ~alloc_size:heap_alloc_size kind

(* The allocation of a set of closures ([Cost_metrics.set_of_closures]), which
   needs [num_stores] stores including that of the header. Sets of closures are
   assumed to be allocated on the heap. *)
let set_of_closures_allocation ~num_stores =
  allocation Heap_alloc (heap_alloc_size + both (Int.sub num_stores 1))

(* These are used for the sizes of statically-allocated constants, which need no
   allocation at runtime; the numbers are kept comparable with those of dynamic
   allocations. *)
let block num_fields =
  map
    (heap_alloc_size + both num_fields)
    ~f:(fun size -> { size with needs_frame = false })

let array num_fields = block num_fields

let simple simple =
  (* A constant costs a [mov $imm] when it is not folded into the consuming
     instruction as an immediate operand (large constants need several [movk]
     instructions on arm64); a symbol needs its address loaded. *)
  Simple.pattern_match simple
    ~const:(fun _ -> both 1)
    ~name:(fun name ~coercion:_ ->
      Name.pattern_match name
        ~var:(fun _ -> zero)
        ~symbol:(fun _ -> symbol_address_size))

let static_consts _ = zero

let apply0 ~is_tail apply =
  let args = moves_size ~for_call:true (Apply_expr.args apply) in
  (* In tail position, calls to OCaml functions become jumps ([jmp] / [b], or
     [jmp *%reg] / [br] after loading the code pointer), with no frame needed
     and no spills. *)
  args
  +
  match Apply_expr.call_kind apply with
  | Function { function_call = Direct _; _ } ->
    if is_tail then both 1 else direct_call_size
  | Function { function_call = Indirect_unknown_arity } ->
    if is_tail then both 1 else indirect_call_size
  | Function { function_call = Indirect_known_arity _ } ->
    if is_tail then both 2 else indirect_call_size
  | C_call { is_c_builtin = true; _ } ->
    (* Builtins such as [sqrt], [clz] and [popcnt] are usually a single
       instruction (see [Cmm_builtins]), with a fallback to an external call
       when the backend does not support them. *)
    both 2
  | C_call { is_c_builtin = false; _ } -> c_call_size
  | Method { kind; obj = _ } -> (
    match kind with
    (* Load of the method table and of the method, then a generic application
       (see [Cmm_helpers.send]). *)
    | Self -> both 2 + indirect_call_size
    (* A call to [caml_get_public_method], then a generic application. *)
    | Public -> c_call_size + indirect_call_size
    (* A direct call to [caml_sendN], with the cache and position passed as
       extra arguments. *)
    | Cached -> direct_call_size + both 2)
  | Effect op -> (
    (* The effect operations are runtime functions called like OCaml functions
       (see [Cmm_helpers.perform] et al.). *)
    match op with
    (* Allocation of the two-field continuation block, then the call. *)
    | Perform _ -> heap_alloc_size + both 2 + direct_call_size
    | Reperform _ | Continue _ | Discontinue _ | Discontinue_with_backtrace _ ->
      if is_tail then both 1 else direct_call_size
    (* An external call to allocate the stack, then the call. *)
    | With_stack _ | With_stack_preemptible _ -> c_call_size + direct_call_size)

(* The code for a trap action, excluding the jump that follows it. *)
let trap_action_size (trap_action : Trap_action.t option) =
  match trap_action with
  | None -> zero
  (* [Lpushtrap] on x86-64: [lea handler(%rip), %r11; push %r11; push
     exn_handler(%r14); mov %rsp, exn_handler(%r14)]. arm64: [adr; stp; mov].
     Trap handlers also force the function to have a frame. *)
  | Some (Push _) -> calls (per_arch ~x86_64:4 ~arm64:3)
  (* [Lpoptrap]: [pop exn_handler(%r14); add $8, %rsp]. arm64: a single [ldr]
     with post-increment. *)
  | Some (Pop { raise_kind = None; _ }) -> per_arch ~x86_64:2 ~arm64:1
  (* Raising (see [Lraise]). With backtraces enabled ([-g]), a call to
     [caml_raise_exn] (preceded by a store on x86-64); otherwise
     [Raise_notrace]: [mov exn_handler(%r14), %rsp; pop exn_handler(%r14); pop
     %r11; jmp *%r11] (arm64: [mov sp, x26; ldp; br]), which is not a call. The
     jump is included in either case. *)
  | Some (Pop { raise_kind = Some _; _ }) ->
    if !Clflags.debug
    then calls (per_arch ~x86_64:2 ~arm64:1) - both 1
    else per_arch ~x86_64:4 ~arm64:3 - both 1

let apply_cont0 apply_cont =
  (* A jump (a [ret] for the return continuation), preceded by the moves of
     constant and symbol arguments. Jumps to non-inlined continuations are often
     eliminated by fallthrough or by the removal of empty blocks. *)
  both 1
  + moves_size ~for_call:false (Apply_cont_expr.args apply_cont)
  + trap_action_size (Apply_cont_expr.trap_action apply_cont)

(* An arm of a switch: the jump to the arm's destination is accounted for by the
   switch itself (as a conditional branch or a jump table entry) and the arm
   needs code of its own only when it has arguments to move (or a trap action),
   in which case it also needs its own jump afterwards. *)
let switch_arm_size action =
  let args = Apply_cont_expr.args action in
  let trap = trap_action_size (Apply_cont_expr.trap_action action) in
  match args with
  | [] -> trap
  | _ :: _ -> both 1 + moves_size ~for_call:false args + trap

let invalid = barrier zero

let switch0 switch =
  let arms = Switch_expr.arms switch in
  let num_arms = Target_ocaml_int.Map.cardinal arms in
  let arms_size =
    Target_ocaml_int.Map.fold
      (fun _ action size -> size + switch_arm_size action)
      arms zero
  in
  arms_size
  +
  if num_arms <= 2
  then
    (* Translated by [To_cmm_expr.switch] to an if-then-else. *)
    if_then_else_size
  else
    (* Translated using [Cmm_helpers.transl_switch_clambda], which uses the
       [Switch] module to choose between a jump table and a tree of comparisons.
       A jump table is used when the discriminants are dense enough; its entries
       cover the whole range of discriminants. The comparison tree needs a
       compare-and-branch per arm; the leaves of the tree are jumps, some of
       which fall through. *)
    let comparison_tree_size = both (Int.mul 2 num_arms) in
    let min_discriminant, _ = Target_ocaml_int.Map.min_binding arms in
    let max_discriminant, _ = Target_ocaml_int.Map.max_binding arms in
    match
      ( Target_ocaml_int.to_int_option min_discriminant,
        Target_ocaml_int.to_int_option max_discriminant )
    with
    | Some min_discriminant, Some max_discriminant ->
      let range = Int.add (Int.sub max_discriminant min_discriminant) 1 in
      (* [Switch.dense] requires at least three tests to be saved and the number
         of cases to be at least a third of the range. *)
      if range > 0 && num_arms >= 4 && range <= Int.mul 3 num_arms
      then jump_table_size + both range
      else comparison_tree_size
    | None, _ | _, None -> comparison_tree_size

(* Calls, jumps and branches all end basic blocks. *)
let apply ~is_tail apply = barrier (apply0 ~is_tail apply)

let apply_cont apply_cont = barrier (apply_cont0 apply_cont)

let switch switch = barrier (switch0 switch)

let evaluate ~args:_ t = float_of_int (target t)

(* Defined last so that the integer comparison operator remains available
   above. *)
let ( <= ) a b = Int.compare (target a) (target b) <= 0
