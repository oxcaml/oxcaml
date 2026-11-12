[@@@ocaml.warning "+a-40-41-42"]

module Datalog = Flambda2_datalog.Datalog

module Instruction : sig
  type t

  include Datalog.Column.S with type t := t

  val of_int : int -> t
end = struct
  include Datalog.Column.Make (struct
    let name = "cfg_instruction"

    let print fmt instruction = Format.fprintf fmt "i%d" instruction
  end)

  let of_int instruction = instruction
end

module Register : sig
  type t

  include Datalog.Column.S with type t := t

  val of_int : int -> t
end = struct
  include Datalog.Column.Make (struct
    let name = "cfg_register"

    let print fmt register = Format.fprintf fmt "r%d" register
  end)

  let of_int register = register
end

module Instruction_relation = Datalog.Schema.Relation1 (Instruction)
module Register_relation = Datalog.Schema.Relation1 (Register)
module Instruction_instruction_relation =
  Datalog.Schema.Relation2 (Instruction) (Instruction)
module Instruction_register_relation =
  Datalog.Schema.Relation2 (Instruction) (Register)

(* The base relations encode facts extracted from the CFG:

   - [next i j] and [exn_next i j] are control-flow edges.

   - [arg i r] and [res i r] record the registers read and written by an
   instruction.

   - [exn_bucket r] identifies the register defined when entering an exception
   handler (see [Proc.loc_exn_bucket]).

   - [tailcall_self i] marks the [Tailcall_self] terminator, across which normal
   successor liveness is not propagated.

   The derived relations describe liveness:

   - [not_removable i] is initially true for impure instructions and
   terminators, and becomes true for a pure instruction when one of its results
   is live.

   - [before i r] means [r] is live before [i].

   - [across i r] means [r] is live across [i]. *)
let create_relation name columns = Datalog.create_relation ~name columns

let next =
  create_relation "cfg_liveness.next" Instruction_instruction_relation.columns

let exn_next =
  create_relation "cfg_liveness.exn_next"
    Instruction_instruction_relation.columns

let arg =
  create_relation "cfg_liveness.arg" Instruction_register_relation.columns

let res =
  create_relation "cfg_liveness.res" Instruction_register_relation.columns

let exn_bucket =
  create_relation "cfg_liveness.exn_bucket" Register_relation.columns

let tailcall_self =
  create_relation "cfg_liveness.tailcall_self" Instruction_relation.columns

let not_removable =
  create_relation "cfg_liveness.not_removable" Instruction_relation.columns

let before =
  create_relation "cfg_liveness.before" Instruction_register_relation.columns

let across =
  create_relation "cfg_liveness.across" Instruction_register_relation.columns

let not_removable_rule =
  Datalog.compile ["instruction"; "register"; "successor"]
    (fun [instruction; register; successor] ->
      Datalog.where
        [ Datalog.atom res [instruction; register];
          Datalog.atom next [instruction; successor];
          Datalog.atom before [successor; register] ]
        (Datalog.deduce (Datalog.atom not_removable [instruction])))

let normal_across_rule =
  Datalog.compile ["instruction"; "successor"; "register"]
    (fun [instruction; successor; register] ->
      Datalog.where
        [ Datalog.atom next [instruction; successor];
          Datalog.atom before [successor; register];
          Datalog.not (Datalog.atom res [instruction; register]);
          Datalog.not (Datalog.atom tailcall_self [instruction]) ]
        (Datalog.deduce (Datalog.atom across [instruction; register])))

let exceptional_across_rule =
  Datalog.compile ["instruction"; "handler"; "register"]
    (fun [instruction; handler; register] ->
      Datalog.where
        [ Datalog.atom exn_next [instruction; handler];
          Datalog.atom before [handler; register];
          Datalog.not (Datalog.atom exn_bucket [register]) ]
        (Datalog.deduce (Datalog.atom across [instruction; register])))

let before_across_rule =
  Datalog.compile ["instruction"; "register"] (fun [instruction; register] ->
      Datalog.where
        [Datalog.atom across [instruction; register]]
        (Datalog.deduce (Datalog.atom before [instruction; register])))

let before_args_rule =
  Datalog.compile ["instruction"; "register"] (fun [instruction; register] ->
      Datalog.where
        [ Datalog.atom not_removable [instruction];
          Datalog.atom arg [instruction; register] ]
        (Datalog.deduce (Datalog.atom before [instruction; register])))

let schedule =
  Datalog.Schedule.saturate
    [ not_removable_rule;
      normal_across_rule;
      exceptional_across_rule;
      before_across_rule;
      before_args_rule ]

let relations_equal =
  Instruction.Map.equal (Register.Map.equal (fun () () -> true))

let print_relation =
  Instruction.Map.print (Register.Map.print (fun _fmt () -> ()))

let instruction instruction_id_gen id =
  Instruction.of_int
    (Cfg_z3.Instruction_id_gen.get_id_int instruction_id_gen ~key:id)

let register register_id_gen reg =
  Register.of_int (Cfg_z3.Reg_id_gen.get_id_int register_id_gen ~key:reg)

module Internal = struct
  let validate (facts : Cfg_validation_facts.Liveness.t) expected
      ~instruction_id_gen ~register_id_gen =
    let instruction = instruction instruction_id_gen in
    let register = register register_id_gen in
    let next_facts =
      List.fold_left
        (fun relation (id, successor) ->
          Instruction_instruction_relation.add_or_replace
            [instruction id; instruction successor]
            () relation)
        Instruction_instruction_relation.empty
        facts.Cfg_validation_facts.Liveness.next
    in
    let exn_next_facts =
      List.fold_left
        (fun relation (id, successor) ->
          Instruction_instruction_relation.add_or_replace
            [instruction id; instruction successor]
            () relation)
        Instruction_instruction_relation.empty facts.exn_next
    in
    let instruction_register_facts facts =
      List.fold_left
        (fun relation (id, reg) ->
          Instruction_register_relation.add_or_replace
            [instruction id; register reg]
            () relation)
        Instruction_register_relation.empty facts
    in
    let instruction_facts facts =
      List.fold_left
        (fun relation id ->
          Instruction_relation.add_or_replace [instruction id] () relation)
        Instruction_relation.empty facts
    in
    let db =
      Datalog.empty
      |> Datalog.set_table next next_facts
      |> Datalog.set_table exn_next exn_next_facts
      |> Datalog.set_table arg (instruction_register_facts facts.args)
      |> Datalog.set_table res (instruction_register_facts facts.results)
      |> Datalog.set_table exn_bucket
           (Register_relation.singleton [register facts.exn_bucket] ())
      |> Datalog.set_table tailcall_self (instruction_facts facts.tailcall_self)
      |> Datalog.set_table not_removable (instruction_facts facts.not_removable)
    in
    let db = Datalog.Schedule.run schedule db in
    let expected_relation get_regs =
      InstructionId.Tbl.fold
        (fun id domain relation ->
          Reg.Set.fold
            (fun reg relation ->
              Instruction_register_relation.add_or_replace
                [instruction id; register reg]
                () relation)
            (get_regs domain) relation)
        expected Instruction_register_relation.empty
    in
    let check relation_name relation expected =
      let actual = Datalog.get_table relation db in
      if relations_equal expected actual
      then Ok ()
      else
        Error
          (Format.asprintf "%s mismatch.@.Expected:@.%a@.Actual:@.%a"
             relation_name print_relation expected print_relation actual)
    in
    match
      check "before" before
        (expected_relation (fun (domain : Cfg_liveness.domain) -> domain.before))
    with
    | Error _ as error -> error
    | Ok () ->
      check "across" across
        (expected_relation (fun (domain : Cfg_liveness.domain) -> domain.across))
end

module Z3 = struct
  let code (facts : Cfg_validation_facts.Liveness.t) expected
      ~instruction_id_gen ~register_id_gen =
    let buffer = Buffer.create 4096 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt
      {|
(set-option :fp.engine datalog)

(define-sort instr () (_ BitVec %d))
(define-sort reg () (_ BitVec %d))

(declare-rel next (instr instr))
(declare-rel arg (instr reg))
(declare-rel res (instr reg))
(declare-rel exn-next (instr instr))
(declare-rel exn-bucket (reg))
(declare-rel expected-before (instr reg))
(declare-rel expected-across (instr reg))
(declare-rel tailcall-self (instr))
(declare-rel before (instr reg))
(declare-rel across (instr reg))
(declare-rel not-removable (instr))
(declare-rel bad ())
(declare-var i0 instr)
(declare-var i1 instr)
(declare-var r reg)
(rule (=> (and (res i0 r) (next i0 i1) (before i1 r)) (not-removable i0)))
(rule (=> (and (next i0 i1) (before i1 r)
               (not (res i0 r))
               (not (tailcall-self i0)))
          (across i0 r)))
(rule (=> (and (exn-next i0 i1) (before i1 r) (not (exn-bucket r))) (across i0 r)))
(rule (=> (across i0 r) (before i0 r)))
(rule (=> (and (not-removable i0) (arg i0 r)) (before i0 r)))
(rule (=> (and (not (expected-before i0 r)) (before i0 r)) bad))
(rule (=> (and (expected-before i0 r) (not (before i0 r))) bad))
(rule (=> (and (not (expected-across i0 r)) (across i0 r)) bad))
(rule (=> (and (expected-across i0 r) (not (across i0 r))) bad))
|}
      (Cfg_z3.Instruction_id_gen.width instruction_id_gen)
      (Cfg_z3.Reg_id_gen.width register_id_gen);
    let instruction id =
      Cfg_z3.Instruction_id_gen.get_id instruction_id_gen ~key:id
    in
    let register reg = Cfg_z3.Reg_id_gen.get_id register_id_gen ~key:reg in
    List.iter
      (fun id -> Cfg_z3.fmt_fact fmt "not-removable" [instruction id])
      facts.Cfg_validation_facts.Liveness.not_removable;
    List.iter
      (fun id -> Cfg_z3.fmt_fact fmt "tailcall-self" [instruction id])
      facts.tailcall_self;
    List.iter
      (fun (id, successor) ->
        Cfg_z3.fmt_fact fmt "next" [instruction id; instruction successor])
      facts.next;
    List.iter
      (fun (id, successor) ->
        Cfg_z3.fmt_fact fmt "exn-next" [instruction id; instruction successor])
      facts.exn_next;
    List.iter
      (fun (id, reg) ->
        Cfg_z3.fmt_fact fmt "arg" [instruction id; register reg])
      facts.args;
    List.iter
      (fun (id, reg) ->
        Cfg_z3.fmt_fact fmt "res" [instruction id; register reg])
      facts.results;
    Cfg_z3.fmt_fact fmt "exn-bucket" [register facts.exn_bucket];
    InstructionId.Tbl.iter
      (fun id (domain : Cfg_liveness.domain) ->
        Reg.Set.iter
          (fun reg ->
            Cfg_z3.fmt_fact fmt "expected-before" [instruction id; register reg])
          domain.before;
        Reg.Set.iter
          (fun reg ->
            Cfg_z3.fmt_fact fmt "expected-across" [instruction id; register reg])
          domain.across)
      expected;
    Format.pp_print_string fmt "(query bad)";
    Format.pp_print_flush fmt ();
    Buffer.contents buffer
end

let fallback cfg internal_failure z3_code =
  let z3_result = Cfg_z3.run_validation_fallback z3_code in
  Misc.fatal_errorf
    "validate_liveness: internal Datalog failed for CFG %S.@.%s@.%s@.Z3 \
     reproducer:@.%s"
    cfg.Cfg.fun_name internal_failure z3_result z3_code

let validate_liveness (cfg : Cfg.t) expected =
  let facts = Cfg_validation_facts.Liveness.create cfg in
  let instruction_id_gen = Cfg_z3.create_instruction_id_gen cfg in
  let register_id_gen = Cfg_z3.create_reg_id_gen cfg in
  let z3_code () =
    Z3.code facts expected ~instruction_id_gen ~register_id_gen
  in
  match
    Internal.validate facts expected ~instruction_id_gen ~register_id_gen
  with
  | Ok () -> ()
  | Error error -> fallback cfg error (z3_code ())
  | exception exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    let error =
      Format.sprintf "exception: %s@.%s" (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string backtrace)
    in
    fallback cfg error (z3_code ())
