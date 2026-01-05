module S = struct
  module type Domain_S = sig
    (* The domain is a join-semilattice with a lowest element. To ensure
       termination additionally all ascending chains have to be bounded. *)
    type t

    (** Identity element of the [join] operation. From definition this is also the
        lowest element in the domain. *)
    val bot : t

    (** Join operator of the join-semilattice. This operation has be associative,
        commutative and idempotent. *)
    val join : t -> t -> t

    (** Operator defined as ([less_equal x y] iff [equal (join x y) y]). Is
        separate from [join] for efficiency. *)
    val less_equal : t -> t -> bool
  end

  module Dataflow_result = struct
    type ('a, 'e) t =
      | Ok of 'a
      | Aborted of 'a * 'e
      | Max_iterations_reached
  end

  (* CR-someday gyorsh: Provide a way to get the values before or after an
     instruction, and record values for selected blocks and instructions
     only. *)

  (** [Block] is the abstract value before the analysis of the block. For forward
      analysis, this represents the program state at the start label of the block.  For
      backward analysis, it represents the state after the execution of the terminator.

      [Instr] is the abstract value after the analysis of the instruction. For forward
      analysis, this represents the state after the execution of instruction. For backward
      analysis, this represents the state before the execution of the instruction.


      [Body] is the abstract vlaue after the analyis of the body of a block.  For forward
      analysis, this represents the program state right before executing the terminator.
      For backward analysis, this represents the state at the start of the block. *)

  type (_, 'domain) map =
    | Block : ('domain Label.Tbl.t, 'domain) map
    | Instr : ('domain InstructionId.Tbl.t, 'domain) map
    | Both : ('domain InstructionId.Tbl.t * 'domain Label.Tbl.t, 'domain) map
    | Body : ('domain Label.Tbl.t, 'domain) map

  type 'a control =
    { normal : 'a;
      exceptional : 'a
    }

  module type S = sig
    type domain

    type error

    type context

    (** [init] specifies how to initialize the normal/exception entry points. The
        intepretation of entry points depends on the direction of the analysis.  *)
    type init = domain control

    (** Perform the dataflow analysis on the passed CFG, returning [OK _] if a fix-point has
        been reached or [Max_iterartions_reached] if there is still pending work after
        [max_iteration] have been executed or [Aborted _] otherwise.

        An iteration is the processing of one element from the working set.
        The default [max_iterations] is [max_int]).

        The nested result is a partial map from labels to the domain values at the start of
        the corresponding blocks or from instruction ids to the domain values before the
        instruction. The type of the result is determined by [map] argument.  If
        [Max_iterations_reached _] or [Abort _] is returned then the contents of the map is
        not guaranteed to be sound.

        The [init] value is the initial value of entry points. *)
    val run :
      Cfg.t ->
      ?max_iteration:int ->
      init:init ->
      map:('a, domain) map ->
      context ->
      ('a, error) Dataflow_result.t
  end

  module type Transfer_S = sig
    type domain

    type input

    type output

    type error

    type context

    val basic :
      domain -> Cfg.basic Cfg.instruction -> context -> (domain, error) result

    val terminator :
      input ->
      Cfg.terminator Cfg.instruction ->
      context ->
      (output, error) result

    val exception_ : domain -> context -> (domain, error) result
  end

  module type Forward_transfer = sig
    type d

    include
      Transfer_S
        with type domain = d
         and type input = d
         and type output = d control
  end

  module type Backward_transfer = sig
    type d

    include
      Transfer_S
        with type domain = d
         and type input = d control
         and type output = d
  end
end
