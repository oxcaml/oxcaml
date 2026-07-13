[@@@ocaml.warning "+a-40-41-42"]

(** Pre-register-allocation block-terminator folding. This has some overlap with
    [Simplify_terminator], which can only track value flow after register
    allocation.

    Two related transformations driven by reaching definitions on virtual
    registers:

    - {b Intra-block fold}: when a block ends in [Truth_test r] and [r] was set
      by an [Icomp]/[Icompf] earlier in the block (without its inputs being
      overwritten in between), rewrite the [Truth_test] as the corresponding
      [Int_test]/[Float_test] so the comparison flows directly into the branch.
    - {b Jump threading through an empty merge block}: when a block ends in
      [Always B3] with [B3] an empty merge block ending in a test, evaluate
      [B3]'s test using definitions reaching from this block; if the test
      resolves to a constant or folds into a comparison, rewrite this block's
      terminator directly. Loop headers are never bypassed, so the CFG stays
      reducible. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t
