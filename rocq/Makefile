# Check the Rocq models and proofs, and regenerate the extracted OxCaml.
#
#   make          check every .v file, then run the extraction
#   make assumptions  print global assumptions and declaration types
#   make clean    remove everything generated
#
# The sources live in four directories, each a Rocq library:
#
#   rocq/                   OxCamlStdlib: models of OxCaml's own types (int,
#                           Int64, lists, exceptions as options), generic
#                           integer arithmetic, and their extraction
#                           directives; see HACKING.md there
#   backend/rocq/           OxCamlBackend: backend-wide definitions, such as
#                           the semantics of integer comparisons and the
#                           operations of the backend's instructions
#   backend/ssa/rocq/       OxCamlSsa: the SSA graph as the models see it
#                           (abstract blocks and values, with the few
#                           observations the passes make of them), and the
#                           properties of terminators, uses and dominance
#                           that any pass may need
#   backend/ssa/loop/rocq/  OxCamlLoop: the loop analyses and their
#                           extraction (Extract.v, one .ml per model); the
#                           pass using them is backend/ssa/loop/
#                           delete_empty_loops.ml
#
# The extracted files are run through ocamlformat so that they satisfy the
# repository's `make fmt` check like any other source under backend/ssa/.

ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
ROCQ ?= rocq
OCAMLFORMAT ?= ocamlformat

STDLIB_DIR := $(ROOT)/rocq
BACKEND_DIR := $(ROOT)/backend/rocq
SSA_DIR := $(ROOT)/backend/ssa/rocq
LOOP_DIR := $(ROOT)/backend/ssa/loop/rocq

FLAGS := -Q $(STDLIB_DIR) OxCamlStdlib -Q $(BACKEND_DIR) OxCamlBackend \
  -Q $(SSA_DIR) OxCamlSsa -Q $(LOOP_DIR) OxCamlLoop

STDLIB := $(addprefix $(STDLIB_DIR)/, \
  OxCamlLogic.v OxCamlOption.v ZTactics.v OxCamlInt.v OxCamlInt64.v \
  OxCamlList.v IntArith.v \
  OxCamlExtract.v IntArithExtract.v)
BACKEND := $(addprefix $(BACKEND_DIR)/, \
  IntegerComparison.v IntegerComparisonExtract.v Operation.v \
  OperationExtract.v)
SSA := $(addprefix $(SSA_DIR)/, \
  SsaModel.v SsaModelExtract.v SsaGraph.v SsaDominance.v SsaSemantics.v \
  SsaExecution.v SsaLiveness.v SsaProgress.v SsaContext.v)
LOOP := $(addprefix $(LOOP_DIR)/, \
  FourierMotzkin.v FourierMotzkinExtract.v LoopComparisons.v AffineExpr.v \
  AffineExprExtract.v NaturalLoop.v AffineSsa.v AffineSsaExtract.v \
  InductionVar.v InductionVarExtract.v LoopTermination.v DeadInductionVar.v \
  DeleteEmptyLoops.v LoopExecution.v LoopInvariantExecution.v \
  LoopCounterExecution.v LoopLiveOut.v LoopEligibility.v LoopValueSnapshot.v \
  LoopGuardExecution.v LoopRangeExecution.v)
EXTRACT := $(LOOP_DIR)/Extract.v

# The extraction writes one .ml and .mli per model into the loop directory;
# those of the SSA models are then moved next to their sources. Those of the
# rocq/ models that are not mapped onto OxCaml's own functions (OxCamlList,
# IntArith) stay in the loop directory.
SSA_MODULES := SsaModel SsaGraph SsaDominance SsaSemantics
SSA_EXTRACTED := $(addprefix $(SSA_DIR)/, \
  $(foreach m, $(SSA_MODULES), $(m).ml $(m).mli))
LOOP_EXTRACTED := $(addprefix $(LOOP_DIR)/, \
  OxCamlList.ml OxCamlList.mli IntArith.ml IntArith.mli \
  FourierMotzkin.ml FourierMotzkin.mli \
  LoopComparisons.ml LoopComparisons.mli AffineExpr.ml AffineExpr.mli \
  NaturalLoop.ml NaturalLoop.mli AffineSsa.ml AffineSsa.mli \
  InductionVar.ml InductionVar.mli LoopTermination.ml LoopTermination.mli \
  DeadInductionVar.ml DeadInductionVar.mli \
  DeleteEmptyLoops.ml DeleteEmptyLoops.mli)
EXTRACTED := $(SSA_EXTRACTED) $(LOOP_EXTRACTED)

SOURCES := $(STDLIB) $(BACKEND) $(SSA) $(LOOP) $(EXTRACT)
MODULES := $(patsubst $(STDLIB_DIR)/%.v,OxCamlStdlib.%,$(STDLIB)) \
  $(patsubst $(BACKEND_DIR)/%.v,OxCamlBackend.%,$(BACKEND)) \
  $(patsubst $(SSA_DIR)/%.v,OxCamlSsa.%,$(SSA)) \
  $(patsubst $(LOOP_DIR)/%.v,OxCamlLoop.%,$(LOOP) $(EXTRACT))
DEPS := $(STDLIB_DIR)/.rocqdeps.d

.PHONY: all assumptions clean

all: $(SOURCES:.v=.vo)

# The checker reports global assumptions, not theorem premises. Constant
# types also expose Section arguments and contract fields,
# including declarations inside nested modules.
assumptions: all
	@printf '%s\n' '=== Global assumptions (including dependencies) ==='
	@$(ROCQ) check -silent -o $(FLAGS) $(MODULES) 2>&1
	@printf '\n%s\n' '=== Declaration types and contract fields ===' \
	  'Section hypotheses appear as arguments in these types.' \
	  'This section includes proved theorems, not just assumptions.'
	@set -eu; \
	  audit_dir=$$(mktemp -d); \
	  trap 'rm -rf "$$audit_dir"' 0; \
	  { printf '%s\n' 'Set Printing Width 78.' \
	      'Set Printing Depth 1000000.'; \
	    for module in $(MODULES); do \
	      printf 'Require %s.\nPrint Namespace %s.\n' "$$module" "$$module"; \
	    done; \
	  } > "$$audit_dir/Assumptions.v"; \
	  $(ROCQ) repl -q -batch $(FLAGS) -l "$$audit_dir/Assumptions.v"

# Extract.v writes the .ml and .mli files into the loop directory. Separate
# extraction also writes an empty pair for every Rocq module whose contents
# are all mapped onto OxCaml's own (OxCamlInt, Uint63, ...); those are removed.
# It also puts the same `open M` lines at the top of a .ml and of its .mli;
# in the .mli only types can be referred to, so the open of a module that
# defines no types or submodules (OxCamlList) is unused there, which warning
# 33 rejects. Such opens are removed from the .mli files.
$(EXTRACT:.v=.vo): $(EXTRACT)
	$(ROCQ) compile $(FLAGS) -output-directory $(LOOP_DIR) $<
	for f in $(LOOP_DIR)/*.ml $(LOOP_DIR)/*.mli; do \
	  grep -q '[^[:space:]]' $$f || rm -f $$f; done
	mv $(foreach m, $(SSA_MODULES), $(LOOP_DIR)/$(m).ml $(LOOP_DIR)/$(m).mli) \
	  $(SSA_DIR)/
	@for f in $(LOOP_DIR)/*.ml $(LOOP_DIR)/*.mli; do \
	  case " $(LOOP_EXTRACTED) " in *" $$f "*) ;; *) \
	    echo "error: unexpected extracted file $$f: a model computes with" \
	      "something that has no OxCaml realization (see OxCamlExtract.v)"; \
	    exit 1;; \
	  esac; \
	done
	for f in $(LOOP_DIR)/*.mli $(SSA_DIR)/*.mli; do \
	  for m in `sed -n 's/^open \(.*\)$$/\1/p' $$f`; do \
	    cat $(LOOP_DIR)/$$m.mli $(SSA_DIR)/$$m.mli 2>/dev/null \
	      | grep -q -E '^ *(type|module) ' || \
	      { grep -v "^open $$m$$" $$f > $$f.tmp && mv $$f.tmp $$f; }; \
	  done; \
	done
	$(OCAMLFORMAT) -i $(EXTRACTED)

%.vo: %.v
	$(ROCQ) compile $(FLAGS) $<

# The dependencies between the .v files, computed by rocq dep.
$(DEPS): $(SOURCES)
	$(ROCQ) dep $(FLAGS) $(SOURCES) > $@

-include $(DEPS)

clean:
	rm -f $(DEPS) $(EXTRACTED)
	for d in $(STDLIB_DIR) $(BACKEND_DIR) $(SSA_DIR) $(LOOP_DIR); do \
	  rm -rf $$d/*.vo $$d/*.vos $$d/*.vok $$d/*.glob $$d/.*.aux \
	    $$d/.lia.cache $$d/.nia.cache $$d/.coq-native; \
	done
