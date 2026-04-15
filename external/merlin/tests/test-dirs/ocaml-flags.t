Get a list of all flags that ocaml understands.
  $ OCAMLOPT="$MERLIN_TEST_OCAML_PATH/bin/ocamlopt.opt"
  $ "$OCAMLOPT" --help | grep -oP '(?<=  )-[a-z\-_0-9]+(?= )' > ocamlopt-flags.txt

  $ OCAMLC="$MERLIN_TEST_OCAML_PATH/bin/ocamlc"
  $ "$OCAMLC" --help | grep -oP '(?<=  )-[a-z\-_0-9]+(?= )' > ocamlc-flags.txt

  $ OCAML_FLAGS=$(sort -u ocamlopt-flags.txt ocamlc-flags.txt)

Verify that each flag appears in mconfig.ml. Each flag should either be handled or be in
the list of ignored flags. If a new flag appears in this list, you should either make
Merlin handle it or add it to the list of ignored flags.

  $ is_intentionally_unhandled () {
  >   # These flags are unhandled by Merlin and we really do want to raise an error if
  >   # they are passed.
  >   case "$1" in
  >     "-args" | "-args0" | "-depend")
  >       return 0
  >       ;;
  >     *)
  >       return 1
  >       ;;
  >   esac
  > }

  $ mconfig="../../src/kernel/mconfig.ml"
  > echo "$OCAML_FLAGS" | {
  >   has_unhandled_flags=0
  >   while IFS= read -r flag; do
  >     if ! grep -q "\"$flag\"" "$mconfig" && ! is_intentionally_unhandled "$flag"; then
  >       echo "Unhandled flag: $flag"
  >       has_unhandled_flags=1
  >     fi 
  >   done
  >   if [ "$has_unhandled_flags" -eq 1 ]; then
  >     cat <<EOF
  > A flag has been added to the compiler, and Merlin must known how to handle it. If the
  > flag is relevant to Merlin, Merlin should be updated to parse and use it. If not
  > (which is the usual case, especially for backend flags), Merlin needs to be told to
  > ignore the flag. Do this by adding it to either \`ocaml_ignored_flags\` or
  > \`ocaml_ignored_parametrized_flags\` in src/kernel/mconfig.ml, depending on whether
  > the flag takes a parameter.
  > EOF
  >   fi
  > }

This list is printed to ensure that a change to the help text does not break the parsing
logic.
  $ echo "$OCAML_FLAGS"
  --help
  --version
  -a
  -absname
  -afl-inst-ratio
  -afl-instrument
  -alert
  -alias-deps
  -annot
  -app-funct
  -args
  -args0
  -as-argument-for
  -as-parameter
  -basic-block-sections
  -bin-annot
  -bin-annot-cms
  -bin-annot-occurrences
  -c
  -cached-generic-functions-path
  -caml-apply-inline-fast-path
  -cc
  -cclib
  -ccopt
  -cfg-eliminate-dead-trap-handlers
  -cfg-merge-blocks
  -cfg-peephole-optimize
  -cfg-prologue-shrink-wrap
  -cfg-prologue-shrink-wrap-threshold
  -cfg-prologue-validate
  -cfg-stack-checks
  -cfg-stack-checks-threshold
  -cfg-value-propagation
  -cfg-value-propagation-float
  -cfg-value-propagation-flow
  -clambda-checks
  -cmi-file
  -color
  -compact
  -compat-32
  -config
  -config-var
  -custom
  -dasm-comments
  -davail
  -dblambda
  -dcamlprimc
  -dcfg
  -dcfg-invariants
  -dclambda
  -dcmm
  -dcmm-invariants
  -dcounters
  -dcse
  -ddebug-available-regs
  -ddebug-invariants
  -ddebug-uid-tables
  -ddebug-uids
  -ddissector
  -ddissector-inputs
  -ddissector-partitions
  -ddissector-sizes
  -ddissector-verbose
  -ddwarf-metrics
  -ddwarf-metrics-output-file
  -ddwarf-types
  -debug-long-frames-threshold
  -debug-ocaml
  -depend
  -dfexpr
  -dfexpr-after
  -dfexpr-annot
  -dfexpr-annot-after
  -dfexpr-to
  -dflambda
  -dflambda-heavy-invariants
  -dflambda-invariants
  -dflambda-let
  -dflambda-no-invariants
  -dflambda-verbose
  -dflexpect-to
  -dflow
  -dfreshen
  -dgc-timings
  -dgranularity
  -dinstr
  -directory
  -disable-all-extensions
  -disable-builtin-check
  -disable-poll-insertion
  -disable-precise-zero-alloc-checker
  -disable-zero-alloc-checker
  -dissector
  -dissector-assume-lld-without-64-bit-eh-frames
  -dissector-partition-size
  -dlambda
  -dletreclambda
  -dlinear
  -dllib
  -dllpath
  -dllvmir
  -dlocations
  -dno-asm-comments
  -dno-locations
  -dno-unique-ids
  -dparsetree
  -dprofile
  -dprofile-output
  -dranges
  -drawclambda
  -drawfexpr
  -drawfexpr-to
  -drawflambda
  -drawlambda
  -dreaper
  -dshape
  -dsimplify
  -dslambda
  -dslot-offsets
  -dsource
  -dstartup
  -dtimings
  -dtimings-precision
  -dtlambda
  -dtypedtree
  -dtypes
  -dump-dir
  -dump-inlining-paths
  -dump-into-csv
  -dump-into-file
  -dump-pass
  -dunique-ids
  -dvectorize
  -dzero-alloc
  -enable-poll-insertion
  -error-style
  -extension
  -extension-universe
  -favx
  -favx2
  -favx512f
  -fbmi
  -fbmi2
  -fclmul
  -ff16c
  -ffma
  -flambda2-advanced-meet
  -flambda2-backend-cse-at-toplevel
  -flambda2-basic-meet
  -flambda2-cse-depth
  -flambda2-debug
  -flambda2-debug-concrete-types-only-on-canonicals
  -flambda2-debug-keep-invalid-handlers
  -flambda2-expert-can-inline-recursive-functions
  -flambda2-expert-cmm-safe-subst
  -flambda2-expert-cont-lifting-budget
  -flambda2-expert-cont-specialization-budget
  -flambda2-expert-fallback-inlining-heuristic
  -flambda2-expert-inline-effects-in-cmm
  -flambda2-expert-max-block-size-for-projections
  -flambda2-expert-max-function-simplify-run
  -flambda2-expert-max-unboxing-depth
  -flambda2-expert-phantom-lets
  -flambda2-expert-shorten-symbol-names
  -flambda2-inline-alloc-cost
  -flambda2-inline-branch-cost
  -flambda2-inline-call-cost
  -flambda2-inline-indirect-cost
  -flambda2-inline-large-function-size
  -flambda2-inline-max-depth
  -flambda2-inline-max-rec-depth
  -flambda2-inline-poly-compare-cost
  -flambda2-inline-prim-cost
  -flambda2-inline-small-function-size
  -flambda2-inline-threshold
  -flambda2-inlining-report-bin
  -flambda2-join-algorithm
  -flambda2-join-depth
  -flambda2-join-points
  -flambda2-kind-checks
  -flambda2-reaper
  -flambda2-result-types-all-functions
  -flambda2-result-types-functors-only
  -flambda2-speculative-inlining-only-if-arguments-useful
  -flambda2-unbox-along-intra-function-control-flow
  -flambda2-unicode
  -flzcnt
  -fno-asan
  -fno-avx
  -fno-avx2
  -fno-avx512f
  -fno-bmi
  -fno-bmi2
  -fno-clmul
  -fno-f16c
  -fno-fma
  -fno-lzcnt
  -fno-popcnt
  -fno-prefetchw
  -fno-prefetchwt1
  -fno-sse3
  -fno-sse41
  -fno-sse42
  -fno-ssse3
  -fno-trap-notes
  -for-pack
  -fpopcnt
  -fprefetchw
  -fprefetchwt1
  -fsse3
  -fsse41
  -fsse42
  -fssse3
  -ftrap-notes
  -function-layout
  -function-sections
  -g
  -gdwarf-compression
  -gdwarf-config-max-cms-files-per-unit
  -gdwarf-config-max-cms-files-per-variable
  -gdwarf-config-max-evaluation-steps-per-variable
  -gdwarf-config-max-shape-reduce-steps-per-variable
  -gdwarf-config-max-type-to-shape-depth
  -gdwarf-config-shape-eval-depth
  -gdwarf-config-shape-reduce-depth
  -gdwarf-config-shape-reduce-fuel
  -gdwarf-fidelity
  -gdwarf-fission
  -gdwarf-inlined-frames
  -gdwarf-max-function-complexity
  -gdwarf-may-alter-codegen
  -gdwarf-may-alter-codegen-experimental
  -gdwarf-pedantic
  -gno-dwarf-inlined-frames
  -gno-dwarf-may-alter-codegen
  -gno-dwarf-may-alter-codegen-experimental
  -gno-startup
  -gno-upstream-dwarf
  -gstartup
  -gupstream-dwarf
  -heap-reduction-threshold
  -help
  -i
  -impl
  -inline
  -inline-alloc-cost
  -inline-branch-cost
  -inline-branch-factor
  -inline-call-cost
  -inline-indirect-cost
  -inline-lifting-benefit
  -inline-max-depth
  -inline-max-unroll
  -inline-prim-cost
  -inline-toplevel
  -inlining-report
  -insn-sched
  -instantiate
  -internal-assembler
  -intf
  -intf-suffix
  -intf_suffix
  -keep-docs
  -keep-llvmir
  -keep-locs
  -kind-verbosity
  -labels
  -linkall
  -linscan
  -llvm-backend
  -llvm-flags
  -llvm-path
  -locs
  -long-frames
  -make-runtime
  -make_runtime
  -manual-module-init
  -match-context-rows
  -modern
  -module-entry-functions-section
  -no-absname
  -no-alias-deps
  -no-app-funct
  -no-auto-include-otherlibs
  -no-cfg-eliminate-dead-trap-handlers
  -no-cfg-merge-blocks
  -no-cfg-peephole-optimize
  -no-cfg-prologue-shrink-wrap
  -no-cfg-prologue-validate
  -no-cfg-stack-checks
  -no-cfg-value-propagation
  -no-cfg-value-propagation-float
  -no-cfg-value-propagation-flow
  -no-check-prims
  -no-dissector-assume-lld-without-64-bit-eh-frames
  -no-extension
  -no-flambda2-backend-cse-at-toplevel
  -no-flambda2-debug
  -no-flambda2-debug-concrete-types-only-on-canonicals
  -no-flambda2-debug-keep-invalid-handlers
  -no-flambda2-expert-can-inline-recursive-functions
  -no-flambda2-expert-cmm-safe-subst
  -no-flambda2-expert-fallback-inlining-heuristic
  -no-flambda2-expert-inline-effects-in-cmm
  -no-flambda2-expert-phantom-lets
  -no-flambda2-expert-shorten-symbol-names
  -no-flambda2-join-points
  -no-flambda2-reaper
  -no-flambda2-result-types
  -no-flambda2-speculative-inlining-only-if-arguments-useful
  -no-flambda2-unbox-along-intra-function-control-flow
  -no-float-const-prop
  -no-g
  -no-insn-sched
  -no-keep-docs
  -no-keep-locs
  -no-locs
  -no-long-frames
  -no-mach-ir
  -no-manual-module-init
  -no-ocamlcfg
  -no-principal
  -no-probes
  -no-probes-optimized
  -no-reaper-change-calling-conventions
  -no-reaper-local-fields
  -no-reaper-unbox
  -no-rectypes
  -no-regalloc-validate
  -no-strict-formats
  -no-strict-sequence
  -no-symbol-visibility-protected
  -no-unbox-free-vars-of-closures
  -no-unbox-specialised-args
  -no-unboxed-types
  -no-vectorize
  -no-verbose-types
  -no-x86-peephole-combine-add-rsp
  -no-x86-peephole-optimize
  -no-x86-peephole-remove-mov-to-dead-register
  -no-x86-peephole-remove-redundant-cmp
  -no-zero-alloc-checker-details-extra
  -noassert
  -noautolink
  -nocwd
  -nodynlink
  -nolabels
  -nopervasives
  -nostdlib
  -o
  -ocamlcfg
  -ocamlrunparam
  -only-erasable-extensions
  -opaque
  -open
  -output-complete-exe
  -output-complete-obj
  -output-obj
  -p
  -pack
  -parameter
  -plugin
  -pp
  -ppx
  -principal
  -probes
  -probes-optimized
  -reaper-change-calling-conventions
  -reaper-debug-flags
  -reaper-local-fields
  -reaper-max-unbox-size
  -reaper-preserve-direct-calls
  -reaper-unbox
  -rectypes
  -regalloc
  -regalloc-linscan-threshold
  -regalloc-param
  -regalloc-validate
  -remove-unused-arguments
  -reorder-blocks-random
  -requires-metaprogramming
  -rounds
  -runtime-variant
  -safe-string
  -safer-matching
  -save-ir-after
  -save-ir-before
  -shape-format
  -shared
  -short-paths
  -stop-after
  -strict-formats
  -strict-sequence
  -symbol-visibility-protected
  -thread
  -unbox-closures
  -unbox-closures-factor
  -unboxed-types
  -unsafe
  -use-cached-generic-functions
  -use-prims
  -use-runtime
  -use_runtime
  -uses-metaprogramming
  -v
  -vectorize
  -vectorize-max-block-size
  -verbose
  -verbose-types
  -verify-binary-emitter
  -version
  -vmthread
  -vnum
  -w
  -warn-error
  -warn-help
  -where
  -with-runtime
  -without-runtime
  -x86-peephole-optimize
  -zero-alloc-assert
  -zero-alloc-check
  -zero-alloc-checker-details-cutoff
  -zero-alloc-checker-details-extra
  -zero-alloc-checker-join
