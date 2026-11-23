
; The operations on pseudo-instrumentation counters ([Fdo_counter]).

(executable (name fdo_counters) (modules fdo_counters) (libraries ocamlcommon))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to fdo_counters.output.corrected (run ./fdo_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps fdo_counters.output fdo_counters.output.corrected)
 (action (diff fdo_counters.output fdo_counters.output.corrected)))

; The counters of the cases of the switches compiled to Cmm.

(executable
 (name cmm_switch_counters)
 (modules cmm_switch_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   cmm_switch_counters.output.corrected
   (run ./cmm_switch_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps cmm_switch_counters.output cmm_switch_counters.output.corrected)
 (action (diff cmm_switch_counters.output cmm_switch_counters.output.corrected)))

; The counters that instruction selection puts in the Cfg.

(executable
 (name selection_counters)
 (modules selection_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   selection_counters.output.corrected
   (run ./selection_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps selection_counters.output selection_counters.output.corrected)
 (action (diff selection_counters.output selection_counters.output.corrected)))

; The counters of the terminators that [Simplify_terminator] rewrites.

(executable
 (name simplify_terminator_counters)
 (modules simplify_terminator_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   simplify_terminator_counters.output.corrected
   (run ./simplify_terminator_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps
  simplify_terminator_counters.output
  simplify_terminator_counters.output.corrected)
 (action
  (diff
   simplify_terminator_counters.output
   simplify_terminator_counters.output.corrected)))

; The counters through the passes of the SSA pipeline.

(executable (name ssa_counters) (modules ssa_counters) (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to ssa_counters.output.corrected (run ./ssa_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps ssa_counters.output ssa_counters.output.corrected)
 (action (diff ssa_counters.output ssa_counters.output.corrected)))

; The counters on the machine edges of the terminators that [Cfg_to_linear]
; linearizes.

(executable
 (name cfg_to_linear_counters)
 (modules cfg_to_linear_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   cfg_to_linear_counters.output.corrected
   (run ./cfg_to_linear_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps cfg_to_linear_counters.output cfg_to_linear_counters.output.corrected)
 (action
  (diff cfg_to_linear_counters.output cfg_to_linear_counters.output.corrected)))

; The counters of the conditional branches that [Branch_relaxation] relaxes.

(executable
 (name branch_relaxation_counters)
 (modules branch_relaxation_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   branch_relaxation_counters.output.corrected
   (run ./branch_relaxation_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps
  branch_relaxation_counters.output
  branch_relaxation_counters.output.corrected)
 (action
  (diff
   branch_relaxation_counters.output
   branch_relaxation_counters.output.corrected)))

; The counters of the blocks that [Cfg_merge_blocks] merges.

(executable
 (name merge_blocks_counters)
 (modules merge_blocks_counters)
 (libraries ocamloptcomp))

(rule
 (enabled_if (= %{context_name} "main"))
 (action
  (with-stdout-to
   merge_blocks_counters.output.corrected
   (run ./merge_blocks_counters.exe))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps merge_blocks_counters.output merge_blocks_counters.output.corrected)
 (action
  (diff merge_blocks_counters.output merge_blocks_counters.output.corrected)))

; Extracting the FDO annotations from dumps.

(executable (name fdo_annotations) (modules fdo_annotations))

; Function identities ([Fdo_function_naming]). function_ids_edited.ml is
; function_ids.ml with anonymous functions inserted (other functions keep their
; names), an optional argument added to [opt] (it keeps its name, and gains a
; stub), and the second [shadowing] removed. In both, shadowed functions and
; anonymous functions with the same body get distinct discriminators, and
; functions without a location (as in classes) are named by their binders.

(rule
 (targets function_ids.output.corrected)
 (deps function_ids.ml function_ids_edited.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run
    %{bin:ocamlopt.opt}
    -c
    -fdo-counters
    -fexpr-print-fdo-counters
    -dfexpr-annot-after
    raw
    -color
    never
    function_ids.ml)
   (run
    %{bin:ocamlopt.opt}
    -c
    -fdo-counters
    -fexpr-print-fdo-counters
    -dfexpr-annot-after
    raw
    -color
    never
    -o
    function_ids.edited.cmx
    function_ids_edited.ml)
   (with-stdout-to
    function_ids.output.corrected
    (run
     ./fdo_annotations.exe
     compare-entries
     function_ids.raw.fl
     function_ids.edited.raw.fl)))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps function_ids.output function_ids.output.corrected)
 (action (diff function_ids.output function_ids.output.corrected)))

; Body hashes ([Fdo_body_hash]). body_hash_edited.ml changes the [keep_*]
; functions in ways that must keep their hashes, and the [change_*] functions in
; ways that must change them. Functions with the same hash in both must have the
; same counters.

(rule
 (targets body_hash.output.corrected)
 (deps body_hash.ml body_hash_edited.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run
    %{bin:ocamlopt.opt}
    -c
    -fdo-counters
    -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations
    -dfexpr-annot-after
    raw
    -color
    never
    body_hash.ml)
   (run
    %{bin:ocamlopt.opt}
    -c
    -fdo-counters
    -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations
    -dfexpr-annot-after
    raw
    -color
    never
    -o
    body_hash.edited.cmx
    body_hash_edited.ml)
   (with-stdout-to
    body_hash.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      compare-entries
      body_hash.raw.fl
      body_hash.edited.raw.fl)
     (run
      ./fdo_annotations.exe
      compare-counters
      body_hash.raw.fl
      body_hash.edited.raw.fl))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps body_hash.output body_hash.output.corrected)
 (action (diff body_hash.output body_hash.output.corrected)))

; The counters attached by [Fdo_instrumentation] during the translation from
; Lambda: edges of switches, interior and module-level calls, in nested
; functions, functors and local modules.

(rule
 (targets instrumentation.output.corrected)
 (deps instrumentation.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run
    %{bin:ocamlopt.opt}
    -c
    -fdo-counters
    -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations
    -dflambda-heavy-invariants
    -dfexpr-annot-after
    raw
    -color
    never
    instrumentation.ml)
   (with-stdout-to
    instrumentation.output.corrected
    (run ./fdo_annotations.exe list instrumentation.raw.fl)))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps instrumentation.output instrumentation.output.corrected)
 (action (diff instrumentation.output instrumentation.output.corrected)))

; The counters after Simplify: inlined bodies get their call site as inlining
; context, the functions of inlined functor applications are copies with their
; own counters, and the counters of removed instructions (inlined calls, folded
; switches, self tail calls) are dropped. Compilation stops after the middle
; end, so that only its dropped counters are listed.

(rule
 (targets simplify.output.corrected)
 (deps simplify.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dflambda-heavy-invariants
     -dfexpr-annot-after
     simplify
     -dfdo-dropped-counters
     -dump-into-file
     -stop-after
     middle_end
     -color
     never
     simplify.ml))
   (with-stdout-to
    simplify.output.corrected
    (progn
     (run ./fdo_annotations.exe list simplify.simplify.fl)
     (run cat simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps simplify.output simplify.output.corrected)
 (action (diff simplify.output simplify.output.corrected)))

; The same at -O3 with the reaper, after the reaper (in a directory of its own,
; so that the module keeps its name). Without local value slots, which CI may
; enable by default and which remove the code of [R.step].

(rule
 (targets simplify_o3_reaper.output.corrected)
 (deps simplify.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p o3_reaper)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -flambda2-reaper
     -no-reaper-local-fields
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dflambda-heavy-invariants
     -dfexpr-annot-after
     reaper
     -dfdo-dropped-counters
     -dump-into-file
     -stop-after
     middle_end
     -color
     never
     -o
     o3_reaper/simplify.cmx
     simplify.ml))
   (with-stdout-to
    simplify_o3_reaper.output.corrected
    (progn
     (run ./fdo_annotations.exe list o3_reaper/simplify.reaper.fl)
     (run cat o3_reaper/simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps simplify_o3_reaper.output simplify_o3_reaper.output.corrected)
 (action (diff simplify_o3_reaper.output simplify_o3_reaper.output.corrected)))

; The counters in Cmm: none of the Flambda translated to Cmm is lost, and none
; is added. [to_cmm.ml] is also compiled with the inline fast path of
; [caml_apply], whose two calls share the counter, and [simplify.ml] at -O3 with
; the reaper.

(rule
 (targets to_cmm.output.corrected)
 (deps to_cmm.ml simplify.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p to_cmm_fast_path to_cmm_o3_reaper)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dfexpr-annot-after
     simplify
     -dcmm
     -dump-into-file
     -color
     never
     -o
     to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -caml-apply-inline-fast-path
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dfexpr-annot-after
     simplify
     -dcmm
     -dump-into-file
     -color
     never
     -o
     to_cmm_fast_path/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -flambda2-reaper
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dfexpr-annot-after
     reaper
     -dcmm
     -dump-into-file
     -color
     never
     -o
     to_cmm_o3_reaper/simplify.cmx
     simplify.ml))
   (with-stdout-to
    to_cmm.output.corrected
    (progn
     (run ./fdo_annotations.exe compare-sets to_cmm.simplify.fl to_cmm.cmx.dump)
     (run ./fdo_annotations.exe list to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      to_cmm_fast_path/to_cmm.simplify.fl
      to_cmm_fast_path/to_cmm.cmx.dump)
     (run ./fdo_annotations.exe list to_cmm_fast_path/to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      to_cmm_o3_reaper/simplify.reaper.fl
      to_cmm_o3_reaper/simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps to_cmm.output to_cmm.output.corrected)
 (action (diff to_cmm.output to_cmm.output.corrected)))

; The counters of the switches To_cmm turns into a table lookup or a single
; jump, which the reaper leaves for it: they are dropped.

(rule
 (targets to_cmm_reaper.output.corrected)
 (deps to_cmm_reaper.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -flambda2-reaper
     -fdo-counters
     -fexpr-print-fdo-counters
     -fexpr-print-branch-annotations
     -dflambda-heavy-invariants
     -dfexpr-annot-after
     reaper
     -dcmm
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     to_cmm_reaper.ml))
   (with-stdout-to
    to_cmm_reaper.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      compare-sets
      to_cmm_reaper.reaper.fl
      to_cmm_reaper.cmx.dump)
     (run ./fdo_annotations.exe list to_cmm_reaper.cmx.dump)
     (run ./fdo_annotations.exe dropped to_cmm_reaper.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps to_cmm_reaper.output to_cmm_reaper.output.corrected)
 (action (diff to_cmm_reaper.output to_cmm_reaper.output.corrected)))

; The counter of the call of a probe handler, which To_cmm replaces by the
; probe: it is dropped. Probes are only supported on Linux.

(rule
 (targets to_cmm_probe.output.corrected)
 (deps to_cmm_probe.ml fdo_annotations.exe)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     to_cmm_probe.ml))
   (with-stdout-to
    to_cmm_probe.output.corrected
    (progn
     (run ./fdo_annotations.exe list to_cmm_probe.cmx.dump)
     (run ./fdo_annotations.exe dropped to_cmm_probe.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (deps to_cmm_probe.output to_cmm_probe.output.corrected)
 (action (diff to_cmm_probe.output to_cmm_probe.output.corrected)))

; The counters in Cfg: none of the Cmm selected is lost, and none is added. A
; self tail call becomes a jump, whose counter is dropped. The Cfg dumps hold
; every pass that prints one.

(rule
 (targets cfg.output.corrected)
 (deps cfg.ml to_cmm.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p cfg_cmm cfg_cfg)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     cfg_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcfg
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     cfg_cfg/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     cfg_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcfg
     -dump-into-file
     -color
     never
     -o
     cfg_cfg/to_cmm.cmx
     to_cmm.ml))
   (with-stdout-to
    cfg.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      compare-sets
      cfg_cmm/cfg.cmx.dump
      cfg_cfg/cfg.cmx.dump)
     (run ./fdo_annotations.exe list cfg_cmm/cfg.cmx.dump)
     (run ./fdo_annotations.exe list cfg_cfg/cfg.cmx.dump)
     (run ./fdo_annotations.exe dropped cfg_cfg/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      cfg_cmm/to_cmm.cmx.dump
      cfg_cfg/to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps cfg.output cfg.output.corrected)
 (action (diff cfg.output cfg.output.corrected)))

; The counters through SSA: those of the Cmm are in the SSA and in the Cfg
; converted from it, except that of a self tail call, which becomes a jump.
; [-use-ssa] also checks, by [-ssa-validate], that the Cfg converted from SSA
; has the counters of the one selected from Cmm, on the same edges and calls.
; The SSA dumps hold the SSA before and after [Ssa_simplify].

(rule
 (targets ssa.output.corrected)
 (deps cfg.ml to_cmm.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p ssa_cmm ssa_ssa ssa_cfg)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     ssa_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dssa
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     ssa_ssa/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dcfg
     -dump-into-file
     -color
     never
     -o
     ssa_cfg/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     ssa_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dssa
     -dump-into-file
     -color
     never
     -o
     ssa_ssa/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -use-ssa
     -fdo-counters
     -dcfg
     -dump-into-file
     -color
     never
     -o
     ssa_cfg/to_cmm.cmx
     to_cmm.ml))
   (with-stdout-to
    ssa.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      compare-sets
      ssa_cmm/cfg.cmx.dump
      ssa_ssa/cfg.cmx.dump)
     (run ./fdo_annotations.exe list ssa_ssa/cfg.cmx.dump)
     (run ./fdo_annotations.exe dropped ssa_ssa/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      ssa_cmm/cfg.cmx.dump
      ssa_cfg/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      ssa_cmm/to_cmm.cmx.dump
      ssa_ssa/to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      ssa_cmm/to_cmm.cmx.dump
      ssa_cfg/to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps ssa.output ssa.output.corrected)
 (action (diff ssa.output ssa.output.corrected)))

; The counters in Linear: none of the Cmm is lost through the Cfg passes apart
; from the dropped ones, and none is added. A conditional branch has the
; counters of its taken edge and of its fallthrough edge.

(rule
 (targets linear.output.corrected)
 (deps linear.ml cfg.ml to_cmm.ml fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p linear_cmm linear_linear)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     linear_cmm/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dlinear
     -dump-into-file
     -color
     never
     -o
     linear_linear/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     linear_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dlinear
     -dump-into-file
     -color
     never
     -o
     linear_linear/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     linear_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -fdo-counters
     -dlinear
     -dump-into-file
     -color
     never
     -o
     linear_linear/to_cmm.cmx
     to_cmm.ml))
   (with-stdout-to
    linear.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      compare-sets
      linear_cmm/linear.cmx.dump
      linear_linear/linear.cmx.dump)
     (run ./fdo_annotations.exe list linear_linear/linear.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      linear_cmm/cfg.cmx.dump
      linear_linear/cfg.cmx.dump)
     (run ./fdo_annotations.exe list linear_linear/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      compare-sets
      linear_cmm/to_cmm.cmx.dump
      linear_linear/to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps linear.output linear.output.corrected)
 (action (diff linear.output linear.output.corrected)))

; End to end through the backend, at -O3, with the SSA pipeline and with the
; merging of identical blocks: each counter of the Cmm is in the Linear code or
; dropped, and the Linear code has no other counter.

(rule
 (targets backend_o3.output.corrected)
 (deps
  linear.ml
  cfg.ml
  to_cmm.ml
  to_cmm_reaper.ml
  simplify.ml
  fdo_annotations.exe)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run
    mkdir
    -p
    backend_o3_cmm
    backend_o3_linear
    backend_o3_ssa_cmm
    backend_o3_ssa_linear
    backend_o3_merge_blocks_cmm
    backend_o3_merge_blocks_linear)
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_cmm/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_linear/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_linear/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_linear/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_cmm/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_linear/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_cmm/simplify.cmx
     simplify.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_linear/simplify.cmx
     simplify.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_cmm/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_linear/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_linear/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_linear/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_cmm/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_linear/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_cmm/simplify.cmx
     simplify.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -use-ssa
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_ssa_linear/simplify.cmx
     simplify.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_cmm/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_linear/linear.cmx
     linear.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_cmm/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_linear/cfg.cmx
     cfg.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_cmm/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_linear/to_cmm.cmx
     to_cmm.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_cmm/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_linear/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dcmm
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_cmm/simplify.cmx
     simplify.ml))
   (setenv
    OCAMLPARAM
    ""
    (run
     %{bin:ocamlopt.opt}
     -c
     -O3
     -cfg-merge-blocks
     -fdo-counters
     -dlinear
     -dfdo-dropped-counters
     -dump-into-file
     -color
     never
     -o
     backend_o3_merge_blocks_linear/simplify.cmx
     simplify.ml))
   (with-stdout-to
    backend_o3.output.corrected
    (progn
     (run
      ./fdo_annotations.exe
      check
      backend_o3_cmm/linear.cmx.dump
      backend_o3_linear/linear.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_cmm/cfg.cmx.dump
      backend_o3_linear/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_cmm/to_cmm.cmx.dump
      backend_o3_linear/to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_cmm/to_cmm_reaper.cmx.dump
      backend_o3_linear/to_cmm_reaper.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_cmm/simplify.cmx.dump
      backend_o3_linear/simplify.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_ssa_cmm/linear.cmx.dump
      backend_o3_ssa_linear/linear.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_ssa_cmm/cfg.cmx.dump
      backend_o3_ssa_linear/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_ssa_cmm/to_cmm.cmx.dump
      backend_o3_ssa_linear/to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_ssa_cmm/to_cmm_reaper.cmx.dump
      backend_o3_ssa_linear/to_cmm_reaper.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_ssa_cmm/simplify.cmx.dump
      backend_o3_ssa_linear/simplify.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_merge_blocks_cmm/linear.cmx.dump
      backend_o3_merge_blocks_linear/linear.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_merge_blocks_cmm/cfg.cmx.dump
      backend_o3_merge_blocks_linear/cfg.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_merge_blocks_cmm/to_cmm.cmx.dump
      backend_o3_merge_blocks_linear/to_cmm.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_merge_blocks_cmm/to_cmm_reaper.cmx.dump
      backend_o3_merge_blocks_linear/to_cmm_reaper.cmx.dump)
     (run
      ./fdo_annotations.exe
      check
      backend_o3_merge_blocks_cmm/simplify.cmx.dump
      backend_o3_merge_blocks_linear/simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps backend_o3.output backend_o3.output.corrected)
 (action (diff backend_o3.output backend_o3.output.corrected)))
