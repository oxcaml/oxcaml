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
 (deps simplify_terminator_counters.output
  simplify_terminator_counters.output.corrected)
 (action
  (diff simplify_terminator_counters.output
   simplify_terminator_counters.output.corrected)))

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
 (deps branch_relaxation_counters.output
  branch_relaxation_counters.output.corrected)
 (action
  (diff branch_relaxation_counters.output
   branch_relaxation_counters.output.corrected)))

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

(rule
 (targets function_ids.output.corrected)
 (deps function_ids.ml function_ids_edited.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations -dflambda-heavy-invariants
    -dfexpr-annot-after raw -color never function_ids.ml)
   (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations -dflambda-heavy-invariants
    -dfexpr-annot-after raw -color never -o function_ids.edited.cmx
    function_ids_edited.ml)
   (with-stdout-to
    function_ids.output.corrected
    (run %{exe:tools/fdo_annotations.exe} compare-entries function_ids.raw.fl
     function_ids.edited.raw.fl)))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps function_ids.output function_ids.output.corrected)
 (action (diff function_ids.output function_ids.output.corrected)))

(rule
 (targets body_hash.output.corrected)
 (deps body_hash.ml body_hash_edited.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations -dflambda-heavy-invariants
    -dfexpr-annot-after raw -color never body_hash.ml)
   (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations -dflambda-heavy-invariants
    -dfexpr-annot-after raw -color never -o body_hash.edited.cmx
    body_hash_edited.ml)
   (with-stdout-to
    body_hash.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-entries body_hash.raw.fl
      body_hash.edited.raw.fl)
     (run %{exe:tools/fdo_annotations.exe} compare-counters body_hash.raw.fl
      body_hash.edited.raw.fl))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps body_hash.output body_hash.output.corrected)
 (action (diff body_hash.output body_hash.output.corrected)))

(rule
 (targets instrumentation.output.corrected)
 (deps instrumentation.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
    -fexpr-print-branch-annotations -dflambda-heavy-invariants
    -dfexpr-annot-after raw -color never instrumentation.ml)
   (with-stdout-to
    instrumentation.output.corrected
    (run %{exe:tools/fdo_annotations.exe} list instrumentation.raw.fl)))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps instrumentation.output instrumentation.output.corrected)
 (action (diff instrumentation.output instrumentation.output.corrected)))

(rule
 (targets simplify.output.corrected)
 (deps simplify.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     simplify.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
      -fexpr-print-branch-annotations -dflambda-heavy-invariants
      -dfexpr-annot-after simplify -dfdo-dropped-counters -stop-after middle_end
      -color never simplify.ml)))
   (with-stdout-to
    simplify.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} list simplify.simplify.fl)
     (run cat simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps simplify.output simplify.output.corrected)
 (action (diff simplify.output simplify.output.corrected)))

(rule
 (targets simplify_o3_reaper.output.corrected)
 (deps simplify.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p o3_reaper)
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     o3_reaper.simplify.cmx.dump
     (run %{bin:ocamlopt.opt} -c -O3 -flambda2-reaper -no-reaper-local-fields
      -fdo-counters -fexpr-print-fdo-counters -fexpr-print-branch-annotations
      -dflambda-heavy-invariants -dfexpr-annot-after reaper
      -dfdo-dropped-counters -stop-after middle_end -color never -o
      o3_reaper/simplify.cmx simplify.ml)))
   (with-stdout-to
    simplify_o3_reaper.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} list o3_reaper/simplify.reaper.fl)
     (run cat o3_reaper.simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps simplify_o3_reaper.output simplify_o3_reaper.output.corrected)
 (action (diff simplify_o3_reaper.output simplify_o3_reaper.output.corrected)))

(rule
 (targets to_cmm.output.corrected)
 (deps to_cmm.ml simplify.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p to_cmm_fast_path to_cmm_o3_reaper)
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -fexpr-print-fdo-counters
      -fexpr-print-branch-annotations -dflambda-heavy-invariants
      -dfexpr-annot-after simplify -dcmm -color never -o to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     to_cmm_fast_path.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -caml-apply-inline-fast-path -fdo-counters
      -fexpr-print-fdo-counters -fexpr-print-branch-annotations
      -dflambda-heavy-invariants -dfexpr-annot-after simplify -dcmm -color never
      -o to_cmm_fast_path/to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     to_cmm_o3_reaper.simplify.cmx.dump
     (run %{bin:ocamlopt.opt} -c -O3 -flambda2-reaper -fdo-counters
      -fexpr-print-fdo-counters -fexpr-print-branch-annotations
      -dflambda-heavy-invariants -dfexpr-annot-after reaper -dcmm -color never
      -o to_cmm_o3_reaper/simplify.cmx simplify.ml)))
   (with-stdout-to
    to_cmm.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-sets to_cmm.simplify.fl
      to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets
      to_cmm_fast_path/to_cmm.simplify.fl to_cmm_fast_path.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list
      to_cmm_fast_path.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets
      to_cmm_o3_reaper/simplify.reaper.fl to_cmm_o3_reaper.simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps to_cmm.output to_cmm.output.corrected)
 (action (diff to_cmm.output to_cmm.output.corrected)))

(rule
 (targets to_cmm_reaper.output.corrected)
 (deps to_cmm_reaper.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     to_cmm_reaper.cmx.dump
     (run %{bin:ocamlopt.opt} -c -O3 -flambda2-reaper -fdo-counters
      -fexpr-print-fdo-counters -fexpr-print-branch-annotations
      -dflambda-heavy-invariants -dfexpr-annot-after reaper -dcmm
      -dfdo-dropped-counters -color never to_cmm_reaper.ml)))
   (with-stdout-to
    to_cmm_reaper.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-sets to_cmm_reaper.reaper.fl
      to_cmm_reaper.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list to_cmm_reaper.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} dropped to_cmm_reaper.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps to_cmm_reaper.output to_cmm_reaper.output.corrected)
 (action (diff to_cmm_reaper.output to_cmm_reaper.output.corrected)))

(rule
 (targets to_cmm_probe.output.corrected)
 (deps to_cmm_probe.ml)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{system} "linux")
   (= %{architecture} "amd64")))
 (action
  (progn
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     to_cmm_probe.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -dfdo-dropped-counters
      -color never to_cmm_probe.ml)))
   (with-stdout-to
    to_cmm_probe.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} list to_cmm_probe.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} dropped to_cmm_probe.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if
  (and
   (= %{context_name} "main")
   (= %{system} "linux")
   (= %{architecture} "amd64")))
 (deps to_cmm_probe.output to_cmm_probe.output.corrected)
 (action (diff to_cmm_probe.output to_cmm_probe.output.corrected)))

(rule
 (targets cfg.output.corrected)
 (deps cfg.ml to_cmm.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p cfg_cmm cfg_cfg)
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     cfg_cmm.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -color never -o
      cfg_cmm/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     cfg_cfg.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcfg -dfdo-dropped-counters
      -color never -o cfg_cfg/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     cfg_cmm.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -color never -o
      cfg_cmm/to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     cfg_cfg.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcfg -dfdo-dropped-counters
      -color never -o cfg_cfg/to_cmm.cmx to_cmm.ml)))
   (with-stdout-to
    cfg.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-sets cfg_cmm.cfg.cmx.dump
      cfg_cfg.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list cfg_cmm.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list cfg_cfg.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} dropped cfg_cfg.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets cfg_cmm.to_cmm.cmx.dump
      cfg_cfg.to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps cfg.output cfg.output.corrected)
 (action (diff cfg.output cfg.output.corrected)))

(rule
 (targets ssa.output.corrected)
 (deps cfg.ml to_cmm.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p ssa_cmm ssa_ssa ssa_cfg)
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_cmm.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dcmm -color never -o
      ssa_cmm/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_ssa.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dssa
      -dfdo-dropped-counters -color never -o ssa_ssa/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_cfg.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dcfg -color never -o
      ssa_cfg/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_cmm.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dcmm -color never -o
      ssa_cmm/to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_ssa.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dssa
      -dfdo-dropped-counters -color never -o ssa_ssa/to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     ssa_cfg.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -dcfg -color never -o
      ssa_cfg/to_cmm.cmx to_cmm.ml)))
   (with-stdout-to
    ssa.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-sets ssa_cmm.cfg.cmx.dump
      ssa_ssa.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list ssa_ssa.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} dropped ssa_ssa.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets ssa_cmm.cfg.cmx.dump
      ssa_cfg.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets ssa_cmm.to_cmm.cmx.dump
      ssa_ssa.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets ssa_cmm.to_cmm.cmx.dump
      ssa_cfg.to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps ssa.output ssa.output.corrected)
 (action (diff ssa.output ssa.output.corrected)))

(rule
 (targets linear.output.corrected)
 (deps linear.ml cfg.ml to_cmm.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p linear_cmm linear_linear)
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_cmm.linear.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -color never -o
      linear_cmm/linear.cmx linear.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_linear.linear.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dlinear -color never -o
      linear_linear/linear.cmx linear.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_cmm.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -color never -o
      linear_cmm/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_linear.cfg.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dlinear -color never -o
      linear_linear/cfg.cmx cfg.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_cmm.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dcmm -color never -o
      linear_cmm/to_cmm.cmx to_cmm.ml)))
   (setenv
    OCAMLPARAM
    ""
    (with-stderr-to
     linear_linear.to_cmm.cmx.dump
     (run %{bin:ocamlopt.opt} -c -fdo-counters -dlinear -color never -o
      linear_linear/to_cmm.cmx to_cmm.ml)))
   (with-stdout-to
    linear.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} compare-sets
      linear_cmm.linear.cmx.dump linear_linear.linear.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list linear_linear.linear.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets linear_cmm.cfg.cmx.dump
      linear_linear.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} list linear_linear.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} compare-sets
      linear_cmm.to_cmm.cmx.dump linear_linear.to_cmm.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps linear.output linear.output.corrected)
 (action (diff linear.output linear.output.corrected)))

(rule
 (targets backend_o3.output.corrected)
 (deps linear.ml cfg.ml to_cmm.ml to_cmm_reaper.ml simplify.ml)
 (enabled_if (= %{context_name} "main"))
 (action
  (progn
   (run mkdir -p backend_o3_cmm backend_o3_linear backend_o3_ssa_cmm
    backend_o3_ssa_linear backend_o3_merge_blocks_cmm
    backend_o3_merge_blocks_linear)
   (with-stderr-to
    backend_o3_cmm.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dcmm -color never -o
     backend_o3_cmm/linear.cmx linear.ml))
   (with-stderr-to
    backend_o3_linear.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_linear/linear.cmx
     linear.ml))
   (with-stderr-to
    backend_o3_cmm.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dcmm -color never -o
     backend_o3_cmm/cfg.cmx cfg.ml))
   (with-stderr-to
    backend_o3_linear.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_linear/cfg.cmx cfg.ml))
   (with-stderr-to
    backend_o3_cmm.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dcmm -color never -o
     backend_o3_cmm/to_cmm.cmx to_cmm.ml))
   (with-stderr-to
    backend_o3_linear.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_linear/to_cmm.cmx
     to_cmm.ml))
   (with-stderr-to
    backend_o3_cmm.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dcmm -color never -o
     backend_o3_cmm/to_cmm_reaper.cmx to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_linear.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_linear/to_cmm_reaper.cmx
     to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_cmm.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dcmm -color never -o
     backend_o3_cmm/simplify.cmx simplify.ml))
   (with-stderr-to
    backend_o3_linear.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_linear/simplify.cmx
     simplify.ml))
   (with-stderr-to
    backend_o3_ssa_cmm.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dcmm -color never -o
     backend_o3_ssa_cmm/linear.cmx linear.ml))
   (with-stderr-to
    backend_o3_ssa_linear.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_ssa_linear/linear.cmx
     linear.ml))
   (with-stderr-to
    backend_o3_ssa_cmm.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dcmm -color never -o
     backend_o3_ssa_cmm/cfg.cmx cfg.ml))
   (with-stderr-to
    backend_o3_ssa_linear.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_ssa_linear/cfg.cmx
     cfg.ml))
   (with-stderr-to
    backend_o3_ssa_cmm.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dcmm -color never -o
     backend_o3_ssa_cmm/to_cmm.cmx to_cmm.ml))
   (with-stderr-to
    backend_o3_ssa_linear.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_ssa_linear/to_cmm.cmx
     to_cmm.ml))
   (with-stderr-to
    backend_o3_ssa_cmm.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dcmm -color never -o
     backend_o3_ssa_cmm/to_cmm_reaper.cmx to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_ssa_linear.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_ssa_linear/to_cmm_reaper.cmx to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_ssa_cmm.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dcmm -color never -o
     backend_o3_ssa_cmm/simplify.cmx simplify.ml))
   (with-stderr-to
    backend_o3_ssa_linear.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o backend_o3_ssa_linear/simplify.cmx
     simplify.ml))
   (with-stderr-to
    backend_o3_merge_blocks_cmm.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dcmm -color
     never -o backend_o3_merge_blocks_cmm/linear.cmx linear.ml))
   (with-stderr-to
    backend_o3_merge_blocks_linear.linear.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_merge_blocks_linear/linear.cmx linear.ml))
   (with-stderr-to
    backend_o3_merge_blocks_cmm.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dcmm -color
     never -o backend_o3_merge_blocks_cmm/cfg.cmx cfg.ml))
   (with-stderr-to
    backend_o3_merge_blocks_linear.cfg.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_merge_blocks_linear/cfg.cmx cfg.ml))
   (with-stderr-to
    backend_o3_merge_blocks_cmm.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dcmm -color
     never -o backend_o3_merge_blocks_cmm/to_cmm.cmx to_cmm.ml))
   (with-stderr-to
    backend_o3_merge_blocks_linear.to_cmm.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_merge_blocks_linear/to_cmm.cmx to_cmm.ml))
   (with-stderr-to
    backend_o3_merge_blocks_cmm.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dcmm -color
     never -o backend_o3_merge_blocks_cmm/to_cmm_reaper.cmx to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_merge_blocks_linear.to_cmm_reaper.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_merge_blocks_linear/to_cmm_reaper.cmx to_cmm_reaper.ml))
   (with-stderr-to
    backend_o3_merge_blocks_cmm.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dcmm -color
     never -o backend_o3_merge_blocks_cmm/simplify.cmx simplify.ml))
   (with-stderr-to
    backend_o3_merge_blocks_linear.simplify.cmx.dump
    (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -dlinear
     -dfdo-dropped-counters -color never -o
     backend_o3_merge_blocks_linear/simplify.cmx simplify.ml))
   (with-stdout-to
    backend_o3.output.corrected
    (progn
     (run %{exe:tools/fdo_annotations.exe} check backend_o3_cmm.linear.cmx.dump
      backend_o3_linear.linear.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check backend_o3_cmm.cfg.cmx.dump
      backend_o3_linear.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check backend_o3_cmm.to_cmm.cmx.dump
      backend_o3_linear.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_cmm.to_cmm_reaper.cmx.dump
      backend_o3_linear.to_cmm_reaper.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_cmm.simplify.cmx.dump backend_o3_linear.simplify.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_ssa_cmm.linear.cmx.dump backend_o3_ssa_linear.linear.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check backend_o3_ssa_cmm.cfg.cmx.dump
      backend_o3_ssa_linear.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_ssa_cmm.to_cmm.cmx.dump backend_o3_ssa_linear.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_ssa_cmm.to_cmm_reaper.cmx.dump
      backend_o3_ssa_linear.to_cmm_reaper.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_ssa_cmm.simplify.cmx.dump
      backend_o3_ssa_linear.simplify.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_merge_blocks_cmm.linear.cmx.dump
      backend_o3_merge_blocks_linear.linear.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_merge_blocks_cmm.cfg.cmx.dump
      backend_o3_merge_blocks_linear.cfg.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_merge_blocks_cmm.to_cmm.cmx.dump
      backend_o3_merge_blocks_linear.to_cmm.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_merge_blocks_cmm.to_cmm_reaper.cmx.dump
      backend_o3_merge_blocks_linear.to_cmm_reaper.cmx.dump)
     (run %{exe:tools/fdo_annotations.exe} check
      backend_o3_merge_blocks_cmm.simplify.cmx.dump
      backend_o3_merge_blocks_linear.simplify.cmx.dump))))))

(rule
 (alias runtest)
 (enabled_if (= %{context_name} "main"))
 (deps backend_o3.output backend_o3.output.corrected)
 (action (diff backend_o3.output backend_o3.output.corrected)))

(rule
 (targets body_hash.default.without.code body_hash.default.with.code)
 (deps body_hash.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/body_hash.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/body_hash.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff body_hash.default.without.code body_hash.default.with.code)))

(rule
 (targets cfg.default.without.code cfg.default.with.code)
 (deps cfg.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o codegen_default_without/cfg.cmx
    cfg.ml)
   (with-stdout-to
    cfg.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/cfg.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/cfg.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff cfg.default.without.code cfg.default.with.code)))

(rule
 (targets function_ids.default.without.code function_ids.default.with.code)
 (deps function_ids.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/function_ids.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/function_ids.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff function_ids.default.without.code function_ids.default.with.code)))

(rule
 (targets instrumentation.default.without.code
  instrumentation.default.with.code)
 (deps instrumentation.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff instrumentation.default.without.code instrumentation.default.with.code)))

(rule
 (targets linear.default.without.code linear.default.with.code)
 (deps linear.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/linear.cmx linear.ml)
   (with-stdout-to
    linear.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/linear.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/linear.cmx linear.ml)
   (with-stdout-to
    linear.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/linear.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff linear.default.without.code linear.default.with.code)))

(rule
 (targets simplify.default.without.code simplify.default.with.code)
 (deps simplify.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/simplify.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/simplify.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff simplify.default.without.code simplify.default.with.code)))

(rule
 (targets to_cmm.default.without.code to_cmm.default.with.code)
 (deps to_cmm.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm.default.without.code to_cmm.default.with.code)))

(rule
 (targets to_cmm_probe.default.without.code to_cmm_probe.default.with.code)
 (deps to_cmm_probe.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm_probe.default.without.code to_cmm_probe.default.with.code)))

(rule
 (targets to_cmm_reaper.default.without.code to_cmm_reaper.default.with.code)
 (deps to_cmm_reaper.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_default_without codegen_default_with)
   (run %{bin:ocamlopt.opt} -c -color never -o
    codegen_default_without/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.default.without.code
    (pipe-stdout
     (run objdump -dr codegen_default_without/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -fdo-counters -color never -o
    codegen_default_with/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.default.with.code
    (pipe-stdout
     (run objdump -dr codegen_default_with/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm_reaper.default.without.code to_cmm_reaper.default.with.code)))

(rule
 (targets body_hash.o3.without.code body_hash.o3.with.code)
 (deps body_hash.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/body_hash.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/body_hash.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff body_hash.o3.without.code body_hash.o3.with.code)))

(rule
 (targets cfg.o3.without.code cfg.o3.with.code)
 (deps cfg.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o codegen_o3_without/cfg.cmx
    cfg.ml)
   (with-stdout-to
    cfg.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/cfg.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/cfg.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff cfg.o3.without.code cfg.o3.with.code)))

(rule
 (targets function_ids.o3.without.code function_ids.o3.with.code)
 (deps function_ids.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/function_ids.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/function_ids.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff function_ids.o3.without.code function_ids.o3.with.code)))

(rule
 (targets instrumentation.o3.without.code instrumentation.o3.with.code)
 (deps instrumentation.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff instrumentation.o3.without.code instrumentation.o3.with.code)))

(rule
 (targets linear.o3.without.code linear.o3.with.code)
 (deps linear.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o codegen_o3_without/linear.cmx
    linear.ml)
   (with-stdout-to
    linear.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/linear.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/linear.cmx linear.ml)
   (with-stdout-to
    linear.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/linear.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff linear.o3.without.code linear.o3.with.code)))

(rule
 (targets simplify.o3.without.code simplify.o3.with.code)
 (deps simplify.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/simplify.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/simplify.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff simplify.o3.without.code simplify.o3.with.code)))

(rule
 (targets to_cmm.o3.without.code to_cmm.o3.with.code)
 (deps to_cmm.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o codegen_o3_without/to_cmm.cmx
    to_cmm.ml)
   (with-stdout-to
    to_cmm.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm.o3.without.code to_cmm.o3.with.code)))

(rule
 (targets to_cmm_probe.o3.without.code to_cmm_probe.o3.with.code)
 (deps to_cmm_probe.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm_probe.o3.without.code to_cmm_probe.o3.with.code)))

(rule
 (targets to_cmm_reaper.o3.without.code to_cmm_reaper.o3.with.code)
 (deps to_cmm_reaper.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_without codegen_o3_with)
   (run %{bin:ocamlopt.opt} -c -O3 -color never -o
    codegen_o3_without/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_without/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -fdo-counters -color never -o
    codegen_o3_with/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_with/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm_reaper.o3.without.code to_cmm_reaper.o3.with.code)))

(rule
 (targets body_hash.ssa.without.code body_hash.ssa.with.code)
 (deps body_hash.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/body_hash.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/body_hash.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff body_hash.ssa.without.code body_hash.ssa.with.code)))

(rule
 (targets cfg.ssa.without.code cfg.ssa.with.code)
 (deps cfg.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/cfg.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/cfg.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff cfg.ssa.without.code cfg.ssa.with.code)))

(rule
 (targets function_ids.ssa.without.code function_ids.ssa.with.code)
 (deps function_ids.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/function_ids.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/function_ids.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff function_ids.ssa.without.code function_ids.ssa.with.code)))

(rule
 (targets instrumentation.ssa.without.code instrumentation.ssa.with.code)
 (deps instrumentation.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff instrumentation.ssa.without.code instrumentation.ssa.with.code)))

(rule
 (targets linear.ssa.without.code linear.ssa.with.code)
 (deps linear.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/linear.cmx linear.ml)
   (with-stdout-to
    linear.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/linear.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/linear.cmx linear.ml)
   (with-stdout-to
    linear.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/linear.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff linear.ssa.without.code linear.ssa.with.code)))

(rule
 (targets simplify.ssa.without.code simplify.ssa.with.code)
 (deps simplify.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/simplify.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/simplify.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff simplify.ssa.without.code simplify.ssa.with.code)))

(rule
 (targets to_cmm.ssa.without.code to_cmm.ssa.with.code)
 (deps to_cmm.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm.ssa.without.code to_cmm.ssa.with.code)))

(rule
 (targets to_cmm_probe.ssa.without.code to_cmm_probe.ssa.with.code)
 (deps to_cmm_probe.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm_probe.ssa.without.code to_cmm_probe.ssa.with.code)))

(rule
 (targets to_cmm_reaper.ssa.without.code to_cmm_reaper.ssa.with.code)
 (deps to_cmm_reaper.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_ssa_without codegen_ssa_with)
   (run %{bin:ocamlopt.opt} -c -use-ssa -color never -o
    codegen_ssa_without/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_without/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -use-ssa -fdo-counters -color never -o
    codegen_ssa_with/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_ssa_with/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm_reaper.ssa.without.code to_cmm_reaper.ssa.with.code)))

(rule
 (targets body_hash.o3_ssa.without.code body_hash.o3_ssa.with.code)
 (deps body_hash.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/body_hash.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/body_hash.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff body_hash.o3_ssa.without.code body_hash.o3_ssa.with.code)))

(rule
 (targets cfg.o3_ssa.without.code cfg.o3_ssa.with.code)
 (deps cfg.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/cfg.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/cfg.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff cfg.o3_ssa.without.code cfg.o3_ssa.with.code)))

(rule
 (targets function_ids.o3_ssa.without.code function_ids.o3_ssa.with.code)
 (deps function_ids.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/function_ids.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/function_ids.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff function_ids.o3_ssa.without.code function_ids.o3_ssa.with.code)))

(rule
 (targets instrumentation.o3_ssa.without.code instrumentation.o3_ssa.with.code)
 (deps instrumentation.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff instrumentation.o3_ssa.without.code instrumentation.o3_ssa.with.code)))

(rule
 (targets linear.o3_ssa.without.code linear.o3_ssa.with.code)
 (deps linear.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/linear.cmx linear.ml)
   (with-stdout-to
    linear.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/linear.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/linear.cmx linear.ml)
   (with-stdout-to
    linear.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/linear.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff linear.o3_ssa.without.code linear.o3_ssa.with.code)))

(rule
 (targets simplify.o3_ssa.without.code simplify.o3_ssa.with.code)
 (deps simplify.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/simplify.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/simplify.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff simplify.o3_ssa.without.code simplify.o3_ssa.with.code)))

(rule
 (targets to_cmm.o3_ssa.without.code to_cmm.o3_ssa.with.code)
 (deps to_cmm.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm.o3_ssa.without.code to_cmm.o3_ssa.with.code)))

(rule
 (targets to_cmm_probe.o3_ssa.without.code to_cmm_probe.o3_ssa.with.code)
 (deps to_cmm_probe.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff to_cmm_probe.o3_ssa.without.code to_cmm_probe.o3_ssa.with.code)))

(rule
 (targets to_cmm_reaper.o3_ssa.without.code to_cmm_reaper.o3_ssa.with.code)
 (deps to_cmm_reaper.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_ssa_without codegen_o3_ssa_with)
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -color never -o
    codegen_o3_ssa_without/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3_ssa.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_without/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -use-ssa -fdo-counters -color never -o
    codegen_o3_ssa_with/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3_ssa.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_ssa_with/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm_reaper.o3_ssa.without.code to_cmm_reaper.o3_ssa.with.code)))

(rule
 (targets body_hash.o3_merge_blocks.without.code
  body_hash.o3_merge_blocks.with.code)
 (deps body_hash.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/body_hash.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/body_hash.cmx body_hash.ml)
   (with-stdout-to
    body_hash.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/body_hash.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff body_hash.o3_merge_blocks.without.code
   body_hash.o3_merge_blocks.with.code)))

(rule
 (targets cfg.o3_merge_blocks.without.code cfg.o3_merge_blocks.with.code)
 (deps cfg.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/cfg.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/cfg.cmx cfg.ml)
   (with-stdout-to
    cfg.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/cfg.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action (diff cfg.o3_merge_blocks.without.code cfg.o3_merge_blocks.with.code)))

(rule
 (targets function_ids.o3_merge_blocks.without.code
  function_ids.o3_merge_blocks.with.code)
 (deps function_ids.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/function_ids.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/function_ids.cmx function_ids.ml)
   (with-stdout-to
    function_ids.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/function_ids.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff function_ids.o3_merge_blocks.without.code
   function_ids.o3_merge_blocks.with.code)))

(rule
 (targets instrumentation.o3_merge_blocks.without.code
  instrumentation.o3_merge_blocks.with.code)
 (deps instrumentation.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/instrumentation.cmx instrumentation.ml)
   (with-stdout-to
    instrumentation.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/instrumentation.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff instrumentation.o3_merge_blocks.without.code
   instrumentation.o3_merge_blocks.with.code)))

(rule
 (targets linear.o3_merge_blocks.without.code linear.o3_merge_blocks.with.code)
 (deps linear.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/linear.cmx linear.ml)
   (with-stdout-to
    linear.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/linear.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/linear.cmx linear.ml)
   (with-stdout-to
    linear.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/linear.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff linear.o3_merge_blocks.without.code linear.o3_merge_blocks.with.code)))

(rule
 (targets simplify.o3_merge_blocks.without.code
  simplify.o3_merge_blocks.with.code)
 (deps simplify.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/simplify.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/simplify.cmx simplify.ml)
   (with-stdout-to
    simplify.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/simplify.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff simplify.o3_merge_blocks.without.code
   simplify.o3_merge_blocks.with.code)))

(rule
 (targets to_cmm.o3_merge_blocks.without.code to_cmm.o3_merge_blocks.with.code)
 (deps to_cmm.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/to_cmm.cmx to_cmm.ml)
   (with-stdout-to
    to_cmm.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/to_cmm.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm.o3_merge_blocks.without.code to_cmm.o3_merge_blocks.with.code)))

(rule
 (targets to_cmm_probe.o3_merge_blocks.without.code
  to_cmm_probe.o3_merge_blocks.with.code)
 (deps to_cmm_probe.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/to_cmm_probe.cmx to_cmm_probe.ml)
   (with-stdout-to
    to_cmm_probe.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/to_cmm_probe.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm_probe.o3_merge_blocks.without.code
   to_cmm_probe.o3_merge_blocks.with.code)))

(rule
 (targets to_cmm_reaper.o3_merge_blocks.without.code
  to_cmm_reaper.o3_merge_blocks.with.code)
 (deps to_cmm_reaper.ml)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (progn
   (run mkdir -p codegen_o3_merge_blocks_without codegen_o3_merge_blocks_with)
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -color never -o
    codegen_o3_merge_blocks_without/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3_merge_blocks.without.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_without/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh})))
   (run %{bin:ocamlopt.opt} -c -O3 -cfg-merge-blocks -fdo-counters -color never
    -o codegen_o3_merge_blocks_with/to_cmm_reaper.cmx to_cmm_reaper.ml)
   (with-stdout-to
    to_cmm_reaper.o3_merge_blocks.with.code
    (pipe-stdout
     (run objdump -dr codegen_o3_merge_blocks_with/to_cmm_reaper.o)
     (run %{dep:tools/filter_objdump.sh}))))))

(rule
 (alias runtest)
 (enabled_if (and (= %{context_name} "main") (= %{system} "linux")))
 (action
  (diff to_cmm_reaper.o3_merge_blocks.without.code
   to_cmm_reaper.o3_merge_blocks.with.code)))
