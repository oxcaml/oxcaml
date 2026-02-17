; Generated automatically by this directory's dune.
; Use `make regen-flambda2-test-dune-rules` to regenerate.

(alias (name regen))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe inlined_rec.flt)
   (diff? inlined_rec.flt inlined_rec.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe inlined_rec.in.fl)
   (diff? inlined_rec.in.fl inlined_rec.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe inlined_rec.out.fl)
   (diff? inlined_rec.out.fl inlined_rec.out.fl.corrected))))

(rule
 (alias regen)
 (targets inlined_rec.flt.new inlined_rec.in.fl.new inlined_rec.out.fl.new)
 (deps inlined_rec.ml)
 (action
   (run ocamlopt -c inlined_rec.ml -nopervasives -nostdlib
        -drawfexpr-to inlined_rec.in.fl.new
        -dfexpr-to inlined_rec.out.fl.new
        -dflexpect-to inlined_rec.flt.new)))

(rule
 (alias regen)
 (action (diff inlined_rec.in.fl inlined_rec.in.fl.new)))

(rule
 (alias regen)
 (action (diff inlined_rec.out.fl inlined_rec.out.fl.new)))

(rule
 (alias regen)
 (action (diff inlined_rec.flt inlined_rec.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe local.flt)
   (diff? local.flt local.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe local.in.fl)
   (diff? local.in.fl local.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe local.out.fl)
   (diff? local.out.fl local.out.fl.corrected))))

(rule
 (alias regen)
 (targets local.flt.new local.in.fl.new local.out.fl.new)
 (deps local.ml)
 (action
   (run ocamlopt -c local.ml -nopervasives -nostdlib
        -drawfexpr-to local.in.fl.new
        -dfexpr-to local.out.fl.new
        -dflexpect-to local.flt.new)))

(rule
 (alias regen)
 (action (diff local.in.fl local.in.fl.new)))

(rule
 (alias regen)
 (action (diff local.out.fl local.out.fl.new)))

(rule
 (alias regen)
 (action (diff local.flt local.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests0.flt)
   (diff? tests0.flt tests0.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests0.in.fl)
   (diff? tests0.in.fl tests0.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests0.out.fl)
   (diff? tests0.out.fl tests0.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests0.flt.new tests0.in.fl.new tests0.out.fl.new)
 (deps tests0.ml)
 (action
   (run ocamlopt -c tests0.ml -nopervasives -nostdlib
        -drawfexpr-to tests0.in.fl.new
        -dfexpr-to tests0.out.fl.new
        -dflexpect-to tests0.flt.new)))

(rule
 (alias regen)
 (action (diff tests0.in.fl tests0.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests0.out.fl tests0.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests0.flt tests0.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests2.flt)
   (diff? tests2.flt tests2.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests2.in.fl)
   (diff? tests2.in.fl tests2.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests2.out.fl)
   (diff? tests2.out.fl tests2.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests2.flt.new tests2.in.fl.new tests2.out.fl.new)
 (deps tests2.ml)
 (action
   (run ocamlopt -c tests2.ml -nopervasives -nostdlib
        -drawfexpr-to tests2.in.fl.new
        -dfexpr-to tests2.out.fl.new
        -dflexpect-to tests2.flt.new)))

(rule
 (alias regen)
 (action (diff tests2.in.fl tests2.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests2.out.fl tests2.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests2.flt tests2.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests3.flt)
   (diff? tests3.flt tests3.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests3.in.fl)
   (diff? tests3.in.fl tests3.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests3.out.fl)
   (diff? tests3.out.fl tests3.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests3.flt.new tests3.in.fl.new tests3.out.fl.new)
 (deps tests3.ml)
 (action
   (run ocamlopt -c tests3.ml -nopervasives -nostdlib
        -drawfexpr-to tests3.in.fl.new
        -dfexpr-to tests3.out.fl.new
        -dflexpect-to tests3.flt.new)))

(rule
 (alias regen)
 (action (diff tests3.in.fl tests3.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests3.out.fl tests3.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests3.flt tests3.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests4.flt)
   (diff? tests4.flt tests4.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4.in.fl)
   (diff? tests4.in.fl tests4.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4.out.fl)
   (diff? tests4.out.fl tests4.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests4.flt.new tests4.in.fl.new tests4.out.fl.new)
 (deps tests4.ml)
 (action
   (run ocamlopt -c tests4.ml -nopervasives -nostdlib
        -drawfexpr-to tests4.in.fl.new
        -dfexpr-to tests4.out.fl.new
        -dflexpect-to tests4.flt.new)))

(rule
 (alias regen)
 (action (diff tests4.in.fl tests4.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests4.out.fl tests4.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests4.flt tests4.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests4a.flt)
   (diff? tests4a.flt tests4a.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4a.in.fl)
   (diff? tests4a.in.fl tests4a.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests4a.out.fl)
   (diff? tests4a.out.fl tests4a.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests4a.flt.new tests4a.in.fl.new tests4a.out.fl.new)
 (deps tests4a.ml)
 (action
   (run ocamlopt -c tests4a.ml -nopervasives -nostdlib
        -drawfexpr-to tests4a.in.fl.new
        -dfexpr-to tests4a.out.fl.new
        -dflexpect-to tests4a.flt.new)))

(rule
 (alias regen)
 (action (diff tests4a.in.fl tests4a.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests4a.out.fl tests4a.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests4a.flt tests4a.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests5.flt)
   (diff? tests5.flt tests5.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests5.in.fl)
   (diff? tests5.in.fl tests5.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests5.out.fl)
   (diff? tests5.out.fl tests5.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests5.flt.new tests5.in.fl.new tests5.out.fl.new)
 (deps tests5.ml)
 (action
   (run ocamlopt -c tests5.ml -nopervasives -nostdlib
        -drawfexpr-to tests5.in.fl.new
        -dfexpr-to tests5.out.fl.new
        -dflexpect-to tests5.flt.new)))

(rule
 (alias regen)
 (action (diff tests5.in.fl tests5.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests5.out.fl tests5.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests5.flt tests5.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests6.flt)
   (diff? tests6.flt tests6.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests6.in.fl)
   (diff? tests6.in.fl tests6.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests6.out.fl)
   (diff? tests6.out.fl tests6.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests6.flt.new tests6.in.fl.new tests6.out.fl.new)
 (deps tests6.ml)
 (action
   (run ocamlopt -c tests6.ml -nopervasives -nostdlib
        -drawfexpr-to tests6.in.fl.new
        -dfexpr-to tests6.out.fl.new
        -dflexpect-to tests6.flt.new)))

(rule
 (alias regen)
 (action (diff tests6.in.fl tests6.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests6.out.fl tests6.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests6.flt tests6.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests7.flt)
   (diff? tests7.flt tests7.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests7.in.fl)
   (diff? tests7.in.fl tests7.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests7.out.fl)
   (diff? tests7.out.fl tests7.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests7.flt.new tests7.in.fl.new tests7.out.fl.new)
 (deps tests7.ml)
 (action
   (run ocamlopt -c tests7.ml -nopervasives -nostdlib
        -drawfexpr-to tests7.in.fl.new
        -dfexpr-to tests7.out.fl.new
        -dflexpect-to tests7.flt.new)))

(rule
 (alias regen)
 (action (diff tests7.in.fl tests7.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests7.out.fl tests7.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests7.flt tests7.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests14.flt)
   (diff? tests14.flt tests14.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests14.in.fl)
   (diff? tests14.in.fl tests14.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests14.out.fl)
   (diff? tests14.out.fl tests14.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests14.flt.new tests14.in.fl.new tests14.out.fl.new)
 (deps tests14.ml)
 (action
   (run ocamlopt -c tests14.ml -nopervasives -nostdlib
        -drawfexpr-to tests14.in.fl.new
        -dfexpr-to tests14.out.fl.new
        -dflexpect-to tests14.flt.new)))

(rule
 (alias regen)
 (action (diff tests14.in.fl tests14.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests14.out.fl tests14.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests14.flt tests14.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe tests15.flt)
   (diff? tests15.flt tests15.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests15.in.fl)
   (diff? tests15.in.fl tests15.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe tests15.out.fl)
   (diff? tests15.out.fl tests15.out.fl.corrected))))

(rule
 (alias regen)
 (targets tests15.flt.new tests15.in.fl.new tests15.out.fl.new)
 (deps tests15.ml)
 (action
   (run ocamlopt -c tests15.ml -nopervasives -nostdlib
        -drawfexpr-to tests15.in.fl.new
        -dfexpr-to tests15.out.fl.new
        -dflexpect-to tests15.flt.new)))

(rule
 (alias regen)
 (action (diff tests15.in.fl tests15.in.fl.new)))

(rule
 (alias regen)
 (action (diff tests15.out.fl tests15.out.fl.new)))

(rule
 (alias regen)
 (action (diff tests15.flt tests15.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll.flt)
   (diff? unroll.flt unroll.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll.in.fl)
   (diff? unroll.in.fl unroll.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll.out.fl)
   (diff? unroll.out.fl unroll.out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll.flt.new unroll.in.fl.new unroll.out.fl.new)
 (deps unroll.ml)
 (action
   (run ocamlopt -c unroll.ml -nopervasives -nostdlib
        -drawfexpr-to unroll.in.fl.new
        -dfexpr-to unroll.out.fl.new
        -dflexpect-to unroll.flt.new)))

(rule
 (alias regen)
 (action (diff unroll.in.fl unroll.in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll.out.fl unroll.out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll.flt unroll.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll2.flt)
   (diff? unroll2.flt unroll2.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll2.in.fl)
   (diff? unroll2.in.fl unroll2.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll2.out.fl)
   (diff? unroll2.out.fl unroll2.out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll2.flt.new unroll2.in.fl.new unroll2.out.fl.new)
 (deps unroll2.ml)
 (action
   (run ocamlopt -c unroll2.ml -nopervasives -nostdlib
        -drawfexpr-to unroll2.in.fl.new
        -dfexpr-to unroll2.out.fl.new
        -dflexpect-to unroll2.flt.new)))

(rule
 (alias regen)
 (action (diff unroll2.in.fl unroll2.in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll2.out.fl unroll2.out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll2.flt unroll2.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll3.flt)
   (diff? unroll3.flt unroll3.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll3.in.fl)
   (diff? unroll3.in.fl unroll3.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll3.out.fl)
   (diff? unroll3.out.fl unroll3.out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll3.flt.new unroll3.in.fl.new unroll3.out.fl.new)
 (deps unroll3.ml)
 (action
   (run ocamlopt -c unroll3.ml -nopervasives -nostdlib
        -drawfexpr-to unroll3.in.fl.new
        -dfexpr-to unroll3.out.fl.new
        -dflexpect-to unroll3.flt.new)))

(rule
 (alias regen)
 (action (diff unroll3.in.fl unroll3.in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll3.out.fl unroll3.out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll3.flt unroll3.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll4.flt)
   (diff? unroll4.flt unroll4.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll4.in.fl)
   (diff? unroll4.in.fl unroll4.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll4.out.fl)
   (diff? unroll4.out.fl unroll4.out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll4.flt.new unroll4.in.fl.new unroll4.out.fl.new)
 (deps unroll4.ml)
 (action
   (run ocamlopt -c unroll4.ml -nopervasives -nostdlib
        -drawfexpr-to unroll4.in.fl.new
        -dfexpr-to unroll4.out.fl.new
        -dflexpect-to unroll4.flt.new)))

(rule
 (alias regen)
 (action (diff unroll4.in.fl unroll4.in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll4.out.fl unroll4.out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll4.flt unroll4.flt.new)))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/flexpect.exe unroll5.flt)
   (diff? unroll5.flt unroll5.flt.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll5.in.fl)
   (diff? unroll5.in.fl unroll5.in.fl.corrected))))

(rule
 (alias runtest)
 (action
  (progn
   (run ../tools/roundtrip.exe unroll5.out.fl)
   (diff? unroll5.out.fl unroll5.out.fl.corrected))))

(rule
 (alias regen)
 (targets unroll5.flt.new unroll5.in.fl.new unroll5.out.fl.new)
 (deps unroll5.ml)
 (action
   (run ocamlopt -c unroll5.ml -nopervasives -nostdlib
        -drawfexpr-to unroll5.in.fl.new
        -dfexpr-to unroll5.out.fl.new
        -dflexpect-to unroll5.flt.new)))

(rule
 (alias regen)
 (action (diff unroll5.in.fl unroll5.in.fl.new)))

(rule
 (alias regen)
 (action (diff unroll5.out.fl unroll5.out.fl.new)))

(rule
 (alias regen)
 (action (diff unroll5.flt unroll5.flt.new)))
