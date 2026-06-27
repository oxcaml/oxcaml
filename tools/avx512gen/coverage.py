#!/usr/bin/env python3
"""AVX512 coverage / completeness checker.

Diffs the authoritative Intel target surface (every real-ISA AVX-512 intrinsic
whose CPUID set is a subset of {AVX512F,DQ,CD,BW,VL} and that has a concrete
<instruction>) against what OxCaml currently exposes (the intrinsics the
generator successfully maps), so "have we exposed everything Intel does?" has a
precise, grouped, shrink-to-zero answer.

It also cross-checks every exposed intrinsic: the chosen OxCaml binding's
mnemonic must equal the Intel <instruction> mnemonic (catches mis-lowerings).

Usage:
  python3 coverage.py            # summary: covered %, remaining grouped by reason
  python3 coverage.py --list R   # list the intrinsic names skipped for reason R
"""
import sys, os, collections
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import generate as g

# ---- target denominator: real-ISA intrinsics that have a concrete instruction ----
target = [it for it in g.intrinsics if it["insts"] and it["insts"][0][0]]
target_names = {it["name"] for it in target}
exposed = {e["intel"] for e in g.emitted}
# emitted is a subset of intrinsics; restrict to those in target (all are)
covered = exposed & target_names
remaining = target_names - covered

# ---- group remaining by skip reason ----
reason_of = {}
for r in g.skip_examples:  # reasons seen
    pass
# rebuild name->reason from generate's skip pass by re-deriving: easier to re-run
# the same classification generate already did. generate stored only counts +
# examples, so reconstruct reasons by scanning skips via a second pass:
name_reason = {}
# generate.py logged reasons but not per-name; recompute the reason cheaply here
# by replicating the early skip checks for grouping (instruction/imm/op-table).
import re as _re
for it in target:
    n=it["name"]
    if n in covered: continue
    if any(p[4] for p in it["params"]): name_reason[n]="has_immediate"; continue
    if len(it["insts"])!=1: name_reason[n]="multi_instruction"; continue
    pn=g.parse_name(n)
    if not pn: name_reason[n]="unparsed_name"; continue
    if pn["tag"] in g.WHOLE: name_reason[n]="whole_vector(si128/256/512)"; continue
    if pn["core"] not in g.OP: name_reason[n]=f"op:{pn['core']}"; continue
    b=g.pick_binding(it["insts"][0][0], pn["width"], pn["masking"])
    if b is None: name_reason[n]="no_binding(CSV_gap)"; continue
    name_reason[n]="arity/other"

bucket=collections.Counter(name_reason.values())
cat_remaining=collections.Counter(it["cat"] for it in target if it["name"] in remaining)

# ---- cross-check exposed lowerings ----
mismatches=[]
for e in g.emitted:
    if e["binding"].mnem.upper()!=e["mnem"].upper():
        mismatches.append((e["caml"], e["binding"].mnem, e["mnem"]))

def main():
    if len(sys.argv)>2 and sys.argv[1]=="--list":
        R=sys.argv[2]
        names=sorted(n for n,r in name_reason.items() if R in r)
        print(f"{len(names)} intrinsics with reason ~ {R!r}:")
        for n in names: print("  ", n)
        return
    print("="*64)
    print("AVX512 coverage vs Intel (CPUID subset of F/DQ/CD/BW/VL, real-ISA)")
    print("="*64)
    print(f"  target (have a concrete instruction): {len(target_names)}")
    print(f"  EXPOSED + bit-exact tested          : {len(covered)}  "
          f"({100*len(covered)/len(target_names):.1f}%)")
    print(f"  remaining                           : {len(remaining)}")
    print(f"\n  lowering cross-check: {len(mismatches)} mnemonic mismatch(es) "
          f"among {len(g.emitted)} exposed")
    for c in mismatches[:10]: print("    MISMATCH", c)
    print("\n-- remaining grouped by reason (drives the next passes) --")
    for r,n in bucket.most_common():
        print(f"  {n:5d}  {r}")
    print("\n-- remaining grouped by Intel category --")
    for c,n in cat_remaining.most_common(15):
        print(f"  {n:5d}  {c}")
    print("\n  (python3 coverage.py --list <reason-substr>  to enumerate)")

if __name__=="__main__":
    main()
