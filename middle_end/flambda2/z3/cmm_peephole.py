#!/usr/bin/env python3

from z3 import BitVecs, LShR, Solver, ULT, sat, unsat


LLVM_REVISION = "95b14f3dd3db293caec8b51fad2a93373b07c587"
LLVM_SOURCE_ROOT = (
    f"https://github.com/llvm/llvm-project/blob/{LLVM_REVISION}/"
    "llvm/lib/Transforms/InstCombine/"
)
LLVM_SOURCES = (
    "InstCombineAndOrXor.cpp#L2541-L2562",
    "InstCombineAndOrXor.cpp#L5324-L5331",
    "InstCombineAndOrXor.cpp#L5523-L5531",
    "InstCombineSimplifyDemanded.cpp#L286-L393",
    "InstCombineShifts.cpp#L759-L777",
    "InstCombineShifts.cpp#L1185-L1188",
)


def verify_rules(bits: int) -> None:
    x, a, b, shift = BitVecs("x a b shift", bits)
    valid_shift = ULT(shift, bits)

    # Constants remain symbolic, so each proof covers every word-sized mask.
    rules = (
        ("and_or_disjoint", (x | a) & b, x & b, a & b == 0),
        ("and_or_covered", (x | a) & b, b, a & b == b),
        ("and_xor_disjoint", (x ^ a) & b, x & b, a & b == 0),
        ("or_and_cover", (x & a) | b, x | b, a | b == -1),
        ("or_and_covered", (x & a) | b, b, a & ~b == 0),
        ("or_xor_covered", (x ^ a) | b, x | b, a & ~b == 0),
        ("xor_or_same", (x | a) ^ a, x & ~a, True),
        ("xor_and_complement", (x & a) ^ ~a, x | ~a, True),
        (
            "lsl_lsr_same",
            LShR(x, shift) << shift,
            x & (-1 << shift),
            valid_shift,
        ),
        (
            "lsl_asr_same",
            (x >> shift) << shift,
            x & (-1 << shift),
            valid_shift,
        ),
        (
            "lsr_lsl_same",
            LShR(x << shift, shift),
            x & LShR(-1, shift),
            valid_shift,
        ),
        ("not_add", ~(x + a), ~a - x, True),
        ("not_sub", ~(a - x), x + ~a, True),
    )

    for name, before, after, guard in rules:
        solver = Solver()
        solver.add(guard)
        result = solver.check()
        if result != sat:
            raise RuntimeError(f"{name}: guard is not satisfiable: {result}")
        solver.add(before != after)
        result = solver.check()
        if result == sat:
            raise AssertionError(f"{name} ({bits} bits): {solver.model()}")
        if result != unsat:
            raise RuntimeError(f"{name}: {solver.reason_unknown()}")
        print(f"Verified {name} ({bits} bits)")


if __name__ == "__main__":
    for source in LLVM_SOURCES:
        print(LLVM_SOURCE_ROOT + source)
    for bits in (32, 64):
        verify_rules(bits)
