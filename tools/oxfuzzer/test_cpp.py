"""Standalone reference-backend checks; see README for the build command."""
import argparse
import asyncio
from pathlib import Path
import shutil
import subprocess

import oxfuzzer


RUNTIME_CHECKS = r'''
#include <cassert>
#define main generated_main
#include "numbers.cpp"
#undef main
int main() {
  using namespace reference;
  for (int x = 0; x < 256; ++x) {
    for (int y = 0; y < 256; ++y) {
      int sx = x < 128 ? x : x - 256;
      int sy = y < 128 ? y : y - 256;
      assert(binary(N::I8, B::Add, raw(x), raw(y)).bits == unsigned((x + y) & 255));
      assert(binary(N::I8, B::Sub, raw(x), raw(y)).bits == unsigned((x - y) & 255));
      assert(binary(N::I8, B::Mul, raw(x), raw(y)).bits == unsigned((x * y) & 255));
      assert(binary(N::I8, B::Bit_and, raw(x), raw(y)).bits == unsigned(x & y));
      assert(binary(N::I8, B::Bit_or, raw(x), raw(y)).bits == unsigned(x | y));
      assert(binary(N::I8, B::Bit_xor, raw(x), raw(y)).bits == unsigned(x ^ y));
      assert(as_int(binary(N::I8, B::Compare, raw(x), raw(y))) == (sx > sy) - (sx < sy));
      assert(as_int(binary(N::I8, B::Unsigned_compare, raw(x), raw(y))) == (x > y) - (x < y));
      if (sy != 0) {
        assert(binary(N::I8, B::Div, raw(x), raw(y)).bits == unsigned((sx / sy) & 255));
        assert(binary(N::I8, B::Rem, raw(x), raw(y)).bits == unsigned((sx % sy) & 255));
      }
    }
    int sx = x < 128 ? x : x - 256;
    assert(unary(N::I8, U::Neg, raw(x)).bits == unsigned((-x) & 255));
    assert(unary(N::I8, U::Abs, raw(x)).bits == unsigned(std::abs(sx) & 255));
    assert(unary(N::I8, U::Succ, raw(x)).bits == unsigned((x + 1) & 255));
    assert(unary(N::I8, U::Pred, raw(x)).bits == unsigned((x - 1) & 255));
    assert(unary(N::I8, U::Bit_not, raw(x)).bits == unsigned((~x) & 255));
    for (unsigned shift = 0; shift < 8; ++shift) {
      assert(binary(N::I8, B::Shift_left, raw(x), raw(shift)).bits == ((unsigned(x) << shift) & 255));
      assert(binary(N::I8, B::Shift_right_logical, raw(x), raw(shift)).bits == (unsigned(x) >> shift));
      int divisor = 1 << shift;
      int rounded = (sx < 0 ? sx - (divisor - 1) : sx) / divisor;
      assert(binary(N::I8, B::Shift_right, raw(x), raw(shift)).bits == unsigned(rounded & 255));
    }
  }
  for (N n : {N::I8, N::I16, N::I32, N::I63, N::I64}) {
    uint64_t sign = UINT64_C(1) << (width(n) - 1);
    assert(binary(n, B::Div, raw(sign), raw(mask(width(n)))).bits == sign);
    assert(binary(n, B::Rem, raw(sign), raw(mask(width(n)))).bits == 0);
    assert(unary(n, U::Abs, raw(sign)).bits == sign);
    assert(convert(N::F64, n, float_value(std::nan(""))).bits == 0);
    assert(convert(N::F64, n, float_value(INFINITY * 1.0)).bits == sign - 1);
    assert(convert(N::F64, n, float_value(-INFINITY * 1.0)).bits == sign);
  }
  assert(unary(N::F32, U::Succ, raw(0x80000000)).bits == 1);
  assert(unary(N::F64, U::Pred, raw(0)).bits == UINT64_C(0x8000000000000001));
  assert(unary(N::F32, U::Round, float_value(0.5f)).bits == float_value(1.0f).bits);
  assert(unary(N::F64, U::Round, float_value(-0.5)).bits == float_value(-1.0).bits);
  assert(binary(N::F64, B::Min, float_value(0.0), float_value(-0.0)).bits == float_value(-0.0).bits);
  assert(binary(N::F32, B::Max_num, float_value(std::nanf("")), float_value(2.0f)).bits == float_value(2.0f).bits);
  assert(truth(binary(N::F64, B::Eq, float_value(std::nan("1")), float_value(std::nan("2")))));
  assert(as_int(compare_string(B::Compare, string("\200", 1), string("\177", 1))) == 1);
  V original = aggregate(0, {raw(1)});
  V alias = original;
  V copy = copy_record(original);
  alias.object->fields.at(0) = raw(2);
  assert(field(original, 0).bits == 2 && field(copy, 0).bits == 1);
  std::puts("integer, floating-point, and sharing runtime checks passed");
}
'''


async def check_program(
    label: str,
    program: oxfuzzer.GeneratedProgram,
    *,
    toolchain: oxfuzzer.Toolchain,
    directory: Path,
    expected: bytes | None,
) -> None:
    slots = asyncio.Semaphore(3)
    results = await asyncio.gather(*(
        oxfuzzer.run_configuration(
            configuration, seed=0, program=program,
            work_dir=directory / configuration.name, slots=slots,
        )
        for configuration in toolchain.configurations
    ))
    for result in results:
        assert not result.compilation.timed_out, (label, result.configuration.name)
        assert result.compilation.returncode == 0, result.compilation.stderr.decode()
        assert result.execution is not None
        assert not result.execution.timed_out, (label, result.configuration.name)
        assert result.execution.returncode == 0, result.execution.stderr.decode()
        assert result.execution.stderr == b"", result.execution.stderr
        if expected is None:
            expected = result.execution.stdout
        assert result.execution.stdout == expected, (
            label, result.configuration.name, expected, result.execution.stdout
        )
    print(f"{label}: bytecode, native, and C++ agree", flush=True)


async def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--install-dir", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--cpp-compiler", default="g++")
    parser.add_argument("--cases-generator", type=Path, default=Path(
        "_build/main/tools/oxfuzzer/tests/reference_cases.exe"
    ))
    parser.add_argument("--samples", type=int, default=5)
    args = parser.parse_args()
    compiler = shutil.which(args.cpp_compiler)
    if compiler is None:
        parser.error(f"C++ compiler not found: {args.cpp_compiler}")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    fixtures = output / "fixtures"
    fixtures.mkdir()
    subprocess.run([str(args.cases_generator.resolve()), str(fixtures)], check=True)
    toolchain = oxfuzzer.Toolchain(
        args.install_dir.resolve(), installed=True, cpp_compiler=Path(compiler)
    )
    for source in sorted(fixtures.glob("*.ml")):
        await check_program(
            source.stem,
            oxfuzzer.GeneratedProgram(source.read_bytes(), source.with_suffix(".cpp").read_bytes()),
            toolchain=toolchain, directory=output / source.stem,
            expected=source.with_suffix(".expected").read_bytes(),
        )
    runtime_source = fixtures / "runtime_checks.cpp"
    runtime_source.write_text(RUNTIME_CHECKS)
    executable = fixtures / "runtime_checks.exe"
    subprocess.run([
        compiler, "-std=c++17", "-O1", "-fno-fast-math", "-ffp-contract=off",
        "-fsanitize=undefined", "-fno-sanitize-recover=all", str(runtime_source),
        "-o", str(executable),
    ], check=True, timeout=100)
    subprocess.run([str(executable)], check=True, timeout=30)
    for seed in range(args.samples):
        directory = output / f"seed-{seed}"
        program = await oxfuzzer.generate(seed, toolchain=toolchain, work_dir=directory)
        assert program is not None, f"generation timed out: {seed}"
        await check_program(f"seed {seed}", program, toolchain=toolchain,
                            directory=directory, expected=None)


if __name__ == "__main__":
    asyncio.run(main())
