// The package commands reject invalid inputs and outputs without writing anything.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawnSync } = require("node:child_process");

const jsOfOcaml = path.resolve(process.argv[2]);
const wasmOfOcaml = path.resolve(process.argv[3]);
const fixture = path.resolve("../fixture");
const archive = path.resolve("packages/runtime.wasma");
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "package-cli-"));
const split = name => path.join(fixture, name);

function rejected(executable, args, pattern, { entry = "entry.js", output = "assets" } = {}) {
  const directory = fs.mkdtempSync(path.join(scratch, "case-"));
  const result = spawnSync(executable, ["package", ...args], { cwd: directory, encoding: "utf8" });
  assert.equal(result.error, undefined);
  assert.notEqual(result.status, 0, args.join(" "));
  assert.match(result.stderr, pattern);
  assert.equal(fs.existsSync(path.join(directory, entry)), false, "no entry on failure");
  const outputPath = path.join(directory, output);
  assert.ok(!fs.existsSync(outputPath) || fs.readdirSync(outputPath).length === 0 || output === "occupied",
    "no assets on failure");
}

const wasm = (...args) => [...args, split("runtime_stubs.js"), "--runtime-archive", archive];
try {
  rejected(jsOfOcaml, [split("modules-js"), "--entry", "entry.js", "-o", "assets"],
    /requires a split with strategy late, not modules/);
  rejected(jsOfOcaml, [split("late-wasm"), "--entry", "entry.js", "-o", "assets"],
    /Expected a javascript split, not a wasm one/);
  rejected(wasmOfOcaml, wasm(split("late-js"), "--entry", "entry.js", "-o", "assets"),
    /Expected a wasm split, not a javascript one/);
  rejected(jsOfOcaml, [fixture, "--entry", "entry.js", "-o", "assets"],
    /Not the output directory of a split/);
  rejected(jsOfOcaml, [split("late-js"), "--entry", "elsewhere/entry.js", "-o", "assets"],
    /must be siblings/, { entry: "elsewhere/entry.js" });
  rejected(jsOfOcaml, [split("late-js"), "--entry", "entry.js", "-o", "bad name"],
    /must be siblings with normalized names/, { output: "bad name" });
  {
    // A non-empty output directory.
    const directory = fs.mkdtempSync(path.join(scratch, "occupied-"));
    fs.mkdirSync(path.join(directory, "assets"));
    fs.writeFileSync(path.join(directory, "assets", "existing"), "");
    const result = spawnSync(jsOfOcaml, ["package", split("late-js"), "--entry", "entry.js", "-o", "assets"],
      { cwd: directory, encoding: "utf8" });
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /Output directory must be empty/);
    assert.deepEqual(fs.readdirSync(path.join(directory, "assets")), ["existing"]);
    assert.equal(fs.existsSync(path.join(directory, "entry.js")), false);
  }
  rejected(wasmOfOcaml, wasm(split("multi-wasm"), "--raw", "-o", "assets"),
    /A raw package has exactly one root/);
  rejected(wasmOfOcaml, wasm(split("late-wasm"), "-o", "assets"),
    /Either --entry or --raw is required/);
  rejected(wasmOfOcaml, wasm(split("late-wasm"), "--raw", "--entry", "entry.js", "-o", "assets"),
    /--raw and --entry are exclusive/);
  rejected(wasmOfOcaml, wasm(split("late-wasm"), "--raw", "--loader-global", "same", "--ready-global", "same", "-o", "assets"),
    /The loader and ready globals must differ/);
  {
    // A runtime built for another effects backend.
    const directory = fs.mkdtempSync(path.join(scratch, "cps-"));
    const cps = path.join(directory, "cps.wasma");
    const build = spawnSync(wasmOfOcaml, ["build-runtime", "--effects", "cps", "--no-source-map",
      split("runtime_stubs.js"), split("runtime_stubs.wat"), "-o", cps], { encoding: "utf8" });
    assert.equal(build.status, 0, build.stderr);
    rejected(wasmOfOcaml, [split("late-wasm"), split("runtime_stubs.js"), "--runtime-archive", cps, "--raw", "-o", "assets"],
      /Incompatible runtime archive: effects is cps, expected jspi/);
  }
  console.log("package command line: backends, reports, outputs, modes and runtimes: ok");
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
