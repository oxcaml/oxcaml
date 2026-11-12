// Command-line checks of js_of_ocaml split that the other fixtures don't exercise.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawnSync } = require("node:child_process");

const executable = path.resolve(process.argv[2]);
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "partition-cli-"));
const inputs = ["shared.cma", "numeric_buckets.cmo", "main.cmo", "lazy.cma", "runtime_stubs.js"];

function split(args, errorPattern) {
  const output = fs.mkdtempSync(path.join(scratch, "out-"));
  const result = spawnSync(executable, ["split", ...args, "-o", output], { encoding: "utf8" });
  assert.equal(result.error, undefined);
  if (errorPattern) {
    assert.notEqual(result.status, 0, args.join(" "));
    assert.match(result.stderr, errorPattern);
    assert.deepEqual(fs.readdirSync(output), [], "no output on failure");
  } else {
    assert.equal(result.status, 0, result.stderr);
    return JSON.parse(fs.readFileSync(path.join(output, "report.json"), "utf8"));
  }
}

try {
  const root = ["--main", "Main", "--root", "Lazy_root"];
  // Lazy_root only reaches lazy.cma's units; Main doesn't depend on them.
  split([...inputs, ...root, "--check-deferred-handle", "Main=lazy.cma"]);
  split([...inputs, ...root, "--check-deferred-handle", "Lazy_root=shared.cma"],
    /Deferred handle Lazy_root has a runtime dependency on its implementation Shared: Lazy_root -> Shared/);
  split([...inputs, ...root, "--check-deferred-handle", "Main=main.cmo"],
    /Deferred handle Main has a runtime dependency on its implementation Main/);
  split([...inputs, ...root, "--check-deferred-handle", "Main=runtime_stubs.js"],
    /is not an input/);
  split([...inputs, ...root, "--require-deferred-unit", "Shared"],
    /Imported implementation Shared must belong to exactly one lazy root/);
  split([...inputs, "notes.txt", ...root], /Unexpected input file notes.txt/);
  split([...inputs, "--main", "Main", "--root", "Missing"],
    /Unresolved compilation-unit dependency Missing \(required by lazy root\)/);
  split([...inputs.filter(name => name !== "main.cmo"), "main.cmo", ...root]);
  split(["main.cmo", ...inputs.filter(name => name !== "main.cmo"), ...root],
    /Inputs are not in initialization order: Main requires \w+/);
  const report = split([...inputs, "--main", "Main", "--root", "chunk-1=Lazy_root",
    "--setenv", "FORCE_DROP_INLINE_TEST=true", "--enable", "use-js-string", "--opt", "2"]);
  assert.deepEqual(report.roots.map(root => [root.key, root.unit]), [["chunk-1", "Lazy_root"]]);
  assert.deepEqual(report.static_env, { FORCE_DROP_INLINE_TEST: "true" });
  assert.equal(report.use_js_string, true);
  assert.equal(report.profile, 2);
  assert.deepEqual(report.partitions.unused, ["Unused"]);
  assert.deepEqual(report.partitions.forced_eager, ["Forced"]);
  console.log("split command line: handles, required units, inputs, keys and options: ok");
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
