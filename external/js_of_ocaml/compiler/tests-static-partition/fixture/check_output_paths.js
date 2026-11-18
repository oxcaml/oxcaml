// The compiler must not write outputs over its inputs or over each other, and must
// leave everything unchanged when it rejects its outputs.
"use strict";
const assert = require("node:assert/strict");
const crypto = require("node:crypto");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawnSync } = require("node:child_process");

const executable = path.resolve(process.argv[2]);
const inputNames = ["runtime_stubs.js", "shared.cma", "numeric_buckets.cmo", "main.cmo", "lazy.cma"];
const hash = file => crypto.createHash("sha256").update(fs.readFileSync(file)).digest("hex");
const originalHashes = inputNames.map(hash);
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "partition-output-paths-"));

function snapshot(file) {
  try {
    const stat = fs.lstatSync(file);
    return stat.isSymbolicLink() ? ["symlink", fs.readlinkSync(file)] : ["file", hash(file)];
  } catch (error) {
    if (error.code === "ENOENT") return null;
    throw error;
  }
}

function check(name, setup, errorPattern) {
  const directory = path.join(scratch, name);
  const out = path.join(directory, "out");
  fs.mkdirSync(out, { recursive: true });
  const file = name => path.join(directory, name);
  for (const name of inputNames) {
    fs.copyFileSync(name, file(name));
    fs.chmodSync(file(name), 0o600);
  }
  const inputs = setup(file) ?? inputNames;
  const outputs = ["main.js", "root-Lazy_root.js", "runtime.js", "report.json"].map(name => path.join("out", name));
  const before = outputs.map(name => snapshot(file(name)));
  const result = spawnSync(executable, [
    "split", ...inputs, "--main", "Main", "--root", "Lazy_root", "-o", "out",
  ], { cwd: directory, encoding: "utf8" });
  assert.equal(result.error, undefined);
  if (errorPattern) {
    assert.notEqual(result.status, 0, name);
    assert.match(result.stderr + result.stdout, errorPattern);
    assert.deepEqual(outputs.map(name => snapshot(file(name))), before, name);
  } else {
    assert.equal(result.status, 0, result.stderr);
    assert.notEqual(hash(file("out/main.js")), hash(file("out/root-Lazy_root.js")));
    if (name === "live-output-link") assert.ok(fs.lstatSync(file("out/main.js")).isSymbolicLink());
  }
  assert.deepEqual(inputNames.map(name => hash(file(name))), originalHashes, name);
}

try {
  check("ordinary", () => {});
  check("live-output-link", file => {
    fs.writeFileSync(file("independent.js"), "existing output");
    fs.symlinkSync("../independent.js", file("out/main.js"));
  });
  check("symlink-to-input", file => fs.symlinkSync("../main.cmo", file("out/main.js")), /Output would overwrite an input/);
  check("hardlink-to-input", file => fs.linkSync(file("main.cmo"), file("out/main.js")), /Output would overwrite an input/);
  check("input-in-output-directory", file => {
    fs.copyFileSync(file("runtime_stubs.js"), file("out/runtime.js"));
    return inputNames.map(name => name === "runtime_stubs.js" ? "out/runtime.js" : name);
  }, /Output would overwrite an input/);
  check("hardlinked-outputs", file => {
    fs.writeFileSync(file("out/main.js"), "existing output");
    fs.linkSync(file("out/main.js"), file("out/runtime.js"));
  }, /Output paths must be distinct/);
  check("dangling-output-link", file => fs.symlinkSync("missing.js", file("out/root-Lazy_root.js")), /Output is a dangling symlink/);
  assert.deepEqual(inputNames.map(hash), originalHashes);
  console.log("Output alias preflight: copied inputs and outputs preserved; valid live symlinks accepted: ok");
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
