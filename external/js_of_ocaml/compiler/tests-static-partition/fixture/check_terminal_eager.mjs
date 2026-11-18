// Split a program whose eager code always raises: optimization then removes the
// roots' anchors, and the split must keep the eager behavior and every output.
import assert from "node:assert/strict";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import vm from "node:vm";
import { spawnSync } from "node:child_process";
import { pathToFileURL } from "node:url";

const compilers = { javascript: path.resolve(process.argv[2]), wasm: path.resolve(process.argv[3]) };
const fixture = process.cwd();
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "terminal-eager-"));
const inputs = ["shared.cma", "lazy.cma", "both.cmo", "lazy_b.cmo", "lazy_c.cmo", "terminal_main.cmo"].map(name => path.join(fixture, name));
const deadNames = ["STATIC_PARTITION_LATE_ONLY_SENTINEL", "MULTI_ROOT_B_ONLY_SENTINEL", "MULTI_ROOT_C_ONLY_SENTINEL", "partition_test_c_only", "partition_test_expose_lazy"];
const ownedUnits = unit => unit === "Lazy_root" ? ["Unique", "Lazy_root"] : [unit];

function compile(backend, units, strategy, label) {
  const directory = path.join(scratch, label);
  const args = [
    "split", "--strategy", strategy, "--opt", "3",
    "--effects", backend === "wasm" ? "jspi" : "disabled",
    ...inputs, path.join(fixture, "runtime_stubs.js"),
    ...(backend === "wasm" ? [path.join(fixture, "runtime_stubs.wat")] : []),
    "--main", "Terminal_main", ...units.flatMap(unit => ["--root", unit]), "-o", directory,
  ];
  const result = spawnSync(compilers[backend], args, { encoding: "utf8" });
  assert.equal(result.error, undefined);
  assert.equal(result.status, 0, result.stderr);
  const read = name => fs.readFileSync(path.join(directory, name));
  const report = JSON.parse(read("report.json").toString());
  assert.equal(report.strategy, strategy);
  assert.equal(report.main_unit, "Terminal_main");
  const extension = backend === "wasm" ? "wasm" : "js";
  const lazyFiles = units.map(unit => `root-${unit}.${extension}`);
  for (const name of lazyFiles) assert.ok(fs.existsSync(path.join(directory, name)));
  assert.deepEqual(report.partitions.lazy, ["Unique", "Lazy_root", "Lazy_b", "Lazy_c"].filter(unit => units.flatMap(ownedUnits).includes(unit)));
  assert.deepEqual(report.roots.map(root => root.unit), units);
  assert.deepEqual(report.roots.map(root => root.file), lazyFiles);
  assert.deepEqual(report.roots.map(root => root.units), units.map(ownedUnits));
  if (backend === "wasm") {
    assert.deepEqual(report.roots.map(root => root.wasm_module), units.map(unit => `partition_lazy_${unit}`));
    assert.deepEqual(report.roots.map(root => root.fragments), units.map(unit => `root-${unit}.fragments.mjs`));
  }
  if (strategy === "late") {
    for (const { late_partition: statistics } of report.roots) {
      assert.deepEqual(statistics.captures, []);
      assert.deepEqual(statistics.relocated, []);
      assert.equal(statistics.rounds, 0);
      assert.equal(statistics.globally_optimized_blocks, statistics.eager_blocks);
      assert.equal(statistics.lazy_blocks, 1);
    }
    assert.deepEqual(report.partitions.main_exports, []);
    for (const name of fs.readdirSync(directory)) {
      if (!/\.(js|mjs|wasm)$/.test(name)) continue;
      const bytes = read(name);
      for (const sentinel of deadNames) assert.ok(!bytes.includes(sentinel), `${name}: resurrected ${sentinel}`);
    }
  }
  return { units, strategy, directory, lazyFiles, read, report };
}

function checkJavascript({ read, units, lazyFiles, strategy }) {
  const context = vm.createContext({ console, partitionTest: { events: [] } });
  vm.runInContext(read("runtime.js").toString(), context);
  let caught;
  try { vm.runInContext(read("main.js").toString(), context); }
  catch (error) { caught = error; }
  const api = context.partitionTest;
  assert.ok(caught, "eager initialization must raise");
  assert.strictEqual(caught, api.expectedException());
  assert.equal(api.beforeFailure(), 17);
  assert.deepEqual(Array.from(api.events), ["shared", "forced", ...(units.length > 1 ? ["both"] : []), "terminal-eager", "right"]);
  assert.equal(api.lazy, undefined);
  assert.equal(api.bProbe, undefined);
  assert.equal(api.cProbe, undefined);
  if (strategy === "late") {
    const events = Array.from(api.events);
    for (const name of lazyFiles) vm.runInContext(read(name).toString(), context);
    assert.deepEqual(Array.from(api.events), events);
    assert.equal(api.lazy, undefined);
    assert.equal(api.bProbe, undefined);
    assert.equal(api.cProbe, undefined);
  }
}

async function checkWasm({ directory, read, units, lazyFiles, report }) {
  for (const name of ["main.wasm", "startup.wasm", ...lazyFiles]) {
    const module = await WebAssembly.compile(read(name));
    assert.ok(!WebAssembly.Module.exports(module).some(item => item.name === "jsoo:split:bridge"));
  }
  const initializers = [];
  for (const [index, unit] of units.entries()) {
    const unitName = `partition_lazy_${unit}`;
    const module = await WebAssembly.compile(read(lazyFiles[index]));
    assert.ok(WebAssembly.Module.exports(module).some(item => item.name === `${unitName}.init` && item.kind === "function"));
    assert.deepEqual(WebAssembly.Module.imports(module), [], "empty roots must not require runtime work");
    const instance = await WebAssembly.instantiate(module, {});
    instance.exports[`${unitName}.init`]();
    const fragments = (await import(pathToFileURL(path.join(directory, `root-${unit}.fragments.mjs`)).href)).default;
    assert.ok(fragments && typeof fragments === "object");
    assert.ok(Object.keys(fragments).every(name => name === `${unitName}.fragments`));
    initializers.push(`${unitName}.init`);
  }
  await import(pathToFileURL(path.join(directory, "main.fragments.mjs")).href);
  assert.deepEqual(report.roots.map(root => root.initializer), initializers);
}

try {
  checkJavascript(compile("javascript", ["Lazy_root"], "modules", "modules-control"));
  for (const units of [["Lazy_root"], ["Lazy_c", "Lazy_b", "Lazy_root"]]) {
    checkJavascript(compile("javascript", units, "late", `js-${units.length}`));
    await checkWasm(compile("wasm", units, "late", `wasm-${units.length}`));
  }
  console.log("Terminal eager: JS exception identity/effects preserved; metadata retained; empty Wasm initializers invoked: ok");
} finally {
  fs.rmSync(scratch, { recursive: true, force: true });
}
