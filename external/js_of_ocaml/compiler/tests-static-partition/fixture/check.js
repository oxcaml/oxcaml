// Run a JavaScript split with one root (modules-js, late-js or embedded-js).
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const directory = process.argv[2];
const embedded = directory === "embedded-js";
const late = directory !== "modules-js";
const file = name => path.join(directory, name);
const read = name => fs.readFileSync(file(name), "utf8");
const sharedGlobal = late ? "jsoo:split:bridge" : "Shared";
const report = JSON.parse(read("report.json"));
assert.deepEqual(report.partitions.main_exports, [sharedGlobal]);
assert.doesNotMatch(JSON.stringify(report), /\bDynlink\b/);
assert.deepEqual(report.roots.map(root => [root.key, root.unit, root.file]), [["Lazy_root", "Lazy_root", "root-Lazy_root.js"]]);
for (const name of ["main.js", "root-Lazy_root.js"]) {
  const source = read(name);
  assert.doesNotMatch(source, /STATIC_PARTITION_(?:PRIVATE|UNUSED)_SENTINEL/);
  assert.doesNotMatch(source, /\bcaml_reify_bytecode\b/);
}

if (embedded) {
  assert.deepEqual(report.embedded_files, ["fixture.meta:/static/fixture.meta"]);
  assert.ok(read("runtime.js").includes("numeric_buckets.cmo"));
}

const context = vm.createContext({ console, partitionTest: { events: [] } });
const evaluate = (name, target = context) => vm.runInContext(read(name), target, { filename: file(name) });
const evaluateMain = (target = context) => { evaluate("runtime.js", target); evaluate("main.js", target); };
const events = () => Array.from(context.partitionTest.events);

evaluateMain();
assert.deepEqual(events(), ["shared", "forced", "main"]);
assert.equal(context.partitionTest.main(), 1);
assert.equal(context.partitionTest.bucketCount(), 26);
assert.equal(context.partitionTest.lazy, undefined);
assert.equal(context.partitionTest.mainFragment, true);
assert.equal(context.partitionTest.lazyFragment, undefined);
const shared = context.jsoo_runtime.caml_get_global(sharedGlobal);
assert.equal(context.partitionTest.before(), -1);
assert.equal(context.partitionTest.probe(), -1);
assert.equal(context.partitionTest.probeFactory(), -1);
context.partitionTest.setState(7);

evaluate("root-Lazy_root.js");
assert.deepEqual(events(), ["shared", "forced", "main", "unique", "lazy"]);
assert.equal(context.partitionTest.main(), 42);
assert.equal(context.partitionTest.lazy(), 42);
assert.equal(context.partitionTest.lazyFragment, true);
const exported = context.jsoo_runtime.caml_get_global("Lazy_root");
assert.equal(context.partitionTest.callExport(exported), 42);
assert.equal(context.partitionTest.before(), -1);
assert.equal(context.partitionTest.probe(), 707);
context.partitionTest.setState(9);
assert.equal(context.partitionTest.probe(), 709);
context.partitionTest.replaceState(11);
assert.equal(context.partitionTest.currentState(), 11);
assert.equal(context.partitionTest.probe(), 709);
assert.equal(context.partitionTest.probeFactory(), 23);
assert.strictEqual(context.jsoo_runtime.caml_get_global(sharedGlobal), shared);
const failedContext = vm.createContext({ console, partitionTest: { events: [], failAfterRegistration: true } });
evaluateMain(failedContext);
assert.throws(() => evaluate("root-Lazy_root.js", failedContext), /initializer failed after registration/);
assert.equal(failedContext.partitionTest.main(), 42);
assert.equal(failedContext.partitionTest.lazy(), 42);
if (late) {
  assert.ok(!read("main.js").includes("STATIC_PARTITION_LATE_ONLY_SENTINEL"));
  assert.ok(read("root-Lazy_root.js").includes("STATIC_PARTITION_LATE_ONLY_SENTINEL"));
}
console.log(`${directory}: eager/lazy initialization and shared mutable identity: ok`);
