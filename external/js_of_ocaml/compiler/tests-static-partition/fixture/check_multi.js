// Run JavaScript splits with three roots (a, b, c), loading the roots in every order.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const orders = [["a", "b", "c"], ["a", "c", "b"], ["b", "a", "c"], ["b", "c", "a"], ["c", "a", "b"], ["c", "b", "a"]];
const literals = { a: "STATIC_PARTITION_LATE_ONLY_SENTINEL", b: "MULTI_ROOT_B_ONLY_SENTINEL", c: "MULTI_ROOT_C_ONLY_SENTINEL" };
const units = { a: "Lazy_root", b: "Lazy_b", c: "Lazy_c" };
for (const directory of process.argv.slice(2)) {
  const read = name => fs.readFileSync(path.join(directory, name), "utf8");
  const main = read("main.js");
  const runtime = read("runtime.js");
  const sources = Object.fromEntries(["a", "b", "c"].map(id => [id, read(`root-${units[id]}.js`)]));
  const report = JSON.parse(read("report.json"));
  assert.equal(report.roots.length, 3);
  for (const root of report.roots) assert.equal(root.file, `root-${units[root.key]}.js`);
  for (const primitive of ["partition_test_b_only", "partition_test_c_only"]) assert.ok(report.runtime_primitives.includes(primitive));
  for (const [id, literal] of Object.entries(literals)) {
    assert.ok(!main.includes(literal), `${id} literal leaked eagerly`);
    for (const [other, source] of Object.entries(sources)) assert.equal(source.includes(literal), other === id, `${id} literal ownership`);
  }
  for (const order of orders) {
    const context = vm.createContext({ console, partitionTest: { events: [] } });
    vm.runInContext(runtime + main, context);
    const api = context.partitionTest;
    assert.deepEqual(Array.from(api.events), ["shared", "forced", "main", "both"]);
    assert.equal(api.bucketCount(), 26);
    assert.equal(api.before(), -1);
    const bridge = context.jsoo_runtime.caml_get_global("jsoo:split:bridge");
    const loaded = new Set();
    const snapshots = {};
    for (const [index, id] of order.entries()) {
      snapshots[id] = index + 7;
      api.setState(index + 7);
      vm.runInContext(sources[id], context);
      loaded.add(id);
      if (id === "a") {
        assert.equal(api.callExport(context.jsoo_runtime.caml_get_global("Lazy_root")), 42);
      }
      assert.equal(api.lazy !== undefined, loaded.has("a"));
      assert.equal(api.bProbe !== undefined, loaded.has("b"));
      assert.equal(api.cProbe !== undefined, loaded.has("c"));
      assert.equal(api.before(), -1);
      for (const active of loaded) {
        const probe = active === "a" ? api.probe : active === "b" ? api.bProbe : api.cProbe;
        assert.equal(probe(), snapshots[active] * 100 + index + 7);
      }
      assert.strictEqual(context.jsoo_runtime.caml_get_global("jsoo:split:bridge"), bridge);
      if (loaded.has("b")) assert.equal(api.bCell(), 11 + (loaded.has("c") ? 2 : 0));
      if (loaded.has("c")) assert.equal(api.cCell(), 12 + (loaded.has("b") ? 1 : 0));
    }
    assert.equal(api.sameIdentity(), 1);
    assert.equal(api.bPrimitive(), 101);
    assert.equal(api.cPrimitive(), 113);
    assert.equal(api.main(), 42);
    api.setState(19);
    assert.equal(api.bProbe(), snapshots.b * 100 + 19);
    api.replaceState(23);
    assert.equal(api.bProbe(), snapshots.b * 100 + 19);
    assert.equal(api.cProbe(), snapshots.c * 100 + 19);
    assert.equal(api.probe(), snapshots.a * 100 + 19);
    assert.equal(api.events.filter(event => event === "both").length, 1);
  }
}
console.log("three independent roots, all orders, snapshots, identity, literals, and primitive union: ok");
