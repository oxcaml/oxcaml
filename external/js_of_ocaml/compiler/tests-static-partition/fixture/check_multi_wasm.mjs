// Check the structure of a Wasm split with three roots.
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

const directory = process.argv[2];
const file = name => path.join(directory, name);
const compile = async name => WebAssembly.compile(fs.readFileSync(file(name)));
const modules = { main: await compile("main.wasm"), startup: await compile("startup.wasm") };
const roots = { a: "Lazy_root", b: "Lazy_b", c: "Lazy_c" };
for (const [id, unit] of Object.entries(roots)) {
  modules[id] = await compile(`root-${unit}.wasm`);
  const imports = WebAssembly.Module.imports(modules[id]);
  const exports = WebAssembly.Module.exports(modules[id]);
  assert.ok(exports.some(item => item.name === `partition_lazy_${unit}.init`));
  assert.ok(!exports.some(item => item.name === "_initialize"));
  assert.deepEqual(exports.filter(item => item.kind === "global").map(item => item.name), [unit]);
  assert.ok(!imports.some(item => /partition_lazy_|caml_main/.test(item.name)));
  assert.ok(imports.some(item => item.module === "OCaml" && item.name === "jsoo:split:bridge"));
  const fragments = (await import(pathToFileURL(path.resolve(file(`root-${unit}.fragments.mjs`))).href)).default;
  for (const name of Object.keys(fragments)) assert.equal(name, `partition_lazy_${unit}.fragments`);
}
assert.ok(WebAssembly.Module.imports(modules.b).some(item => item.name === "partition_test_b_only"));
assert.ok(WebAssembly.Module.imports(modules.c).some(item => item.name === "partition_test_c_only"));
assert.deepEqual(WebAssembly.Module.imports(modules.startup).filter(item => item.module === "OCaml").map(item => item.name).sort(), ["partition_main.init", "prelude.init"]);
assert.deepEqual(WebAssembly.Module.exports(modules.main).filter(item => item.kind === "global").map(item => item.name), ["jsoo:split:bridge"]);
const report = JSON.parse(fs.readFileSync(file("report.json"), "utf8"));
assert.deepEqual(report.roots.map(root => [root.key, root.unit, root.units]), [["a", "Lazy_root", ["Unique", "Lazy_root"]], ["b", "Lazy_b", ["Lazy_b"]], ["c", "Lazy_c", ["Lazy_c"]]]);
console.log("three sibling Wasm modules, stable namespaces, primitive imports, and eager-only startup: ok");
