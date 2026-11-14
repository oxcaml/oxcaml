// Check the structure of a Wasm split with one root (modules-wasm or late-wasm).
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import crypto from "node:crypto";
import { pathToFileURL } from "node:url";

const directory = process.argv[2];
const late = directory === "late-wasm";
const file = name => path.join(directory, name);
const fragmentsOf = async name => (await import(pathToFileURL(path.resolve(file(name))).href)).default;
const mainFragments = await fragmentsOf("main.fragments.mjs");
const lazyFragments = await fragmentsOf("root-Lazy_root.fragments.mjs");
const sharedGlobal = late ? "jsoo:split:bridge" : "Shared";
const lazyModule = "partition_lazy_Lazy_root";

const readModule = async name => {
  const bytes = fs.readFileSync(file(name));
  assert.ok(!bytes.includes("STATIC_PARTITION_PRIVATE_SENTINEL"));
  assert.ok(!bytes.includes("STATIC_PARTITION_UNUSED_SENTINEL"));
  const module = await WebAssembly.compile(bytes);
  const imports = WebAssembly.Module.imports(module);
  const exports = WebAssembly.Module.exports(module);
  for (const item of [...imports, ...exports]) {
    assert.doesNotMatch(item.name, /dynlink|reify_bytecode/i);
  }
  return { imports, exports };
};

const report = JSON.parse(fs.readFileSync(file("report.json"), "utf8"));
assert.deepEqual(report.wasm_initializers, ["prelude.init", "partition_main.init"]);
assert.deepEqual(report.roots.map(root => [root.file, root.fragments, root.wasm_module, root.initializer]),
  [["root-Lazy_root.wasm", "root-Lazy_root.fragments.mjs", lazyModule, `${lazyModule}.init`]]);
const main = await readModule("main.wasm");
const lazy = await readModule("root-Lazy_root.wasm");
const startup = await readModule("startup.wasm");
assert.ok(main.exports.some(item => item.name === "partition_main.init"));
assert.ok(main.exports.some(item => item.name === sharedGlobal && item.kind === "global"));
assert.ok(lazy.exports.some(item => item.name === `${lazyModule}.init`));
assert.ok(!lazy.exports.some(item => item.name === "_initialize"));
assert.deepEqual(lazy.exports.filter(item => item.kind === "global").map(item => item.name), ["Lazy_root"]);
assert.ok(!lazy.imports.some(item => item.name === "caml_main"));
assert.ok(lazy.imports.some(item => item.module === "OCaml" && item.name === sharedGlobal && item.kind === "global"));
assert.deepEqual(
  startup.imports.filter(item => item.module === "OCaml").map(item => item.name).sort(),
  ["partition_main.init", "prelude.init"],
);
for (const [moduleName, fragments] of [["partition_main", mainFragments], [lazyModule, lazyFragments]]) {
  assert.deepEqual(Object.keys(fragments), [`${moduleName}.fragments`]);
  assert.ok(Object.keys(fragments[`${moduleName}.fragments`]).length > 0);
}
if (late) {
  assert.deepEqual(main.exports.filter(item => item.kind === "global").map(item => item.name), [sharedGlobal]);
  const digest = crypto.createHash("md5").update("STATIC_PARTITION_LATE_ONLY_SENTINEL").digest("hex");
  assert.ok(report.roots[0].late_partition.relocated.some(value => value.literal_digest === digest),
    "the live lazy-only literal must be relocated, not captured eagerly");
}
console.log(`${directory}: optimized Wasm ownership, startup boundary, fragments, and DCE: ok`);
