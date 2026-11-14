// Load a raw Wasm package (wasm_of_ocaml package --raw) in Node.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const fsPromises = require("node:fs/promises");
const path = require("node:path");

const directory = path.resolve(process.argv[2]);
const sharedExportName = process.argv[4] || "Shared";
const manifest = JSON.parse(fs.readFileSync(path.join(directory, "bundle-manifest.json")));
const expectedFiles = [...manifest.assets.map(asset => asset.path), "bundle-manifest.json"].sort();
assert.deepEqual(fs.readdirSync(directory).sort(), expectedFiles);
let eagerBytes = 0;
let deferredBytes = 0;
for (const asset of manifest.assets) {
  assert.equal(fs.statSync(path.join(directory, asset.path)).size, asset.bytes);
  assert.match(asset.path, /\.(js|mjs|wasm)$/);
  if (asset.phase === "eager") eagerBytes += asset.bytes;
  else if (asset.phase === "deferred") deferredBytes += asset.bytes;
  else assert.fail(`Unknown phase: ${asset.phase}`);
}
assert.equal(manifest.eager_bytes, eagerBytes);
assert.equal(manifest.deferred_bytes, deferredBytes);
assert.equal(manifest.total_bytes, eagerBytes + deferredBytes);
assert.ok(deferredBytes > 0);
assert.ok(fs.readFileSync(path.join(directory, "root-Lazy_root.fragments.mjs"), "utf8").startsWith("#!/usr/bin/env node\n"));
assert.ok(!manifest.runtime_exports.some(name => /dynlink|reify_bytecode/i.test(name)));
assert.doesNotMatch(fs.readFileSync(path.join(directory, "loader.js"), "utf8"), /initializer failed after registration/);

const loads = [];
const readFile = fsPromises.readFile;
fsPromises.readFile = function (filename, ...args) {
  loads.push(path.basename(filename));
  return readFile.call(this, filename, ...args);
};
const instantiate = WebAssembly.instantiate;
let shared;
let reusedShared = 0;
WebAssembly.instantiate = async function (code, imports, ...args) {
  if (imports.OCaml?.[sharedExportName]) {
    if (manifest.eager_layout === "merged") assert.strictEqual(imports.env, imports.OCaml);
    assert.strictEqual(imports.OCaml[sharedExportName], shared);
    reusedShared++;
  }
  const result = await instantiate.call(this, code, imports, ...args);
  if (result.instance.exports[sharedExportName]) shared = result.instance.exports[sharedExportName];
  return result;
};

// The stock Node loader resolves Wasm paths relative to the entrypoint.
require.main.filename = path.join(directory, "loader.js");
const failAfterRegistration = process.argv[3] === "fail";
globalThis.partitionTest = { events: [], failAfterRegistration };
require(path.join(directory, "loader.js"));
(async () => {
  await globalThis.jsooWasmReady;
  for (const asset of manifest.assets.filter(asset => asset.path.endsWith(".wasm"))) {
    const module = await WebAssembly.compile(fs.readFileSync(path.join(directory, asset.path)), {
      builtins: ["js-string", "text-decoder", "text-encoder"],
      importedStringConstants: "str",
    });
    for (const item of [...WebAssembly.Module.imports(module), ...WebAssembly.Module.exports(module)]) {
      assert.doesNotMatch(item.name, /dynlink|reify_bytecode/i);
    }
  }
  assert.deepEqual(globalThis.partitionTest.events, ["shared", "forced", "main"]);
  assert.equal(await globalThis.partitionTest.main(), 1);
  if (typeof globalThis.partitionTest.bucketCount === "function") {
    assert.equal(await globalThis.partitionTest.bucketCount(), 26);
  }
  assert.equal(globalThis.partitionTest.lazy, undefined);
  assert.equal(globalThis.partitionTest.mainFragment, true);
  assert.equal(globalThis.partitionTest.lazyFragment, undefined);
  assert.equal(loads.filter(name => name === "root-Lazy_root.wasm").length, 0);
  assert.ok(shared instanceof WebAssembly.Global);
  const hasRichState = typeof globalThis.partitionTest.before === "function";
  if (hasRichState) {
    assert.equal(await globalThis.partitionTest.before(), -1);
    assert.equal(await globalThis.partitionTest.probe(), -1);
    assert.equal(await globalThis.partitionTest.probeFactory(), -1);
    await globalThis.partitionTest.setState(7);
    assert.equal(await globalThis.partitionTest.currentState(), 7);
    for (const asset of manifest.assets.filter(asset => asset.phase === "eager")) {
      assert.ok(!fs.readFileSync(path.join(directory, asset.path)).includes("STATIC_PARTITION_LATE_ONLY_SENTINEL"));
    }
  }
  const first = globalThis.jsooWasmLoad("Lazy_root");
  assert.strictEqual(first, globalThis.jsooWasmLoad("Lazy_root"));
  if (failAfterRegistration) await assert.rejects(first, /initializer failed after registration/);
  else await first;
  assert.deepEqual(globalThis.partitionTest.events, ["shared", "forced", "main", "unique", "lazy"]);
  assert.equal(await globalThis.partitionTest.main(), 42);
  assert.equal(await globalThis.partitionTest.lazy(), 42);
  assert.equal(globalThis.partitionTest.lazyFragment, true);
  if (hasRichState && !failAfterRegistration) {
    assert.equal(await globalThis.partitionTest.before(), -1);
    assert.equal(await globalThis.partitionTest.probe(), 707);
    await globalThis.partitionTest.setState(9);
    assert.equal(await globalThis.partitionTest.probe(), 709);
    await globalThis.partitionTest.replaceState(11);
    assert.equal(await globalThis.partitionTest.currentState(), 11);
    assert.equal(await globalThis.partitionTest.probe(), 709);
    assert.equal(await globalThis.partitionTest.probeFactory(), 23);
    assert.deepEqual(globalThis.partitionTest.events, ["shared", "forced", "main", "unique", "lazy", "STATIC_PARTITION_LATE_ONLY_SENTINEL"]);
  }
  assert.strictEqual(first, globalThis.jsooWasmLoad("Lazy_root"));
  assert.equal(loads.filter(name => name === "root-Lazy_root.wasm").length, 1);
  assert.equal(reusedShared, manifest.eager_layout === "merged" ? 1 : 2);
  const missing = globalThis.jsooWasmLoad("missing");
  await assert.rejects(missing, /Unknown static Wasm chunk/);
  assert.strictEqual(missing, globalThis.jsooWasmLoad("missing"));
  console.log(`Wasm packaging: byte manifest, deferred fetch, init-once, shared GC global, rich state=${hasRichState}, sticky failure=${failAfterRegistration}: ok`);
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
