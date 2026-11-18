// Load a generated package with three roots (a, b, c) in Node.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const fsPromises = require("node:fs/promises");
const path = require("node:path");
const vm = require("node:vm");

const entry = path.resolve(process.argv[2], "entry.js");
const assets = path.resolve(process.argv[2], "assets");
const mode = process.argv[3];
const manifest = JSON.parse(fs.readFileSync(path.join(assets, "bundle-manifest.json")));
const payload = path.join(assets, manifest.identity);
assert.equal(manifest.format, 1);
assert.equal(manifest.entry_bytes, fs.statSync(entry).size);
assert.deepEqual(manifest.roots.map(root => [root.key, root.unit]), [
  ["b", "Lazy_b"], ["c", "Lazy_c"], ["a", "Lazy_root"],
]);
const fileNames = manifest.assets.map(asset => asset.path);
assert.equal(new Set(fileNames).size, fileNames.length);
assert.deepEqual(fs.readdirSync(payload).sort(), ["bundle-manifest.json", ...fileNames.map(file => path.basename(file))].sort());
let eagerBytes = manifest.entry_bytes;
let deferredBytes = 0;
for (const asset of manifest.assets) {
  assert.equal(asset.path.split("/")[0], manifest.identity);
  assert.equal(asset.bytes, fs.statSync(path.join(assets, asset.path)).size);
  if (asset.phase === "eager") eagerBytes += asset.bytes;
  else if (asset.phase === "deferred") deferredBytes += asset.bytes;
  else assert.fail(asset.phase);
  if (asset.path.endsWith(".mjs")) {
    assert.ok(fs.readFileSync(path.join(assets, asset.path), "utf8").startsWith("#!/usr/bin/env node\n"));
  }
}
assert.equal(manifest.eager_bytes, eagerBytes);
assert.equal(manifest.deferred_bytes, deferredBytes);
assert.equal(manifest.total_bytes, eagerBytes + deferredBytes);
assert.equal(manifest.roots.reduce((sum, root) => sum + root.bytes, 0), deferredBytes);
for (const root of manifest.roots) {
  assert.equal(root.bytes, root.assets.reduce((sum, name) => sum + fs.statSync(path.join(assets, name)).size, 0));
  assert.ok(root.assets.every(name => manifest.assets.find(asset => asset.path === name)?.phase === "deferred"));
}
assert.deepEqual(manifest.roots.flatMap(root => root.assets).sort(), manifest.assets.filter(asset => asset.phase === "deferred").map(asset => asset.path).sort());

const scripts = [];
vm.runInNewContext(fs.readFileSync(entry, "utf8"), {
  URL,
  location: { origin: "https://example.test" },
  document: {
    currentScript: { src: "https://example.test/app/entry.js" },
    baseURI: "https://untrusted.test/",
    createElement: () => ({}),
    head: { appendChild: script => scripts.push(script) },
  },
});
assert.equal(scripts[0].src, `https://example.test/app/${path.basename(assets)}/${manifest.identity}/loader.js`);

const loads = [];
const readFile = fsPromises.readFile;
let releaseA;
const waitA = new Promise(resolve => { releaseA = resolve; });
fsPromises.readFile = async function (filename, ...args) {
  const name = path.basename(filename instanceof URL ? filename.pathname : String(filename));
  loads.push(name);
  if (mode === "concurrent" && name === "root-Lazy_root.wasm") await waitA;
  return readFile.call(this, filename, ...args);
};
let startupLoad;
let mainFragment;
globalThis.partitionTest = { events: [], failAfterRegistration: mode === "lazy-fail" };
Object.defineProperty(globalThis.partitionTest, "mainFragment", {
  get() { return mainFragment; },
  set(value) {
    mainFragment = value;
    startupLoad = globalThis.__jsoo_imports_v2.load("a");
    startupLoad.catch(() => {});
    assert.strictEqual(startupLoad, globalThis.__jsoo_imports_v2.load("a"));
    if (mode === "eager-fail") throw new Error("multi eager initialization failed");
  },
});

(async () => {
  try { require(entry); }
  catch (error) {
    if (mode !== "eager-fail") throw error;
    assert.match(String(error), /multi eager initialization failed/);
  }
  const registry = globalThis.__jsoo_imports_v2;
  const state = globalThis.partitionTest;
  assert.ok(Object.isFrozen(registry));
  if (mode === "eager-fail") {
    await assert.rejects(registry.ready, /multi eager initialization failed/);
    for (const key of ["a", "b", "c"]) {
      const load = registry.load(key);
      await assert.rejects(load, /multi eager initialization failed/);
      assert.strictEqual(load, registry.load(key));
    }
    assert.equal(loads.some(name => name.startsWith("root-")), false);
    assert.equal(state.lazyFragment, undefined);
    assert.equal(state.bProbe, undefined);
    assert.equal(state.cProbe, undefined);
  } else {
    await registry.ready;
    assert.ok(startupLoad);
    assert.equal(state.bProbe, undefined);
    assert.equal(state.cProbe, undefined);
    assert.equal(loads.some(name => /root-Lazy_[bc][.]/.test(name)), false);
    if (mode === "concurrent") {
      assert.equal(manifest.backend, "wasm");
      assert.equal(state.lazyFragment, undefined);
      const loadB = registry.load("b");
      await loadB;
      assert.equal(state.lazyFragment, undefined);
      assert.equal(state.cProbe, undefined);
      assert.equal(loads.some(name => name.startsWith("root-Lazy_c.")), false);
      releaseA();
      await startupLoad;
      assert.ok(state.events.indexOf("lazy-b") < state.events.indexOf("lazy"));
      await registry.load("c");
    } else if (mode === "lazy-fail") {
      const failure = await startupLoad.then(
        () => assert.fail("expected a to fail"),
        error => error,
      );
      assert.match(String(failure), /initializer failed after registration/);
      assert.equal(state.lazyFragment, true);
      // Roots run as if they were the rest of the eager code: once root a raises,
      // nothing after it runs, and loading another root raises the same error.
      for (const key of ["b", "c"]) {
        const load = registry.load(key);
        assert.strictEqual(load, registry.load(key));
        const error = await load.then(() => assert.fail(`expected ${key} to fail`), e => e);
        assert.strictEqual(error, failure);
      }
      assert.equal(state.bProbe, undefined);
      assert.equal(state.cProbe, undefined);
      assert.equal(state.events.includes("lazy-b"), false);
      assert.equal(state.events.includes("lazy-c"), false);
      assert.equal(await state.main(), 42);
      assert.strictEqual(startupLoad, registry.load("a"));
    } else {
      await startupLoad;
      assert.equal(state.lazyFragment, true);
      assert.equal(state.bProbe, undefined);
      assert.equal(state.cProbe, undefined);
      assert.equal(loads.some(name => /root-Lazy_[bc][.]/.test(name)), false);
      await state.setState(7);
      const loadB = registry.load("b");
      assert.strictEqual(loadB, registry.load("b"));
      await loadB;
      assert.equal(await state.bProbe(), 707);
      assert.equal(state.cProbe, undefined);
      await state.setState(9);
      await registry.load("c");
      assert.equal(await state.bProbe(), 709);
      assert.equal(await state.cProbe(), 909);
      await state.replaceState(11);
      assert.equal(await state.currentState(), 11);
      assert.equal(await state.bProbe(), 709);
      assert.equal(await state.cProbe(), 909);
    }
    if (mode !== "lazy-fail") {
      assert.equal(await state.main(), 42);
      assert.equal(await state.lazy(), 42);
      assert.equal(await state.sameIdentity(), 1);
      assert.equal(await state.bPrimitive(), 101);
      assert.equal(await state.cPrimitive(), 113);
      assert.equal(await state.bCell(), 13);
      assert.equal(await state.cCell(), 13);
      assert.equal(await state.bLiteral(), 1);
      assert.equal(await state.cLiteral(), 1);
      assert.equal(state.events.filter(event => event === "both").length, 1);
      assert.equal(state.events.filter(event => event === "lazy-b").length, 1);
      assert.equal(state.events.filter(event => event === "lazy-c").length, 1);
      assert.strictEqual(startupLoad, registry.load("a"));
      const exported = await startupLoad;
      assert.equal(await state.callExport(exported), 42);
      assert.strictEqual(exported, await registry.load("a"));
      for (const key of ["b", "c"]) {
        const load = registry.load(key);
        await load;
        assert.strictEqual(load, registry.load(String(key)));
      }
      const unknown = registry.load("missing");
      await assert.rejects(unknown, /Unknown generated import key/);
      assert.strictEqual(unknown, registry.load("missing"));
      if (manifest.backend === "wasm") {
        for (const unit of ["Lazy_root", "Lazy_b", "Lazy_c"]) {
          assert.equal(loads.filter(name => name === "root-" + unit + ".wasm").length, 1);
        }
      }
    }
  }
  const events = [...state.events];
  assert.throws(() => vm.runInThisContext(fs.readFileSync(path.join(payload, "loader.js"), "utf8")), /runtime already installed/);
  assert.deepEqual(state.events, events);
  console.log(`Multi-root ${manifest.backend}: exact per-root bytes, isolated A, shared identities, cached ${mode}: ok`);
})().catch(error => { console.error(error); process.exitCode = 1; });
