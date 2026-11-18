// Load a generated package with one root in Node, and in a simulated browser.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const entry = path.resolve(process.argv[2], "entry.js");
const assets = path.resolve(process.argv[2], "assets");
const mode = process.argv[3];
const manifest = JSON.parse(fs.readFileSync(path.join(assets, "bundle-manifest.json")));
assert.equal(manifest.format, 1);
assert.match(manifest.identity, /^[a-f0-9]{32}$/);
assert.equal(manifest.entry_bytes, fs.statSync(entry).size);
const payload = path.join(assets, manifest.identity);
assert.deepEqual(fs.readdirSync(assets).sort(), [manifest.identity, "bundle-manifest.json"].sort());
assert.deepEqual(fs.readdirSync(payload).sort(), ["bundle-manifest.json", ...manifest.assets.map(asset => path.basename(asset.path))].sort());
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

const entrySource = fs.readFileSync(entry, "utf8");
const scripts = [];
const browser = src => vm.createContext({
  URL,
  location: { origin: "https://example.test" },
  document: {
    currentScript: { src },
    baseURI: "https://untrusted.test/",
    createElement: () => ({}),
    head: { appendChild: script => scripts.push(script) },
  },
});
vm.runInContext(entrySource, browser("https://example.test/app/entry.js"));
assert.equal(scripts[0].src, `https://example.test/app/${path.basename(assets)}/${manifest.identity}/loader.js`);
for (const src of ["https://untrusted.test/entry.js", "https://example.test/entry.js?other", "https://example.test/entry.js#other", "https://example.test/bad%20path/entry.js"]) {
  assert.throws(() => vm.runInContext(entrySource, browser(src)), /normalized same-origin/);
}
assert.equal(scripts.length, 1);

let startupLoad;
let fragment;
globalThis.partitionTest = { events: [], failAfterRegistration: mode === "lazy-fail" };
Object.defineProperty(globalThis.partitionTest, "mainFragment", {
  get() { return fragment; },
  set(value) {
    fragment = value;
    startupLoad = globalThis.__jsoo_imports_v2.load("Lazy_root");
    assert.strictEqual(startupLoad, globalThis.__jsoo_imports_v2.load("Lazy_root"));
    startupLoad.catch(() => {});
    assert.equal(globalThis.partitionTest.lazyFragment, undefined);
    if (mode === "eager-fail") throw new Error("eager initialization failed");
  },
});

(async () => {
  try {
    require(entry);
  } catch (error) {
    if (mode !== "eager-fail") throw error;
    assert.match(String(error), /eager initialization failed/);
  }
  const registry = globalThis.__jsoo_imports_v2;
  assert.ok(Object.isFrozen(registry));
  assert.throws(() => { registry.load = () => Promise.resolve(); }, TypeError);
  if (mode === "eager-fail") {
    await assert.rejects(registry.ready, /eager initialization failed/);
    assert.ok(startupLoad);
    await assert.rejects(startupLoad, /eager initialization failed/);
    assert.strictEqual(startupLoad, registry.load("Lazy_root"));
    assert.deepEqual(globalThis.partitionTest.events, ["shared", "forced", "main"]);
    assert.equal(globalThis.partitionTest.lazyFragment, undefined);
  } else {
    await registry.ready;
    assert.ok(startupLoad);
    assert.strictEqual(startupLoad, registry.load("Lazy_root"));
    if (mode === "lazy-fail") await assert.rejects(startupLoad, /initializer failed after registration/);
    else {
      const exported = await startupLoad;
      assert.equal(await globalThis.partitionTest.callExport(exported), 42);
      assert.strictEqual(exported, await registry.load("Lazy_root"));
    }
    assert.deepEqual(globalThis.partitionTest.events, ["shared", "forced", "main", "unique", "lazy"]);
    assert.equal(globalThis.partitionTest.lazyFragment, true);
    assert.equal(await globalThis.partitionTest.main(), 42);
    assert.equal(await globalThis.partitionTest.lazy(), 42);
    assert.equal(await globalThis.partitionTest.bucketCount(), 26);
    assert.strictEqual(startupLoad, registry.load("Lazy_root"));
    const missing = registry.load("missing");
    await assert.rejects(missing, /Unknown generated import key/);
    assert.strictEqual(missing, registry.load("missing"));
  }
  const events = [...globalThis.partitionTest.events];
  assert.throws(() => vm.runInThisContext(fs.readFileSync(path.join(payload, "loader.js"), "utf8")), /runtime already installed/);
  assert.deepEqual(globalThis.partitionTest.events, events);
  console.log(`Generated ${manifest.backend} package: immutable assets, exact bytes, pinned URLs, startup race, cached ${mode}: ok`);
})().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
