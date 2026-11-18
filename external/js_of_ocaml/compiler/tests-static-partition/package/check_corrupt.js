// Corrupt one root of a generated package: its load fails, and other roots still load.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");

const entry = path.resolve(process.argv[2], "entry.js");
const assets = path.resolve(process.argv[2], "assets");
const mode = process.argv[3];
const manifest = JSON.parse(fs.readFileSync(path.join(assets, "bundle-manifest.json")));
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "corrupt-multi-package-"));
fs.copyFileSync(entry, path.join(scratch, path.basename(entry)));
const copy = path.join(scratch, path.basename(assets));
fs.cpSync(assets, copy, { recursive: true });
// Build outputs are read-only.
const makeWritable = file => {
  fs.chmodSync(file, fs.statSync(file).isDirectory() ? 0o755 : 0o644);
  if (fs.statSync(file).isDirectory()) for (const name of fs.readdirSync(file)) makeWritable(path.join(file, name));
};
makeWritable(copy);
const root = manifest.roots.find(root => root.key === "a");
const selected = root.assets.find(name => mode === "fragment-parse" ? name.endsWith(".mjs") : manifest.backend === "wasm" ? name.endsWith(".wasm") : name.endsWith(".mjs"));
const corrupt = path.join(copy, selected);
if (mode === "fetch") fs.unlinkSync(corrupt);
else if (corrupt.endsWith(".wasm")) fs.writeFileSync(corrupt, Buffer.from("not WebAssembly"));
else fs.writeFileSync(corrupt, "#!/usr/bin/env node\nexport default } invalid JavaScript");

globalThis.partitionTest = { events: [] };
(async () => {
  try {
    require(path.join(scratch, path.basename(entry)));
    const registry = globalThis.__jsoo_imports_v2;
    await registry.ready;
    const first = registry.load("a");
    await assert.rejects(first, mode === "fetch" ? /ENOENT|Cannot find module/ : mode === "fragment-parse" || manifest.backend === "javascript" ? SyntaxError : WebAssembly.CompileError);
    assert.strictEqual(first, registry.load("a"));
    assert.equal(globalThis.partitionTest.lazyFragment, undefined);
    await Promise.all([registry.load("b"), registry.load("c")]);
    assert.equal(await globalThis.partitionTest.bPrimitive(), 101);
    assert.equal(await globalThis.partitionTest.cPrimitive(), 113);
    assert.equal(await globalThis.partitionTest.sameIdentity(), 1);
    console.log(`Multi-root ${manifest.backend}: cached ${mode} failure leaves other roots usable: ok`);
  } finally {
    fs.rmSync(scratch, { recursive: true, force: true });
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
