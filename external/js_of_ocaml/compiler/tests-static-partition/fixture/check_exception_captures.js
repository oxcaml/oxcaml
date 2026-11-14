// Compile small programs whose shared value comes out of an exception handler, split
// them, and check that the root sees the eager value instead of recomputing it.
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const vm = require("node:vm");
const { spawnSync } = require("node:child_process");
const { pathToFileURL } = require("node:url");

const jsOfOcaml = path.resolve(process.argv[2]);
const wasmOfOcaml = path.resolve(process.argv[3]);
const runtimeStubs = path.resolve("runtime_stubs.js");
const runtimeStubsWat = path.resolve("runtime_stubs.wat");
const ocamlc = process.env.OCAMLC || "ocamlc";
const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "partition-exception-captures-"));

function run(command, args, cwd) {
  const result = spawnSync("sh", ["-c", `${command} "$@"`, "sh", ...args], { cwd, encoding: "utf8" });
  assert.equal(result.error, undefined);
  assert.equal(result.status, 0, `${command} ${args.join(" ")}\n${result.stderr}`);
}

async function check(name, type, expression) {
  const directory = path.join(scratch, name);
  fs.mkdirSync(directory);
  const sources = {
    shared: {
      mli: `val x : ${type}\n`,
      ml: `external probe : unit -> int = "partition_test_b_only"\nexternal raise_ : exn -> 'a = "%raise"\nexception Failed\nlet x = ${expression}\n`,
    },
    main: {
      mli: "",
      ml: `external expose : string -> (unit -> ${type}) -> unit = "partition_test_expose"\nlet () = expose "mainValue" (fun () -> Shared.x)\n`,
    },
    lazy_root: {
      mli: "",
      ml: `external expose : string -> (unit -> ${type}) -> unit = "partition_test_expose"\nlet () = expose "lazyValue" (fun () -> Shared.x)\n`,
    },
  };
  for (const [unit, source] of Object.entries(sources)) {
    fs.writeFileSync(path.join(directory, `${unit}.mli`), source.mli);
    fs.writeFileSync(path.join(directory, `${unit}.ml`), source.ml);
    run(ocamlc, ["-c", `${unit}.mli`], directory);
    run(ocamlc, ["-g", "-c", `${unit}.ml`], directory);
  }
  const inputs = ["shared.cmo", "main.cmo", "lazy_root.cmo"];
  for (const strategy of ["modules", "late"]) {
    run(jsOfOcaml, [
      "split", "--strategy", strategy, "--opt", "3", "--effects", "disabled",
      ...inputs, runtimeStubs, "--main", "Main", "--root", "Lazy_root", "-o", strategy,
    ], directory);
    const output = path.join(directory, strategy);
    const context = vm.createContext({ console, partitionTest: { events: [] } });
    const evaluate = name => vm.runInContext(fs.readFileSync(path.join(output, name), "utf8"), context);
    evaluate("runtime.js");
    const probe = context.jsoo_runtime.partition_test_b_only;
    assert.equal(typeof probe, "function");
    let calls = 0;
    context.jsoo_runtime.partition_test_b_only = function (...args) {
      calls++;
      return probe.apply(this, args);
    };
    evaluate("main.js");
    assert.equal(calls, 1);
    assert.equal(context.partitionTest.lazyValue, undefined);
    const eager = context.partitionTest.mainValue();
    evaluate("root-Lazy_root.js");
    assert.equal(calls, 1, "lazy initialization must not repeat the eager probe");
    const deferred = context.partitionTest.lazyValue();
    assert.strictEqual(eager, deferred);
    if (type === "int") assert.equal(eager, 101);
    else assert.ok(eager && typeof eager === "object");
    console.log(`${name}/${strategy}: source compilation, captured value/identity, one eager probe: ok`);
  }
  run(wasmOfOcaml, [
    "split", "--strategy", "late", "--opt", "3", "--effects", "jspi",
    ...inputs, runtimeStubs, runtimeStubsWat, "--main", "Main", "--root", "Lazy_root", "-o", "wasm-late",
  ], directory);
  const output = path.join(directory, "wasm-late");
  for (const [wasm, unit] of [["main.wasm", "partition_main"], ["root-Lazy_root.wasm", "partition_lazy_Lazy_root"], ["startup.wasm", null]]) {
    const module = await WebAssembly.compile(fs.readFileSync(path.join(output, wasm)));
    if (unit !== null) {
      assert.ok(WebAssembly.Module.exports(module).some(item => item.name === `${unit}.init`));
      const fragmentsFile = wasm.replace(/\.wasm$/, ".fragments.mjs");
      const fragments = (await import(pathToFileURL(path.join(output, fragmentsFile)).href)).default;
      assert.ok(fragments && typeof fragments === "object");
      assert.ok(Object.keys(fragments).every(key => key === `${unit}.fragments`));
    }
  }
  console.log(`${name}/wasm-late: producer modules and fragments validated (not executed): ok`);
}

(async () => {
  try {
    await check("one-success-try-arm", "int", "try probe () with _ -> raise_ Failed");
    await check("captured-handler-binder", "exn", "try let _ = probe () in raise_ Failed with exn -> exn");
  } finally {
    fs.rmSync(scratch, { recursive: true, force: true });
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
