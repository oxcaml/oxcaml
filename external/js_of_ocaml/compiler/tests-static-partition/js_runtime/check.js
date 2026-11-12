const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");

const source = fs.readFileSync("shared.runtime.js", "utf8");
assert.doesNotMatch(source, /UNUSED_RUNTIME_PRIMITIVE_SENTINEL/);
const context = vm.createContext({ console });
vm.runInContext(source, context, { filename: "shared.runtime.js" });
const runtime = context.jsoo_runtime;
assert.equal(context.runtimeProbeInitializations, 1);
assert.equal(Object.hasOwn(runtime, "runtime_probe_unused"), false);
assert.equal(Object.hasOwn(runtime, "runtime_probe_transitive"), false);
assert.equal(Object.hasOwn(runtime, "runtime_probe_state"), false);
const eager = runtime.runtime_probe_main();
const lazy = runtime.runtime_probe_lazy();
assert.strictEqual(eager, lazy);
assert.strictEqual(lazy, runtime.runtime_probe_alias());
assert.strictEqual(context.runtimeProbeAlways, eager);
eager.count = 42;
assert.equal(lazy.count, 42);

const failure = runtime.caml_get_global("predef:Failure");
assert.ok(failure);
assert.equal(runtime.caml_jsstring_of_string(failure[1]), "Failure");
assert.strictEqual(runtime.caml_get_global("predef:Failure"), failure);
for (const name of ["FORCE_DROP_INLINE_TEST", "FORCE_DROP_BENCH"]) {
  const value = runtime.caml_sys_getenv(runtime.caml_string_of_jsbytes(name));
  assert.equal(runtime.caml_jsstring_of_string(value), "true");
}
for (const [virtualName, physicalName] of [["hg_version.out", "revision.txt"], ["build_info.sexp", "build_info.sexp"]]) {
  const contents = runtime.caml_read_file_content(`/static/${virtualName}`);
  assert.equal(runtime.caml_jsstring_of_string(contents), fs.readFileSync(physicalName, "utf8"));
}
console.log("Pruned JS runtime: union roots, aliases, shared state, required initializer, predefs, static env, embedded files: ok");
