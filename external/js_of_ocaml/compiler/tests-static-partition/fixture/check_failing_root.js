// Run a JavaScript split whose root a raises unconditionally.
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");

const directory = process.argv[2];
const raised = f => {
  try {
    f();
  } catch (error) {
    return error;
  }
  assert.fail("expected an exception");
};
for (const failFirst of [true, false]) {
  const context = vm.createContext({ console, partitionTest: { events: [] } });
  const run = name => vm.runInContext(fs.readFileSync(path.join(directory, name), "utf8"), context);
  run("runtime.js");
  run("main.js");
  const api = context.partitionTest;
  if (failFirst) {
    // Roots run as if they were the rest of the eager code: once A raises, nothing
    // after it runs, and loading another root raises the same exception.
    const failure = raised(() => run("root-Lazy_fail.js"));
    assert.equal(raised(() => run("root-Lazy_b.js")), failure);
    assert.equal(api.bProbe, undefined);
    assert.equal(raised(() => run("root-Lazy_c.js")), failure);
    assert.equal(api.cProbe, undefined);
    continue;
  }
  assert.equal(api.bProbe, undefined);
  api.setState(7);
  run("root-Lazy_b.js");
  assert.equal(api.bProbe(), 707);
  assert.equal(api.cProbe, undefined);
  run("root-Lazy_c.js");
  assert.equal(api.cProbe(), 707);
  assert.equal(api.sameIdentity(), 1);
  assert.equal(api.bPrimitive(), 101);
  assert.equal(api.cPrimitive(), 113);
  assert.throws(() => run("root-Lazy_fail.js"));
}
console.log("an unconditional raising initializer stops later roots: ok");
