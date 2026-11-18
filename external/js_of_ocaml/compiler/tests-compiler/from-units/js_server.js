// Runs JavaScript sent by the test in one long-lived Node process, so that each
// chunk sees the state left by the earlier ones: the stubs in runtime.js, the
// generated program, and the roots it registered.
//
// The test writes chunks to stdin, each ending with a NUL byte. The server runs
// each chunk, then prints [__js_done__] so that the test knows the chunk's
// output is complete.
const fs = require("node:fs");
const vm = require("node:vm");
const path = require("node:path");

function run(code, filename) {
  try {
    vm.runInThisContext(code, {filename});
  } catch (exn) {
    // [sharedException] is defined by runtime.js.
    if (exn === sharedException) console.log("Raised Shared.exn");
    else console.log(`Raised unexpected exception: ${exn}`);
  }
}

run(fs.readFileSync(path.join(__dirname, "runtime.js"), "utf8"), "runtime.js");

let pending = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", data => {
  pending += data;
  let end;
  while ((end = pending.indexOf("\0")) >= 0) {
    run(pending.slice(0, end), "chunk");
    pending = pending.slice(end + 1);
    console.log("__js_done__");
  }
});
