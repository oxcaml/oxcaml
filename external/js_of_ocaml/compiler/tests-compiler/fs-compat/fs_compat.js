const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");

const filesystem = fs.readFileSync(process.argv[2], "utf8");
const runtime = fs.readFileSync(process.argv[3], "utf8");
const files = [
  { name: "/static/file1", content: "This is file 1" },
  { name: "/static/dir/file2", content: "This is file 2" },
];

for (const api of ["jsoo_create_file", "caml_create_file"]) {
  const created = [];
  const context = vm.createContext({
    [api]: (name, content) => created.push({ name, content }),
  });
  vm.runInContext(filesystem, context);
  assert.deepEqual(created, files);
}

const current = [];
const both = vm.createContext({
  jsoo_create_file: (name, content) => current.push({ name, content }),
  caml_create_file: () => assert.fail("prefer the current API"),
});
vm.runInContext(filesystem, both);
assert.deepEqual(current, files);

function read(context, name) {
  const runtime = context.jsoo_runtime;
  return runtime.caml_jsbytes_of_string(runtime.caml_read_file_content(name));
}

const legacyFilesystem = `
  for (const {name, content} of ${JSON.stringify(files)}) {
    if (globalThis.caml_create_file) {
      globalThis.caml_create_file(name, content);
    } else {
      if (!globalThis.caml_fs_tmp) globalThis.caml_fs_tmp = [];
      globalThis.caml_fs_tmp.push({name, content});
    }
  }
`;

for (const script of [filesystem, legacyFilesystem]) {
  const context = vm.createContext({});
  vm.runInContext(script, context);
  vm.runInContext(runtime, context);
  for (const { name, content } of files) {
    assert.equal(read(context, name), content);
  }
  assert.equal(context.jsoo_fs_tmp.length, 0);
  assert.equal(context.caml_fs_tmp.length, 0);
}

const mixed = vm.createContext({
  jsoo_fs_tmp: [files[0]],
  caml_fs_tmp: [files[1]],
});
vm.runInContext(runtime, mixed);
for (const { name, content } of files) {
  assert.equal(read(mixed, name), content);
}
assert.equal(mixed.jsoo_fs_tmp.length, 0);
assert.equal(mixed.caml_fs_tmp.length, 0);

for (const script of [filesystem, legacyFilesystem]) {
  const after = vm.createContext({});
  vm.runInContext(runtime, after);
  vm.runInContext(script, after);
  for (const { name, content } of files) {
    assert.equal(read(after, name), content);
  }
}
