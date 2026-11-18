const assert = require('node:assert/strict');
const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const path = require('node:path');
const [root, buildDir, target] = process.argv.slice(2);
process.chdir(buildDir);
const lib = path.join(root, 'stdlib');
const tool = target + '_of_ocaml';
const env = { ...process.env, OCAMLLIB: lib,
  ['OXCAML_' + tool.toUpperCase()]: path.join(root, tool) };

// Both libraries ship an int.ml. Neither should resolve to one in the cwd.
// A whole program reads the installed sources. A separate link uses the text
// in the archives of the libraries: it is not given the installed sources.
// Both name them as the libraries record.
fs.writeFileSync('int.ml', '(* not a distribution source *)\n');
for (const mode of ['whole', 'separate']) {
  const flags = mode === 'whole'
    ? ['-link-jsir', '-jsoo-opt-compile', '--source-map']
    : ['-jsoo-opt-link', '--source-map'];
  const sources = mode === 'whole' ? {}
    : { BUILD_PATH_PREFIX_MAP: '/ocaml/stdlib/=/nonexistent/' };
  execFileSync(path.join(root, 'ocamlopt.opt'), [
    '-target', tool, '-g', ...flags, '-I', '+stdlib_stable',
    'stdlib_stable.cmxa-' + target, '-o', mode + '.js',
    'stdlib_sourcemap.ml',
  ], { env: { ...env, ...sources }, stdio: 'inherit' });

  const dir = mode + '.assets';
  const maps = target === 'js' ? [mode + '.map']
    : fs.readdirSync(dir).filter(f => f.endsWith('.map'))
      .map(f => path.join(dir, f));
  const expected = ['int.ml', 'stdlib_stable/int.ml'];
  const seen = new Set();
  for (const file of maps) {
    const map = JSON.parse(fs.readFileSync(file, 'utf8'));
    for (const section of map.sections || [{ map }]) {
      for (const name of expected) {
        const i = section.map.sources.indexOf('/ocaml/stdlib/' + name);
        if (i !== -1) {
          const text = fs.readFileSync(path.join(lib, name), 'utf8');
          assert.equal(section.map.sourcesContent[i], text, name);
          seen.add(name);
        }
      }
    }
  }
  assert.deepEqual([...seen].sort(), expected, mode);
}
