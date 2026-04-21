# OxCaml Static Browser Playground

This directory now targets a fully static in-browser OxCaml playground built
against the official OxCaml `5.2.0+ox` source and package universe.

The browser-facing API is:

- `check_string(filename, source) -> string`
- `run_string(filename, source) -> string`
- `utop_string(filename, source) -> string`

The current working path is JS-first:

- `browser_switch_common.ml`, `browser_switch_check.ml`, and
  `browser_switch_run.ml` wrap real OxCaml compiler and toplevel internals
- `browser_switch_js.ml` exposes those wrappers to the browser as
  `globalThis.WebBytecodeJs`
- `build_browser_switch.sh` builds the bytecode bridge and translates it with
  the `js_of_ocaml` binary installed in the official release switch
- `app.js` and `index.html` are a static host that load the translated bundle,
  a staged compressed browser filesystem, and the small runtime shim layer

No local API server is required for the working browser path.

## Build

By default the build script expects the clean official switch:

```text
../oxcaml-5.2.0minus-25/.opam-release-root / switch oxrelease
```

Override that with `OXBROWSER_OPAM_ROOT` and `OXBROWSER_OPAM_SWITCH` if needed.

Required packages in that switch:

- `findlib`
- `js_of_ocaml`
- `js_of_ocaml-toplevel`
- `stdlib_stable`
- `base`
- `core`

From the repo root:

```sh
./experiments/web_bytecode/build_browser_switch.sh
```

The script refuses to build with a non-OxCaml compiler. That is intentional:
the browser artifact must be compiled with an `ocamlc` exposing
`-extension-universe`, otherwise syntax like `stack_`, `local_`, labeled
tuples, and `@ local` silently stops being real OxCaml.

## Serve

Any static file server is enough. For local validation:

```sh
python3 -m http.server 8124
```

Then open:

```text
http://127.0.0.1:8124/experiments/web_bytecode/
```

## Embed on another site

The playground editor can also be embedded into an ordinary static HTML page.
Put one or more `oxcaml` tags in the page and load `oxcaml-embed.js` after
them:

```html
<p>This is normal page content.</p>

<oxcaml utop>
let language = "OCaml";;
let year = 1996 + 1;;
</oxcaml>

<oxcaml>
let square x = x * x

let () =
  Printf.printf "square = %d\n" (square 7)
</oxcaml>

<script src="./oxcaml-embed.js"></script>
```

Supported tag forms:

- `<oxcaml>...</oxcaml>` compiles and runs the whole snippet as a program. It
  also shows inferred types when the snippet typechecks.
- `<oxcaml utop>...</oxcaml>` runs the snippet like a toplevel. Phrases ending
  in `;;` print their inferred type and value, similar to utop.

The embedded editor persists user edits in `localStorage`, keyed by the page
URL, the original tag contents, and the tag's duplicate index when a page has
multiple identical examples. Each editor includes a reset button that restores
the original tag contents and clears that stored edit.

### Files to deploy

Host these files together in one static directory:

- `oxcaml-embed.js`
- `oxcaml-embed-module.js`
- `backend.js`
- `runtime_shims.js`
- `build/web_bytecode_js.bc.js`
- `build/browser_fs_manifest.json`
- `build/browser_fs/**`

The full playground page also needs:

- `index.html`
- `app.js`
- `sample_catalog.js`
- `unsupported_samples.js`

The demo page also needs:

- `embed_demo.html`

`deploy_pages.sh` copies this complete static bundle to the configured GitHub
Pages repository. It is specific to the default target
`~/git/julesjacobs.github.io/misc/oxcaml/playground`, but it is a useful
reference for deploying elsewhere:

```sh
PAGES_REPO=/path/to/site \
PUBLISH=0 \
SKIP_BUILD=1 \
./experiments/web_bytecode/deploy_pages.sh
```

With `PUBLISH=0`, the script only syncs files locally. With `PUBLISH=1`, it
commits and pushes the changed `misc/oxcaml/playground` directory in the Pages
repository.

### Hosting requirements

Static hosting is enough; no API server is required. The files should normally
be served from the same origin and directory as the page that uses them. If the
embed files are served from a different origin, that origin must allow browser
module imports and `fetch` requests for `backend.js`, `runtime_shims.js`,
`build/web_bytecode_js.bc.js`, `build/browser_fs_manifest.json`, and every file
under `build/browser_fs/`.

The wrapper script resolves `oxcaml-embed-module.js` relative to
`oxcaml-embed.js`, and the module resolves `backend.js` and the build assets
relative to itself. For a custom layout, pass an explicit module URL:

```html
<script
  src="https://example.com/oxcaml/oxcaml-embed.js"
  data-module-src="https://example.com/oxcaml/oxcaml-embed-module.js">
</script>
```

Keep `backend.js`, `runtime_shims.js`, and the `build/` directory at the
locations expected by `oxcaml-embed-module.js` and `backend.js`, or update their
relative asset paths before publishing.

### Caching

`oxcaml-embed.js` includes a version query when it imports
`oxcaml-embed-module.js`, and `oxcaml-embed-module.js` includes a version query
when it imports `backend.js`. If the outer script is cached aggressively by the
embedding site or CDN, version the script URL in the page as well:

```html
<script src="./oxcaml-embed.js?v=20260421-int-overflow-guard"></script>
```

When publishing a new bundle, keep the static assets in sync. Mixing a new
`oxcaml-embed-module.js` with an old `backend.js` or old `build/` directory can
leave the editor unable to load the compiler.

## Verified Static Browser Cases

Validated in-browser through the static host and direct API probes:

- `check_string("hello.ml", "let x = 1\n")` returns `""`
- type errors return formatted diagnostics
- `run_string("run.ml", "print_endline \"hi\";;\n")` returns `hi\n`
- runtime exceptions return formatted exception strings
- `stack_` examples run successfully
- `local_` argument examples run successfully
- `@ local` return examples run successfully
- labeled tuple examples run successfully
- incorrect locality returns the expected OxCaml diagnostic
- `open Base` works when the curated package preload succeeds
- `open Core` works when the curated package preload succeeds
- `Stdlib_stable.Iarray` works when the curated package preload succeeds

## Notes

- `runtime_shims.js` provides a small set of missing runtime primitives needed
  by the translated compiler under the browser JS runtime.
- Package and interface files are staged as compressed static assets and loaded
  into the `js_of_ocaml` pseudo-filesystem at startup; the older single-file
  `browser_fs.js` path is intentionally no longer the primary transport.
- The browser build is intentionally tied to the official OxCaml release switch
  rather than the earlier patched local 5.4 bootstrap path.
- The local server-backed playground remains useful as a development harness,
  but it is no longer the primary browser path.
