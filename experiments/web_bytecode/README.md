# OxCaml Static Browser Playground

This directory now targets a fully static in-browser OxCaml playground built
against the official OxCaml `5.2.0+ox` source and package universe.

The browser-facing API is:

- `check_string(filename, source) -> string`
- `run_string(filename, source) -> string`

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
