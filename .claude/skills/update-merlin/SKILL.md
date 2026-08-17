---
name: update-merlin
description: Update/merge the vendored Merlin after a compiler frontend change (parsing/, typing/, file_formats/, utils/), fix a failing Merlin-sync CI check, or teach Merlin about a new compiler flag.
allowed-tools: Bash(external/merlin/scripts/import-ocaml-source.sh)
---

# Updating Merlin After Compiler Frontend Changes

Merlin (`external/merlin/`) vendors the compiler's frontend. Any frontend change — approximately `parsing/`, `typing/`, and the files in `file_formats/` and `utils/` they use — must be imported into Merlin; a CI check verifies this. Import by running `external/merlin/scripts/import-ocaml-source.sh` (never hand-merge compiler changes into `external/merlin`, and never manually modify `external/merlin/upstream/ocaml_flambda` except for `external/merlin/upstream/ocaml_flambda/.gitattributes`), then get `make merlin-test` passing. This is what the user is asking you to do if they say something like "Update Merlin", "Fix Merlin", or "Merge compiler/frontend/typing/type-checker changes into Merlin". New compiler flags (even backend-only ones) also require a Merlin update. Don't do anything before reading the full documentation: `external/merlin/HACKING.jst.md`.
