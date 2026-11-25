# Experimental features

This document describes how experimental features (those behind an
`-experimental-*` flag) are added to the compiler before they have been fully
reviewed, how their review state is recorded, and what is required before one
can be enabled by default. Most are optimizations, but anything self-contained
enough to sit behind a flag qualifies, for example new debug-info emission.

An experimental feature may be written by a person, by an AI model, or both.
The process is the same.

The metadata checker, status table generation, CI check, build-time registry,
and native compiler's per-feature flags, universe selection and testing preset
are implemented. Merlin handling of generated per-feature switches remains
planned for a follow-up.

## Code layout

- Each experimental feature lives in its own directory, wherever it belongs
  in the tree, named after the feature. New code goes there. The directory
  is never moved: promotion changes the flag and the headers, not the path.
- Changes to existing code are kept to the minimum needed to hook the
  feature in, and are reviewed to the normal standard. So is everything that
  runs with the flag off: module initialisers, registrations with other
  parts of the compiler, and the guard itself. A flag is not an isolation
  boundary; the flag-off path must be safe.
- The directory is registered by a `SCRUTINY` file in it (format below).
  That file is the only registration: the flag, the checker and the status
  table are all derived from the set of `SCRUTINY` files in the tree, so
  there is no central list to forget. Registered directories do not nest.

Examples:

```
backend/ssa/loop/SCRUTINY
middle_end/flambda2/reaper/escape/SCRUTINY
backend/debug/dwarf/call_sites/SCRUTINY
```

## Flags

- Every experimental feature is off by default and enabled by a flag with the
  common prefix `-experimental-<name>`, where `<name>` comes from the
  directory's `SCRUTINY` file. The names and review levels come from the
  generated registry described below.
- `-experimental-universe=<level>` replaces the selection with every registered
  feature whose review level (below) is at least `<level>`; `none` disables all
  registered features. Options apply in command-line order: a later universe
  replaces earlier choices, and later `-experimental-<name>` or
  `-no-experimental-<name>` flags override it. Ordinary compiler flags are
  unaffected. Both `-experimental-universe=<level>` and
  `-experimental-universe <level>` are accepted.
- The default test suite runs with every experimental flag off, unless
  explicitly overridden.
- When a feature is promoted (enabled by default), its flag is renamed
  without the prefix or removed entirely. The prefix therefore always means
  "not yet promoted". Options that have become invariants are removed.

The options are available in `ocamlopt` and the native toplevel. In `ocamlopt`,
they also support `OCAMLPARAM`. `experimental-universe=<level>` selects a
universe, `experimental-<name>=1` enables a feature, and `experimental-<name>=0`
disables it. `no-experimental-<name>=1` disables the feature and `=0` enables
it. Parameters before `_` apply before command-line options; parameters after
`_` apply before each compilation or link, overriding command-line choices.
Invalid boolean values warn without changing the selection; unknown parameter
names use the compiler's existing discarded-parameter warning.

Feature code queries the typed selection through
`Oxcaml_flags.Experimental.is_enabled`. For example, once a feature is
registered as `reaper-escape`, its guard can use:

```ocaml
Oxcaml_flags.Experimental.is_enabled
  Experimental_feature_registry.Feature_reaper_escape
```

Selecting a feature does not implicitly enable prerequisite passes, such as
the reaper, or change target capabilities.

### Testing preset

`-test-optimizations` (formerly `-experimental-optimizations`) enables a
bundle of already-reviewed codegen options. Those options have their own
flags outside the `-experimental-*` namespace and do not need `SCRUTINY`
registrations or review headers. Being off by default does not itself put a
feature under this workflow.

The flag enables that bundle, including its register-allocation parameter
settings, then selects `-experimental-universe=ai-reviewed`. These remain
separate operations: selecting a universe affects only registered features.
In particular, `-experimental-universe=none` after the testing preset disables
registered features but leaves the older bundle enabled. Individual flags
retain their normal command-line ordering: a later feature flag overrides the
preset, and a later preset replaces earlier feature selections.

The preset is intended for CI, regression tests and fuzzing, not as an
optimization level. Registered features need not be optimizations. Tests using
the preset opt into new features as they join the universe, so their expected
assembly may change. Tests that require a fixed configuration should use
explicit flags instead.

CI enables the same preset with `OCAMLPARAM=_,test-optimizations=1`.

## Build-time registry

A root dune rule depends on `(glob_files_rec SCRUTINY)` and runs
`tools/gen_experimental_features.ml` to generate
`experimental_feature_registry.ml` and `.mli` in the build directory. Adding,
changing or removing a registration regenerates the module. The generator
uses only the OCaml standard library, not compiler libraries or Python.

`Experimental_feature_registry`, in `oxcaml_common`, exposes:

- a type `t` with one constructor per registered feature;
- `all : t list`, ordered by flag name;
- `flag_name : t -> string`, the suffix from the registration;
- `review_level : t -> review_level`, with `Ai_reviewed` and `Human_reviewed`.

Constructors have a `Feature_` prefix, with `-` replaced by `_` and `.` by
`'`. For example, `reaper-escape` becomes `Feature_reaper_escape`. This also
supports names beginning with a digit and keeps distinct flag names distinct.
With no registrations, `t` is empty and `all` is `[]`.

The generator rejects malformed registrations and duplicate flag names before
writing either output. Source-header checks and verification of declared
review levels remain the responsibility of `scrutiny/check.py`. The registry
contains metadata only; `Oxcaml_flags.Experimental` owns the selection state,
and `Oxcaml_args` derives option names and actions from the registry.

The cram tests in `scrutiny/generate_registry.t` and `scrutiny/selection.t`
run under `make runtest`. Selection tests compile the production flag modules
against a synthetic registry, without registering real compiler features.
To run these tests with the bootstrap compiler:

```sh
make boot-runtest runtest_targets=@scrutiny/runtest
```

## Files

All files are plain text, one entry per line, with `#` starting a comment and
blank lines ignored. Tokens match `[a-z0-9][a-z0-9.-]*`.

`SCRUTINY`, in each experimental feature's directory:

```
flag  ssa-loop-opts
level ai-reviewed
```

Both lines are required. `flag` gives the suffix of the feature's
`-experimental-` flag. `level` is the feature's review level (next section);
it is written here so the build can read it without parsing every source
file, and the checker verifies it against the headers. Duplicate and unknown
keys are rejected.

`scrutiny/` at the repository root holds everything else: this document
(`scrutiny/README.md`), the model allowlist (`scrutiny/models`), the
checker (`scrutiny/check.py`, requiring Python 3.6 or later, only standard
library modules, and the `git` executable) and the generated
`scrutiny/STATUS.md`.

`scrutiny/models` is the allowlist of AI models. A line `<model>` allows that
model in `OxCaml-Written-By` (See per-file header section below); a line
`<model>:<effort>` allows that exact pair in `OxCaml-Review-Status`.  Entries
are matched verbatim, so the file also fixes the spelling of model names and
effort levels. An entry is active unless followed by `retired`. Retired entries
remain valid where a header already has them, but a pull request may not add
one to a header, so old files keep an accurate record while new reviews use
current models.

Model names carry a version, as in the example below, so that a header says
which generation of a model wrote or reviewed a file and retirement can tell
generations apart.

Model names and effort levels each follow the token syntax above. Each entry
is either `<model>` or `<model>:<effort>`, optionally followed by `retired`;
other fields or retirement markers are rejected. Duplicate entries are
rejected, including an entry listed as both active and retired.

```
# writing
example-model-4
example-model-3            retired
# reviewing
example-model-4:high
example-model-4:max
example-model-3:high       retired
```

## Review levels and universes

Every experimental feature has a review level, the weakest review status of
any `.ml`/`.mli`/`.v` file in its directory, computed from the headers:

- `ai-reviewed`: every file has been reviewed by an approved model. This is
  the minimum for merging, so every experimental feature is at least here.
  Each file's change was also approved by a maintainer when its pull request
  merged, but that approval is not a full review.
- `human-reviewed`: every file additionally has `human` in its review status.

The levels are ordered by trust: `human-reviewed` is stricter than
`ai-reviewed`. A universe is the set of features at or above a level, so the
`human-reviewed` universe is a subset of the `ai-reviewed` universe, which
contains every experimental feature.

```
-experimental-universe=none            # default: no experimental features
-experimental-universe=human-reviewed  # only features every file of which
                                       # a maintainer has fully reviewed
-experimental-universe=ai-reviewed     # every experimental feature
```

Levels are coarse by design. Finer distinctions (which model, what effort,
provenance) stay in the per-file headers and `STATUS.md`; a universe only
asks whether a human has read all of the code.

## Requirements for merging

These apply to the pull request that introduces a feature and to every later
pull request that touches a registered directory.

- Review of the changed `.ml`/`.mli`/`.v` files by an approved AI model (listed in
  `scrutiny/models`), at a reasoning effort recorded in the header.
- Ordinary pull-request approval by a maintainer, having looked over the
  patch for anything harmful. This is not a full review and is not recorded
  in the header.
- Ordinary review of everything other than the files named in the header:
  the integration into existing code, build files, C code and scripts.
- Tests exercising the feature under its flag.

## Per-file header

Every `.ml`/`.mli`/`.v` file under a registered directory starts with a header
comment:

```
(*= OxCaml-Written-By: ai:<model>
    OxCaml-Review-Status: ai:<model>:<effort> *)
```

The comment opens with `(*=`, which makes ocamlformat leave it verbatim.
Several directories enable `wrap-comments`, under which a plain `(*` comment
would be reflowed and the two fields joined onto one line, so the checker
requires `(*=`. Each field is written on a single line.

Both fields are nonempty comma-separated sets drawn from a fixed vocabulary.

`OxCaml-Written-By` records the file's origin, not its current maintainers
(use `git blame` for those). Values:

- `ai:<model>`: generated by the named model, which must appear in
  `scrutiny/models`.
- `human`: written by hand.

A file drafted by a model and substantially rewritten by hand lists both.

`OxCaml-Review-Status` records the reviews the current contents have
received. Values:

- `ai:<model>:<effort>`: reviewed by the named model at the given effort;
  the pair must appear in `scrutiny/models`
- `human`: fully reviewed by a maintainer, to the usual standard

At least one `ai:` entry is required; there is no "unreviewed" state, because
nothing unreviewed is merged.

An `ai:` entry says more than that a review took place: it is added once the
findings of that review have been addressed. The person who asked for the
review triages and evaluates the findings, and asks the author for changes
where needed. A finding is addressed once the change is made, or once that
person has judged that none is needed.

The header contains no commit hashes, dates or PR numbers: squash merging
makes branch hashes meaningless, and `git blame` on the status line already
gives the commit and PR that last changed it.

The header describes the current contents of the file. A pull request that
changes a registered file must have the change AI-reviewed before merging and
must update the header if the set of reviews changed (for example, dropping
`human` from a file that no longer has full review). Keeping the header
accurate is the responsibility of the person asking the AI model to review; the
checker cannot tell whether a review covers the current contents (see Future).

The header is removed when the feature is promoted.

## The checker

A CI job runs `scrutiny/check.py` on every pull request. The checker uses Git
to enumerate tracked files and compare headers against the pull request's
base. Untracked files are not checked.

For each source file, the checker reads through the first comment containing
an `OxCaml-` header line, then stops at the end of that comment. It checks the
whole stanza; later stanzas are ignored. Comments and strings are recognised
lexically, by OCaml rules in `.ml`/`.mli` files and by Rocq rules in `.v`
files, so header text inside a string literal does not count.

It enforces:

- `scrutiny/models` follows the format above, with valid model and effort
  tokens, no duplicate entries, and no unknown retirement markers or extra
  fields.
- Every `SCRUTINY` file parses and has exactly one `flag` and one `level`
  line, with no unknown keys; no two give the same name; no name is `all`,
  `universe`, or `optimizations`; neither generated option
  (`-experimental-<name>` or `-no-experimental-<name>`) collides with an
  existing driver option; no `SCRUTINY` lies under another registered
  directory.
- Each registered directory contains at least one tracked `.ml`/`.mli`/`.v`
  file. Its `level` is `ai-reviewed` or `human-reviewed` and equals the level
  computed from the headers of the files under it.
- Every `.ml`/`.mli`/`.v` file under a directory containing `SCRUTINY` has both
  header lines, in a comment opened with `(*=`, with nonempty sets of values
  from the vocabulary, models from `scrutiny/models`, and at least one `ai:`
  review. Values the pull request adds to a header field use only active
  models.
- No `.ml`/`.mli`/`.v` file outside such a directory contains an `OxCaml-` header
  line, whether its comment opens with `(*=` or `(*`. This catches code that
  copies the convention without being registered.
- `scrutiny/STATUS.md`, a table of every feature, its files and their review
  state, matches what the checker generates from the headers. Regenerate it
  with `python3 scrutiny/check.py --update` when it is out of date.

Run the checker from anywhere in the checkout, passing the pull request's
base commit explicitly in CI:

```sh
python3 scrutiny/check.py --base <base-commit>
python3 scrutiny/check.py --base <base-commit> --update
```

`--base` is the exact comparison commit, not a branch whose merge base the
checker computes. For a PR-head checkout, supply the PR's merge base; for a
merge-commit checkout, supply the pre-merge target commit. Without `--base`,
the checker warns that retirement checks are skipped; all current-tree
checks still run.

Retirement checks compare the values of each header field with those of the
same field in the file's base version, following Git-detected renames. A
value naming a retired entry can stay if the base version had it, whatever
else changes: the file's code, the field's other values, their order, or the
layout of the comment. Otherwise it is rejected; that includes every retired
entry in a new file, and in a file whose base header was missing or invalid.
This does not establish whether the code has been re-reviewed.

Driver collisions are checked against experimental option string literals
in tracked `.ml` files under `driver/`, without building the compiler.
Dynamically constructed option names are not evaluated.

`--update` only regenerates `scrutiny/STATUS.md`, in a deterministic order,
and only if metadata validation succeeds. The checker never repairs headers
or review levels automatically, though it may suggest fixes in diagnostics.
Failures produce diagnostics and a nonzero exit status.

The unit tests are in `scrutiny/test_check.py` and use temporary Git
repositories. Run them with:

```sh
python3 -m unittest discover -s scrutiny -v
```

## Promotion

A feature may be enabled by default once:

- consensus has been reached that the scrutiny for each `.ml`/`.mli`/`.v` file in
  its directory is appropriate (this would normally mean having `human` in all
  relevant review stanzas); and
- it has been sufficiently exercised at scale; and
- its flag has been renamed or removed, and its `SCRUTINY` file and headers
  deleted. Deleting `SCRUTINY` is what removes the `-experimental-` flag.

## Future

- A way for the checker to detect a file edited after its last review. Header
  changes alone cannot express this reliably (a repeat review by the same
  model leaves the header unchanged); pull-request review records are the
  likely source.
- An `OxCaml-Validation:` header line (e.g. `differential`, `validator`,
  `proof`) once there are validation artefacts to point at.
- Randomised testing of flag subsets in CI, to cover interactions between
  experimental features.
