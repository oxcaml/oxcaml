# Unicode specification extraction

The file `src/syntax/unicode.ml` is generated using the data available at
[unicode.org](https://www.unicode.org/Public/).

The rule with `targets unicode.ml` at `src/syntax/dune` is the main entry point for this process.
It specifies how `unicode.ml` should be generated when running `dune @build` and triggers:
* download of data files at `src/generator/data`
* build of `src/generator/gen_unicode.exe`
* generation `unicode.ml` and places a copy in the source tree and a copy in the build tree

The rule is ignored when using the `--ignore-promoted-rules` option. This option is also implied
when using `-p`/`--for-release-of-packages` which is used for production build so production build
do not download the text data and re-generate `unicode.ml`.

However, each development build re-generates a `unicode.ml` file which is placed into the source
tree and, thus, can be easily commited when it is updated.

See: [dune documentation](https://dune.readthedocs.io/en/latest/dune-files.html#modes) for more
information.

## Update to new Unicode versions

To update the supported version, update the URL at `src/generator/data/base_url`. Make sure to
not include a trailing new line so that it is properly read in `src/generator/data/dune`.

Finally, place a copy of the old `unicode.ml` at `examples/unicode_old.ml` and update 
`test_versions` and `regressions` in `examples/regressions.ml`.
