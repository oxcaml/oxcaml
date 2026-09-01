# How to release ppxlib

`ppxlib` is a rather central piece of the OCaml ecosystem and as such,
new versions must be released with care.

It is advised to start with a "draft" release by using github's auto-generated
archives with the git hash of the main branch's HEAD. This can be done by
opening a draft PR to opam-repository using a copy of `ppxlib.opam` and adding
the following:
```
url {
  src: "https://github.com/ocaml-ppx/ppxlib/archive/<rev>.tar.gz"
  checksum: [
    "sha256=<sha256sum>"
    "sha512=<sha512sum>"
  ]
}
```
and replacing `<rev>` by the main branch's HEAD git hash (or the revision you'd
like to release) and adding the hash of the corresponding archive.

Once we are satisfied with the results of `opam-ci` on our draft PR, we can
proceed with a regular dune-release workflow, ideally reusing the existing PR.

## Dealing with trunk support

Starting with OCaml 5.3, we are experimenting incrementally adding support
for the next OCaml release directly onto ppxlib's main branch.

Our opam file therefore declares a compiler upper bound two versions higher than
the latest release to allow installing ppxlib with a trunk compiler when pinning
it to the main branch.

We do not want to release the support before the next compiler is stable enough,
usually that's when it reaches the first beta release. In the meantime, when
releasing we must edit the upper bound, setting it back so that we support OCaml
versions up to the latest release. Once the release went through, we must set it
back to its previous value.

To both act as a reminder and help interested users to keep track of trunk
support, we also added a changelog section about it in the `unreleased` section.
Similarly to the compiler upperbound, this section is not be included into the
release changelog until the new compiler is stable enough. The section should
be removed from the changelog before the release and re-added afterwards.
