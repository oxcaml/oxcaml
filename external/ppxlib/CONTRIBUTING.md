This repository contains core libraries and tools used to develop ppx
rewriters. The code was originally developed and is still maintained
and used by [Jane Street][js].

This repository is not the first piece of open source software
released by Jane Street, however it is the first to be entirely
developed on GitHub. We are hoping that opening the development of
this repository will help collaboration with other open source users.

We welcome contributions and we will be happy to add contributors,
given that they are motivated to help maintain and grow the
project. However, given how important this code is to the functioning
of Jane Street, we do require that at least one Jane Street developer
reads every pull request that modifies the source code.

Additionally, all contributors must sign-off their commits, see
below for details.

### Developing patches

#### Setting up your dev environment

Before starting development on `ppxlib` you should install ppxlib's
dependencies. If you're doing it for the first time you can create
a local switch with all the right dependencies installed by running:
```
opam switch create ./ --with-test --with-dev-setup
```
or if you want to use a pre-existing switch:
```
opam install ./ppxlib.opam --deps-only --with-test --with-dev-setup
```

Note that the `--with-dev-setup` flag is only available from `opam.2.2.0`.
If you are running an older opam and do not wish to update it, you will have
to manually install `ocamlformat`.

#### Submitting patches

Before submitting a PR, please run `dune build @install @runtest @fmt`
on your machine.

[cinaps][cinaps] is used to keep up-to-date some parts of the code that are
auto-generated and committed in the repository.

### Submitting patches and code review

Once a patch is ready according to the criteria stated in the
previous section, it should be submitted via the GitHub website. When
submitting a pull request, we prefer if you tick the `Allow edits from
maintainers` box as it is much simpler to fix typos or do simple
improvements directly rather than go back and forth through the web
interface.

### Signing commits

We require that you sign your contributions. Your signature certifies
that you wrote the patch or otherwise have the right to pass it on as
an open-source patch. The rules are pretty simple: if you can certify
the below (from [developercertificate.org][dco]):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

```
Signed-off-by: Joe Smith <joe.smith@email.com>
```

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign
your commit automatically with git commit -s.

[js]:     https://opensource.janestreet.com/
[ocpi]:   https://github.com/OCamlPro/ocp-indent
[cinaps]: https://github.com/janestreet/cinaps
[dco]:    http://developercertificate.org/
