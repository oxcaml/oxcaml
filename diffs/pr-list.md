`merge PR` = this is a merge commit

`non-merge` = this is a non-PR commit, or a squash+merge PR commit.  In the latter case you can click through (via the changeset page) to get to the PR page.

## asmcomp

Classification: `now` = must do during upgrade, `later` = can do later, `skip` = only if needed


`a9f9c84 | merge PR13673 | later     |` [#13669: use compilation unit name in dependency check for the native linker](https://github.com/ocaml/ocaml//pull/13673)
`1680bfe | merge PR12084 | later     |` [Check link order when creating archive and when using ocamlopt ](https://github.com/ocaml/ocaml//pull/12084)

`e5a0e1b | merge PR14074 | later     |` [Emit `stlr` instead of `dmb ishld; str` for stores on Apple Silicon macOS](https://github.com/ocaml/ocaml//pull/14074)
`6332880 | merge PR13878 | later     |` [Track mutability of Cmm variables](https://github.com/ocaml/ocaml//pull/13878)
`25265d7 | merge PR13807 | later     |` [Add support for unaligned accesses on arm64](https://github.com/ocaml/ocaml//pull/13807)
`8db7821 | merge PR13759 | later     |` [Propagate missing Cmm value kind](https://github.com/ocaml/ocaml//pull/13759)
`ecaaf78 | merge PR13735 | later     |` [Follow the behaviour of the C compiler for the `.size` and `.type` directives and the `.note.GNU-stack` section](https://github.com/ocaml/ocaml//pull/13735)
`930d7e2 | merge PR13667 | later     |` [instr_size fixes for arm64](https://github.com/ocaml/ocaml//pull/13667)
`51622f4 | merge PR13672 | later     |` [Avoid register stall on conversion instructions on amd64](https://github.com/ocaml/ocaml//pull/13672)
`1ad1804 | merge PR13595 | later     |` [Use correct CFA register on ARM64](https://github.com/ocaml/ocaml//pull/13595)
`f3d5df4 | merge PR13565 | later     |` [Less tagging in asmcomp-optimized switches](https://github.com/ocaml/ocaml//pull/13565)
`39f6aa6 | merge PR13500 | later     |` [Add frame pointers support for ARM64 on Linux and macOS](https://github.com/ocaml/ocaml//pull/13500)
`8bf61a7 | merge PR13453 | later     |` [Typos](https://github.com/ocaml/ocaml//pull/13453)
`143706d | merge PR13449 | later     |` [cmm: stricter computation of boxed number kinds](https://github.com/ocaml/ocaml//pull/13449)
`55ee0e0 | merge PR13392 | later     |` [Fix a few bugs in instr_size on arm64](https://github.com/ocaml/ocaml//pull/13392)
`93263f9 | merge PR13251 | later     |` [Register error printer in Emitaux](https://github.com/ocaml/ocaml//pull/13251)
`8a3dc3d | merge PR13194 | later     |` [arm64: use pair load/store instructions when operating on trap handlers.](https://github.com/ocaml/ocaml//pull/13194)
`e663755 | merge PR13015 | later     |` [arm64: emit floating-point literals in .rodata on ELF platforms](https://github.com/ocaml/ocaml//pull/13015)
`73463a0 | non-merge     | later     |` [FreeBSD amd64: emit .note.GNU-stack same as on Linux (#13103)](https://github.com/ocaml/ocaml//commit/73463a05559ec699f7058082d194c4395f5eddbc)
`c39c3f9 | merge PR13079 | later     |` [Save and restore frame pointer across Iextcall on ARM64](https://github.com/ocaml/ocaml//pull/13079)
`ac5a3d7 | merge PR13044 | later     |` [Remove small OCaml3 leftovers](https://github.com/ocaml/ocaml//pull/13044)
`33502d6 | non-merge     | later     |` [#12984: restore the filename computation for companion cmi (#12987)](https://github.com/ocaml/ocaml//commit/33502d68c48e159632dd13731238e643b5785e40)
`7f3ecc8 | non-merge     | later     |` [Fixing typos (#12955)](https://github.com/ocaml/ocaml//commit/7f3ecc86e7d982dbef8fddcf9d744c1eb5d9f014)

`46c7582 | non-merge     | skip      |` [symbol names: revert to using `$` only on macOS and Windows (#14143)](https://github.com/ocaml/ocaml//commit/46c7582adac0ae6f87c319f5fb07e151f25aed8c)
`9e2faaa | non-merge     | skip      |` [Fix under-estimated Lcondbranch instruction size](https://github.com/ocaml/ocaml//commit/9e2faaa7f83f7db64fc7b7d9cd9d722589b8a3da)
`061adb7 | merge PR13607 | skip      |` [[riscv] Fix CFA annotation](https://github.com/ocaml/ocaml//pull/13607)
`703ba8e | merge PR13050 | skip      |` [Use '$' instead of '.' to separate module from identifiers in symbols](https://github.com/ocaml/ocaml//pull/13050)
`51a1781 | non-merge     | skip      |` [docs(asmcomp/riscv/NOTES.md): update deadlink (#13505)](https://github.com/ocaml/ocaml//commit/51a1781be2730f99ca33e4351d0cc5557b4c43cb)
`dae3b65 | non-merge     | skip      |` [Don't use r12 to pass size to caml_call_realloc_stack (#13410)](https://github.com/ocaml/ocaml//commit/dae3b659e8ef27ae61ec0b62cfd768b73b52816a)
`d74fe12 | merge PR13396 | skip      |` [[refactoring] Simplify the Patomic_load primitive by dropping immediate information](https://github.com/ocaml/ocaml//pull/13396)
`6475f63 | merge PR13397 | skip      |` [[refactor] simplify the definition of atomic functions](https://github.com/ocaml/ocaml//pull/13397)
`cd7bf78 | non-merge     | skip      |` [Ignore Clflags.pic_code, never set on riscv64. (#13266)](https://github.com/ocaml/ocaml//commit/cd7bf7845ed5db8bfbf373b7995556b5c437f407)
`44d9b93 | non-merge     | skip      |` [Use Clflags.pic_code instead of an internal variable. (#13265)](https://github.com/ocaml/ocaml//commit/44d9b93af5405f09aacaa32b1e5d132f94f7cd86)
`b5cc86e | merge PR13221 | skip      |` [[power] more accurate instruction sizes for branch relaxation](https://github.com/ocaml/ocaml//pull/13221)
`1f3c26c | non-merge     | skip      |` [Remove unused Arch.num_args_addressing (#13060)](https://github.com/ocaml/ocaml//commit/1f3c26c89b87d9c610699253b605b3fbace2f070)
`2e2b17d | merge PR12954 | skip      |` [Restore the MSVC port of OCaml](https://github.com/ocaml/ocaml//pull/12954)
`2a16b0e | non-merge     | skip      |` [Add per function sections support to the missing compiler backends (#13014)](https://github.com/ocaml/ocaml//commit/2a16b0e17f7e9d4fcc80a2929c16cdcc87f5b797)
`22d220a | merge PR12915 | skip      |` [ThreadSanitizer support for s390x](https://github.com/ocaml/ocaml//pull/12915)
`5548338 | merge PR12876 | skip      |` [ThreadSanitizer support for POWER](https://github.com/ocaml/ocaml//pull/12876)
`993a7e8 | non-merge     | skip      |` [ThreadSanitizer support for Risc-V (#12907)](https://github.com/ocaml/ocaml//commit/993a7e8c2153b50315eef635e5772bade1d13970)
`42ece63 | merge PR12914 | skip      |` [s390x: allow builds with llvm/clang](https://github.com/ocaml/ocaml//pull/12914)
`03ddea4 | merge PR12890 | skip      |` [arm64 backend: tweak specific_operation names](https://github.com/ocaml/ocaml//pull/12890)
`857f8ea | merge PR12810 | skip      |` [ThreadSanitizer support for arm64](https://github.com/ocaml/ocaml//pull/12810)


## middle_end
Classification: `now` = must do during upgrade, `later` = can do later, `skip` = only if needed

`8bf61a7 | merge PR13453 | later     |` [Typos](https://github.com/ocaml/ocaml//pull/13453)

`46c7582 | non-merge     | skip      |` [symbol names: revert to using `$` only on macOS and Windows (#14143)](https://github.com/ocaml/ocaml//commit/46c7582adac0ae6f87c319f5fb07e151f25aed8c)
`895f2ae | non-merge     | skip      |` [Enforce evaluation order for generic applications in Closure (#13882)](https://github.com/ocaml/ocaml//commit/895f2aed91d783c5665dc772a8a48652278a688b)
`8fe740d | non-merge     | skip      |` [Allow unboxing of static catch parameters with flambda (#13758)](https://github.com/ocaml/ocaml//commit/8fe740dd1c5b4d5496a9b0f9ed38755036ed13a9)
`c18466e | merge PR13526 | skip      |` [Simplify the build of cross compilers](https://github.com/ocaml/ocaml//pull/13526)
`703ba8e | merge PR13050 | skip      |` [Use '$' instead of '.' to separate module from identifiers in symbols](https://github.com/ocaml/ocaml//pull/13050)
`d74fe12 | merge PR13396 | skip      |` [[refactoring] Simplify the Patomic_load primitive by dropping immediate information](https://github.com/ocaml/ocaml//pull/13396)
`6475f63 | merge PR13397 | skip      |` [[refactor] simplify the definition of atomic functions](https://github.com/ocaml/ocaml//pull/13397)
`787b4fb | non-merge     | skip      |` [flambda: Improve transitive closure in invariant_params_in_recursion (#13150)](https://github.com/ocaml/ocaml//commit/787b4fbb5aaf3728de54ca240ba9ca0bf56ace60)
