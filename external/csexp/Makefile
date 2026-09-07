INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	dune runtest

test:
	dune runtest

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev

dune-release:
	dune-release tag
	dune-release distrib --skip-build --skip-lint --skip-tests
# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: default install uninstall reinstall clean test
