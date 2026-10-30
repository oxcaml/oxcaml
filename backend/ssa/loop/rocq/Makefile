# The Rocq models span three directories (see rocq/Makefile at the root of
# the repository, which builds all of them). This forwards to it, so that
# `make` here still checks the proofs and regenerates the extracted OCaml.

.PHONY: all clean

all clean:
	$(MAKE) -C ../../../../rocq $@
