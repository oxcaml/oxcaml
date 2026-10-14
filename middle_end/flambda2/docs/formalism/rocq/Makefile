# Rocq mechanization of the flambda2 formalism. Independent of the compiler
# build; requires rocq (9.2+) on PATH (opam switch comp-5.4.0).

.PHONY: build clean

build: CoqMakefile
	$(MAKE) -f CoqMakefile -j

CoqMakefile: _CoqProject
	rocq makefile -f _CoqProject -o CoqMakefile

clean:
	if [ -f CoqMakefile ]; then $(MAKE) -f CoqMakefile clean; fi
	rm -f CoqMakefile CoqMakefile.conf
