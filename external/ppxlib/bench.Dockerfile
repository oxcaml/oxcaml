# Borrowed from Eio's benchmarking Dockerfile
FROM ocaml/opam:debian-13-ocaml-5.5
RUN sudo ln -sf /usr/bin/opam-2.5 /usr/bin/opam
# Ensure opam-repository is up-to-date:
RUN cd opam-repository && git pull -q origin 68acdc87b95f10206b36751f535034556b3cb606 && opam update

# Install Ppxlib's dependencies:
RUN mkdir ppxlib
WORKDIR ppxlib
COPY *.opam ./
RUN opam pin --with-version=dev . -yn
RUN opam install --deps-only . 
# Build the benchmarks:
COPY . ./
RUN opam exec -- dune build ./bench
