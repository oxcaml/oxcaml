{
  pkgs ? import <nixpkgs> { },
  ...
}@args:
let
  drv = (import ./default.nix { inherit pkgs; } // args);
in
with pkgs;
drv.overrideAttrs (new: old: {
  nativeBuildInputs = ( old.nativeBuildInputs or [] ) ++ [
    git
    less
    drv.ocamlPackages.merlin
    drv.ocamlPackages.ocaml-lsp
    drv.ocamlPackages.ocamlformat_0_24_1
    drv.ocamlPackages.ocamlformat_0_27_0
  ];

  shellHook =
    ''
      prefix="$(pwd)/_install"
      export out="$(pwd)/_install"
      exportOcamlDestDir
      dune clean
    ''
    + lib.optionalString (new ? configurePhase) ''
      configurePhase() {
        ${new.configurePhase}
      }
    ''
    + lib.optionalString (new ? buildPhase) ''
      buildPhase() {
        ${new.buildPhase}
      }
    ''
    + lib.optionalString (new ? installPhase) ''
      installPhase() {
        ${new.installPhase}
      }
    ''
    + ''
      [[ -n ''${PS1-} ]] || PS1=$'\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] '
      cat >&2 << EOF
      OxCaml $version Development Environment
      ===============================''${version//?/=}

      Available commands:
        configurePhase           - Pre-build setup
        make boot-compiler       - Quick build (recommended for development)
        make boot-_install       - Quick install (recommended for development)
        make fmt                 - Auto-format code
        make                     - Full build
        make install             - Install
        make test                - Run all tests
        make test-one TEST=...   - Run a single test
      EOF
    '';
})
