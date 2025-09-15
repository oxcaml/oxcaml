{
  pkgs ? import <nixpkgs> { },
}:
with pkgs;
let
  drv = (import ./default.nix { inherit pkgs; });
in
mkShell {
  inputsFrom = [ drv ];
  nativeBuildInputs = [
    git
    less
    drv.ocamlPackages.merlin
    drv.ocamlPackages.ocaml-lsp
    drv.ocamlPackages.ocamlformat_0_27_0
  ];

  shellHook = ''
    export out="$(pwd)/_install"
    exportOcamlDestDir
    dune clean

    buildPhase() {
      ${drv.buildPhase}
    }

    installPhase() {
      ${drv.installPhase}
    }

    export -f buildPhase installPhase
  '';
}
