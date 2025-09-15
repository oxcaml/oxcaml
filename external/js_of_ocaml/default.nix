{
  pkgs ? import <nixpkgs> { },
  ...
}:
with pkgs;
let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14.overrideScope (
    self: super: {
      dune_3 =
        if lib.versionAtLeast super.dune_3.version "3.19" then
          super.dune_3
        else
          super.dune_3.overrideAttrs rec {
            version = "3.19.1";
            src = fetchurl {
              url = "https://github.com/ocaml/dune/releases/download/${version}/dune-${version}.tbz";
              hash = "sha256-oQOG+YDNqUF9FGVGa+1Q3SrvnJO50GoPf+7tsKFUEVg=";
            };
          };
    }
  );
in
ocamlPackages.buildDunePackage rec {
  pname = "js_of_ocaml-compiler";
  version = lib.strings.trim (builtins.readFile ./VERSION);
  src = lib.cleanSource ./.;

  nativeBuildInputs = [
    # CR-soon jvanburen: build wasm too
    # pkgs.binaryen
    ocamlPackages.menhir
  ];

  buildInputs = [
    ocamlPackages.time_now
    ocamlPackages.cmdliner
    ocamlPackages.ppxlib
  ];

  propagatedBuildInputs = [
    ocamlPackages.menhirLib
    ocamlPackages.sedlex
    ocamlPackages.yojson
  ];

  buildPhase = ''
    runHook preBuild
    dune build -p ${pname} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
    runHook postBuild
  '';

  passthru = {
    inherit ocamlPackages;
  };
}
