{
  description = "Merlin Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  # Tip of nixos-24.11 as of 2026-02-14, chosen as the last revision before `menhir` added a `menhirGLR` package.
  inputs.nixpkgs.url = "github:nixos/nixpkgs/626a1db9776eb9db61b1e1f394d928505162cf69";
  inputs.menhir-repository = {
    url = "gitlab:fpottier/menhir/20231231?host=gitlab.inria.fr";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      menhir-repository,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";

        # Build with OCaml 5.4
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_4.overrideScope (
          _: osuper: {
            # Override menhirLib to the pinned version
            menhirLib = osuper.menhirLib.overrideAttrs (_: {
              version = "20231231";
              src = menhir-repository;
            });

            inherit (packages) merlin-lib dot-merlin-reader merlin;
          }
        );

        inherit (ocamlPackages) buildDunePackage;

        packages = rec {
          default = merlin;
          merlin-lib = buildDunePackage {
            pname = "merlin-lib";
            version = "dev";
            src = self;
            duneVersion = "3";
            propagatedBuildInputs = with ocamlPackages; [
              csexp
              alcotest
            ];
            doCheck = true;
          };

          dot-merlin-reader = buildDunePackage {
            pname = "dot-merlin-reader";
            version = "dev";
            src = self;
            duneVersion = "3";
            propagatedBuildInputs = [ ocamlPackages.findlib ];
            buildInputs = [ merlin-lib ];
            doCheck = true;
          };

          merlin = buildDunePackage {
            pname = "merlin";
            version = "dev";
            src = self;
            duneVersion = "3";
            buildInputs = [
              merlin-lib
              dot-merlin-reader
              ocamlPackages.menhirLib
              ocamlPackages.menhirSdk
              ocamlPackages.yojson
            ];
            nativeBuildInputs = [
              ocamlPackages.menhir
              pkgs.jq
            ];
            nativeCheckInputs = [ dot-merlin-reader ];
            checkInputs = with ocamlPackages; [ ppxlib ];
            doCheck = false; # Depends on a OxCaml
            checkPhase = ''
              runHook preCheck

              patchShebangs tests/merlin-wrapper
              MERLIN_TEST_OCAML_PATH=${ocamlPackages.ocaml} \
                dune build @check @runtest

              runHook postCheck
            '';
            meta = with pkgs; {
              mainProgram = "ocamlmerlin";
            };
          };
        };
      in
      {
        inherit packages;

        formatter = pkgs.nixfmt-tree;

        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with ocamlPackages; [ merlin ];
        };
      }
    );
}
