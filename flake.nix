{
  description = "OxCaml - A performance-focused fork of OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/fcb8fcd6bf2d0adecae5bd491afaaaf8311b758d";
    flake-utils.url = "github:numtide/flake-utils";
    nix-github-actions = {
      url = "github:nix-community/nix-github-actions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nix-github-actions,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        oxcaml = pkgs.callPackage ./default.nix { src = self; };
        merlinPackages = oxcaml.mkMerlinPackages oxcaml;
      in
      {
        packages = {
          inherit oxcaml;
          inherit (merlinPackages) merlin-lib dot-merlin-reader merlin;
          oxcaml-fp = oxcaml.override { framePointers = true; };
          oxcaml-asan = oxcaml.override { addressSanitizer = true; };
          jsoo = oxcaml.jsoo.binaries;
          default = oxcaml;
        };

        checks = lib.attrsets.filterAttrs (key: drv: !(drv.meta.broken or false)) {
          inherit (self.packages.${system})
            oxcaml
            oxcaml-fp
            oxcaml-asan
            merlin
            jsoo
            ;
        };

        formatter = pkgs.nixfmt-tree;

        # Use the compiler derivation itself as the dev shell so `nix develop`
        # exposes its full build environment (configureFlags, preConfigure,
        # OXCAML_LLDB/OXCAML_CLANG, ...) and the `configurePhase` advertised by
        # the shellHook behaves exactly like the nix build. withMerlin only
        # extends its inputs with what `make merlin-build` / `make merlin-test`
        # need.
        devShells.default = oxcaml.override { withMerlin = true; };
      }
    )
    // {
      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks = nixpkgs.lib.getAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ] self.checks;
        platforms = {
          "x86_64-linux" = "warp-ubuntu-latest-x64-8x";
          "aarch64-linux" = "warp-ubuntu-latest-arm64-8x";
          "aarch64-darwin" = "warp-macos-15-arm64-6x";
        };
      };
    };
}
