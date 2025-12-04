{
  description = "OxCaml - A performance-focused fork of OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      in
      {
        packages = {
          inherit oxcaml;
          oxcaml-fp = oxcaml.override { framePointers = true; };
          oxcaml-r5 = oxcaml.override { runtime5 = true; };
          oxcaml-fp-r5 = oxcaml.override {
            framePointers = true;
            runtime5 = true;
          };
          oxcaml-asan = oxcaml.override { addressSanitizer = true; };
          oxcaml-asan-r5 = oxcaml.override {
            addressSanitizer = true;
            runtime5 = true;
          };
          default = oxcaml;
        };

        checks = lib.attrsets.filterAttrs (key: drv: !(drv.meta.broken or false)) {
          inherit (self.packages.${system})
            oxcaml
            oxcaml-fp
            oxcaml-r5
            oxcaml-fp-r5
            oxcaml-asan
            oxcaml-asan-r5
            ;
        };

        formatter = pkgs.nixfmt-tree;

        devShells.default = self.packages.${system}.oxcaml;

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
