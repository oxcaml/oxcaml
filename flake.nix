{
  description = "OxCaml - A performance-focused fork of OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/ca534a76c4afb2bdc07b681dbc11b453bab21af8";
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
          oxcaml-r5 = oxcaml;
          oxcaml-fp-r5 = oxcaml.override {
            framePointers = true;
          };
          oxcaml-asan-r5 = oxcaml.override {
            addressSanitizer = true;
          };
        };

        checks = lib.attrsets.filterAttrs (key: drv: !(drv.meta.broken or false)) {
          inherit (self.packages.${system})
            oxcaml-r5
            oxcaml-fp-r5
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
