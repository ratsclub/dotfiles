{
  description = "Rust Project Template.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rustOverlay.url = "github:oxalica/rust-overlay";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, rustOverlay }:
    utils.lib.eachDefaultSystem (system:
      let
        projectName = "project";

        rustChannel = "stable";
        rustVersion = "latest"; # ex. 1.58.0

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rustOverlay) ];
        };

        inherit (pkgs)
          lib
          mkShell
          rust-analyzer
          rust-bin
          rustPlatform;

        rustToolchain = rust-bin."${rustChannel}"."${rustVersion}".default.override {
          extensions = [
            "rust-std"
            "rust-src"
          ];
        };

        projectPkg = rustPlatform.buildRustPackage {
          name = projectName;
          src = lib.cleanSource ./.;

          rustc = "${rustToolchain}/bin/rustc";

          cargoSha256 = lib.fakeSha256;
        };
      in
      rec {
        # `nix build`
        packages."${projectName}" = projectPkg;
        defaultPackage = packages."${projectName}";

        # `nix run`
        apps."${projectName}" = utils.lib.mkApp {
          drv = packages."${projectName}";
        };
        defaultApp = apps."${projectName}";

        # `nix develop`
        devShell = mkShell {
          nativeBuildInputs = [
            rustToolchain
            rust-analyzer
          ];
        };
      });
}
