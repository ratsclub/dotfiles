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
        pname = "project";
        version = (builtins.fromTOML
          (builtins.readFile ./Cargo.toml)).package.version;

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
          inherit pname version;
          src = lib.cleanSource ./.;

          nativeBuildInputs = [ rustToolchain ];

          cargoSha256 = lib.fakeSha256;
        };
      in
      rec {
        # `nix build`
        packages."${pname}" = projectPkg;
        defaultPackage = packages."${pname}";

        # `nix run`
        apps."${pname}" = utils.lib.mkApp {
          drv = packages."${pname}";
        };
        defaultApp = apps."${pname}";

        # `nix develop`
        devShell = mkShell {
          nativeBuildInputs = [
            rustToolchain
            rust-analyzer
          ];
        };
      });
}
