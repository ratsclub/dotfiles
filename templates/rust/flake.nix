{
  description = "Rust Project Template.";

  inputs = {
    naersk.url = "github:nmattia/naersk";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust.url = "github:oxalica/rust-overlay";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, naersk, rust }:
    utils.lib.eachDefaultSystem
      (
        system:
        let
          project-name = "project-name";
          rust-channel = "stable";
          rust-version = "latest"; # ex. 1.55.0
          rust-overlay = import rust;

          pkgs = import nixpkgs {
            inherit system;
            overlays = [ rust-overlay ];
          };

          rust-toolchain = pkgs.rust-bin."${rust-channel}"."${rust-version}".default.override {
            extensions = [
              "rust-std"
              "rust-src"
            ];
          };

          naersk-lib = naersk.lib."${system}".override {
            rustc = rust-toolchain;
          };

          project-pkg = naersk-lib.buildPackage {
            pname = project-name;
            root = pkgs.lib.cleanSource ./.;
          };
        in
        rec {
          # `nix build`
          packages."${project-name}" = project-pkg;
          defaultPackage = packages."${project-name}";

          # `nix run`
          apps."${project-name}" = utils.lib.mkApp {
            drv = packages."${project-name}";
          };
          defaultApp = apps."${project-name}";

          # `nix develop`
          devShell = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              rust-toolchain
              rust-analyzer
            ];
          };
        }
      );
}
