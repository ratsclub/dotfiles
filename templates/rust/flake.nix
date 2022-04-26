{
  description = "Rust Project Template.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust.url = "github:oxalica/rust-overlay";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, rust, ...}:
    let
      name = (builtins.fromTOML
        (builtins.readFile ./Cargo.toml)).package.name;
      version = (builtins.fromTOML
        (builtins.readFile ./Cargo.toml)).package.version;
    in
    {
      overlays.default = nixpkgs.lib.composeManyExtensions [
        rust.overlay
        (final: prev: {
          "${pname}RustToolchain" = final.rust-bin.selectLatestNightlyWith
            (toolchain:
              toolchain.default.override {
                extensions = [ "rust-std" "rust-src" ];
              });

          "${pname}" = import ./nix/package.nix {
            inherit pname version;
            pkgs = final;
          };
        })
      ];
    } // utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
          ];
        };
      in
      rec {
        # `nix build`
        packages."${pname}" = pkgs.callPackage ./.nix/package.nix {
          inherit pkgs pname version;
        };
        defaultPackage = packages."${pname}";

        # `nix run`
        apps."${pname}" = utils.lib.mkApp {
          drv = packages."${pname}";
        };
        defaultApp = apps."${pname}";

        # `nix develop`
        devShells.default = pkgs.callPackage ./.nix/default.shell.nix {
          inherit pkgs pname;
        };
      });
}
