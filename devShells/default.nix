{ inputs }:

let

  inherit (inputs)
    nixpkgs
    stable
    devenv;

  # System types to support.
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];

  # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
  forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

  # Nixpkgs instantiated for supported system types.
  nixpkgsFor = nixpkgs: forAllSystems (system: import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  });
in
forAllSystems (system:
let
  pkgs = (nixpkgsFor stable)."${system}";
in
{
  java = devenv.lib.mkShell {
    inherit inputs pkgs;
    modules = [
      ({ pkgs, lib, ... }: {
        languages = {
          java.enable = true;
          java.jdk.package = pkgs.jdk17_headless;
          java.maven.enable = true;
        };
      })
    ];
  };

  web = devenv.lib.mkShell {
    inherit inputs pkgs;
    modules = [
      ({ pkgs, lib, ... }: {
        packages = [ ];
        languages = {
          javascript.enable = true;
          javascript.package = pkgs.nodejs_22;
          javascript.yarn.enable = true;
        };
      })
    ];
  };
})
