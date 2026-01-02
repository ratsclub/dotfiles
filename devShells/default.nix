{ inputs }:

let

  inherit (inputs)
    nixpkgs
    devenv
    ;

  # System types to support.
  supportedSystems = [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];

  # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
  forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

  # Nixpkgs instantiated for supported system types.
  nixpkgsFor =
    nixpkgs:
    forAllSystems (
      system:
      import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      }
    );
in
forAllSystems (
  system:
  let
    pkgs = (nixpkgsFor nixpkgs).${system};
  in
  {
    web = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        (
          { pkgs, lib, ... }:
          {
            packages = [ ];
            languages = {
              javascript = {
                enable = true;
                package = pkgs.nodejs;
                yarn.enable = true;
                bun.enable = true;
              };
            };
          }
        )
      ];
    };
  }
)
