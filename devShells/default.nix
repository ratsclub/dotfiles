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

  mkPkgs = import ../lib/mk-pkgs.nix;

  # Nixpkgs instantiated for supported system types.
  nixpkgsFor = nixpkgs: forAllSystems (system: mkPkgs { inherit nixpkgs system; });
in
forAllSystems (
  system:
  let
    pkgs = (nixpkgsFor nixpkgs).${system};
  in
  {
    ci = pkgs.callPackage ./ci.nix { };
    web = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        (
          { pkgs, ... }:
          {
            packages = with pkgs; [
              typescript-go
              typos
            ];
            languages = {
              javascript = {
                enable = true;
                yarn.enable = true;
                pnpm = {
                  enable = true;
                  package = pkgs.pnpm;
                };
                bun.enable = true;
              };
            };
          }
        )
      ];
    };
  }
)
