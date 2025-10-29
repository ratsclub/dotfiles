{
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    stable.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs =
    {
      self,
      unstable,
      stable,
      ...
    }:
    let
      # change the system if needed
      system = "x86_64-linux";

      vmOverlay = final: prev: { };

      unstablePkgs = import unstable {
        inherit system;
        overlays = [ vmOverlay ];
      };

      stablePkgs = import stable {
        inherit system;
        overlays = [ vmOverlay ];
      };
    in
    {
      # `test` is a hostname for our machine
      # run:
      #  1. `nix shell nixpkgs#nixos-rebuild`
      #  2. `nixos-rebuild build-vm --flake .#test`
      nixosConfigurations.test =
        # change `unstable` to `stable` if needed
        unstable.lib.nixosSystem {
          inherit system;

          # change this to `stablePkgs` or `unstablePkgs` if needed
          pkgs = unstablePkgs;

          modules = [
            ./configuration.nix
          ];
        };
    };
}
