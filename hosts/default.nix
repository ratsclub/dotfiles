{ inputs, outputs, ... }:

let
  inherit (inputs)
    nixpkgs
    stable
    small
    ;

  mkPkgs = { nixpkgs, system, overlay, ... }:
    import nixpkgs {
      inherit system;
      overlays = [
        outputs.overlays.default
        overlay
      ];
      config.allowUnfree = true;
    };
in
{
  magnus = nixpkgs.lib.nixosSystem rec {
    system = "aarch64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
    };
    modules = [ ./magnus ];
    specialArgs = { inherit inputs; };
  };

  capivaras = stable.lib.nixosSystem rec {
    system = "aarch64-linux";
    pkgs = mkPkgs {
      inherit system;

      nixpkgs = stable;

      # TODO: until v9 reaches nixpkgs stable
      overlay = (final: prev: {
        forgejo = small.legacyPackages.${system}.forgejo;
        forgejo-runner = small.legacyPackages.${system}.forgejo-runner;
      });
    };
    modules = [
      ./capivaras
      inputs.agenix.nixosModules.default
    ];
    specialArgs = { inherit inputs; };
  };

  davila = stable.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit system;

      nixpkgs = stable;

      # TODO: until v9 reaches nixpkgs stable
      overlay = (final: prev: {
        forgejo = small.legacyPackages.${system}.forgejo;
        forgejo-runner = small.legacyPackages.${system}.forgejo-runner;
      });
    };
    modules = [
      ./davila
      inputs.agenix.nixosModules.default
    ];
    specialArgs = { inherit inputs; };
  };
}
