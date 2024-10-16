{ inputs, outputs, ... }:

let
  inherit (inputs)
    nixpkgs
    stable
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

  capivarasdev = stable.lib.nixosSystem rec {
    system = "aarch64-linux";
    pkgs = mkPkgs {
      inherit system;

      nixpkgs = stable;

      # TODO: until v8 reaches nixpkgs stable
      overlay = (final: prev: {
        forgejo = nixpkgs.legacyPackages.${system}.forgejo;
        forgejo-runner = nixpkgs.legacyPackages.${system}.forgejo-runner;
      });
    };
    modules = [
      ./capivarasdev
      inputs.agenix.nixosModules.default
    ];
    specialArgs = { inherit inputs; };
  };
}
