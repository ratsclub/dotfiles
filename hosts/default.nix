{ inputs, outputs, ... }:

let
  inherit (inputs)
    nixpkgs
    stable
    small
    ;

  mkPkgs =
    {
      nixpkgs,
      system,
      overlay,
      ...
    }:
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
  catarina = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
      overlay = (final: prev: { });
    };
    modules = [ ./catarina ];
    specialArgs = { inherit inputs; };
  };

  joan = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
      overlay = (final: prev: { });
    };
    modules = [ ./joan ];
    specialArgs = { inherit inputs; };
  };
}
