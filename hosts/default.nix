{ inputs, outputs, ... }:

let
  inherit (inputs)
    hardware
    nixpkgs
    stable
    ;

  mkPkgs = { nixpkgs, system, ... }:
    import nixpkgs {
      inherit system;
      overlays = [
        inputs.nur.overlay
        outputs.overlays.default
      ];
      config.allowUnfree = true;
    };
in
{
  magnus = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
    };
    modules = [ ./magnus ];
    specialArgs = { inherit inputs; };
  };

  teresa = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
    };
    modules = [ ./teresa ];
    specialArgs = { inherit inputs; };
  };
}
