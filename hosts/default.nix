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
  teresa = nixpkgs.lib.nixosSystem rec {
    system = "x86_64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
    };
    modules = [ ./teresa ];
    specialArgs = { inherit inputs; };
  };

  magnus = nixpkgs.lib.nixosSystem rec {
    system = "aarch64-linux";
    pkgs = mkPkgs {
      inherit nixpkgs system;
    };
    modules = [ ./magnus ];
    specialArgs = { inherit inputs; };
  };
}
