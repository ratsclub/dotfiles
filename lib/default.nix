inputs:

rec {
  mkNixpkgs = { system }:
    import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = (import ../overlays).nixpkgs.overlays ++ [ inputs.nur.overlay ];
    };

  mkHost = { host, deviceType, system ? "x86_64-linux", username }:
    let pkgs = mkNixpkgs { inherit system; };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = {
        inherit pkgs;
        inherit (inputs) nixpkgs home-manager nur;
        hardware = inputs.hardware.nixosModules;
      };

      modules = [
        # host configuration
        ../hosts/${host}

        # home manager
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager = {
            useUserPackages = true;
            users."${username}" = import ../home;
            extraSpecialArgs = {
              inherit inputs username pkgs;
              super.device.type = deviceType;
            };
          };
        }
      ];
    };
}
