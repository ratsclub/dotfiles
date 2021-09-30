inputs:

rec {
  mkNixpkgs = { system }:
    import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = (import ../overlays).nixpkgs.overlays;
    };

  mkHost = { host, deviceType, system ? "x86_64-linux", username }:
    let pkgs = mkNixpkgs { inherit system; };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = {
        inherit pkgs;
        hardware = inputs.hardware.nixosModules;
        nixpkgs = inputs.nixpkgs;
        home-manager = inputs.home-manager;
        nur = inputs.nur;
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
