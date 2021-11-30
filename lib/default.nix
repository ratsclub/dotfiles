inputs:

rec {
  mkNixpkgs = { system }:
    import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays =
        (import ../overlays).nixpkgs.overlays
        ++ [
          inputs.nur.overlay
        ];
    };

  mkHost = { host, deviceType, system ? "x86_64-linux", username }:
    let pkgs = mkNixpkgs { inherit system; };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = {
        inherit pkgs inputs;
        inherit (inputs) nixpkgs home-manager nur;
        hardware = inputs.hardware.nixosModules;
      };

      modules = [
        # host configuration
        (../hosts + "/${host}")

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

  mkHome = { username, system, deviceType }:
    let
      pkgs = mkNixpkgs { inherit system; };
      homeDirectory = "/home/${username}";
    in
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit system username homeDirectory pkgs;
      configuration = ../home;
      extraSpecialArgs = {
        inherit inputs system username;
        super.device.type = deviceType;
      };
    };
}
