inputs:

let
  lib = inputs.nixpkgs.lib;
in
rec {
  mkNixpkgs = { system }:
    import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.emacsOverlay.overlay
        inputs.nur.overlay
        (import ../overlay)
      ];
    };

  mkHost =
    { host
    , username
    , system ? "x86_64-linux"
    , nixosModules ? [ ]
    , homeModules ? [ ]
    }:
    let pkgs = mkNixpkgs { inherit system; };
    in
    inputs.nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = {
        inherit pkgs inputs system;
        inherit (inputs) nixpkgs homeManager nur hardware;
      };

      modules =
        nixosModules
        ++ [
          # host configuration
          (../hosts + "/${host}")

          # home manager
          inputs.homeManager.nixosModules.home-manager
          {
            home-manager = {
              useUserPackages = true;
              users."${username}" = {
                imports = homeModules;
              };
              extraSpecialArgs = {
                inherit inputs pkgs username;
              };
            };
          }
        ];
    };

  mkHome =
    { username
    , system
    , homeModules ? [ ]
    }:
    let
      pkgs = mkNixpkgs { inherit system; };
      homeDirectory = "/home/${username}";
    in
    inputs.homeManager.lib.homeManagerConfiguration {
      inherit system username homeDirectory pkgs;
      configuration = {
        imports = homeModules;
      };

      extraSpecialArgs = {
        inherit inputs system username;
      };
    };
}
