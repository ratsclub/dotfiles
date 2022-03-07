inputs:

let
  lib = inputs.nixpkgs.lib;
in
rec {
  mkNixpkgs =
    { nixpkgs ? inputs.unstable
    , system
    }:
    import nixpkgs {
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
    , nixpkgs ? inputs.unstable
    }:
    let pkgs = mkNixpkgs { inherit nixpkgs system; };
    in
    nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = {
        inherit pkgs inputs system nixpkgs;
        inherit (inputs) homeManager nur hardware;
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
