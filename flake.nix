{
  description = "My personal dotfiles";

  inputs = {
    nur.url = github:nix-community/nur;
    hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    nix-colors.url = "github:misterio77/nix-colors";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { ... }@inputs:
    let lib = import ./lib inputs;
    in
    {
      nixosConfigurations.t495 = lib.mkHost {
        host = "t495";
        system = "x86_64-linux";
        username = "victor";
        deviceType = "graphical";
      };

      homeConfigurations =
        let
          system = "x86_64-linux";
          username = "victor";
        in
        {
          # `home-manager switch --flake ".#graphical"`
          # `nix build ".#homeConfigurations.graphical.activationPackage"`
          graphical = lib.mkHome {
            inherit username system;
            deviceType = "graphical";
          };

          # `home-manager switch --flake ".#textual"`
          # `nix build ".#homeConfigurations.textual.activationPackage"`
          textual = lib.mkHome {
            inherit username system;
            deviceType = "textual";
          };
        };

      templates = import ./templates;
    } // inputs.utils.lib.eachDefaultSystem (system:
      let pkgs = lib.mkNixpkgs { inherit system; };
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            nixpkgs-fmt
          ];
        };
      }
    );
}
