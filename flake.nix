{
  description = "My personal dotfiles";

  inputs = {
    nur.url = github:nix-community/nur;
    hardware.url = "github:NixOS/nixos-hardware/master";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

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

      homeConfigurations.victor =
        let
          system = "x86_64-linux";
          username = "victor";
        in
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit system username;
          pkgs = lib.mkNixpkgs { inherit system; };
          configuration = ./home;
          extraSpecialArgs = {
            inherit inputs system username;
            super.device.type = "textual";
          };
        };
    };
}
