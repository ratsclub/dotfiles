{
  description = "My personal dotfiles";

  inputs = {
    nur.url = github:nix-community/nur;
    hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    nix-colors.url = "github:misterio77/nix-colors";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { ... }@inputs:
    let prelude = import ./prelude inputs;
    in
    {
      nixosConfigurations = {
        t495 = prelude.mkHost {
          host = "t495";
          system = "x86_64-linux";
          username = "victor";
          nixosModules = [
            ./modules/meta.nix
            ./nixos/cli.nix
            ./nixos/nix.nix
            ./nixos/user.nix

            inputs.hardware.nixosModules.lenovo-thinkpad-t495
          ];

          homeModules = [
            ./home/bash.nix
            ./home/chromium.nix
            ./home/firefox.nix
            ./home/git.nix
            ./home/cli.nix
            ./home/gui.nix
            ./home/home.nix
            ./home/kitty.nix
            ./home/neovim.nix
            ./home/newsboat.nix
            ./home/vscodium.nix

            ./modules/meta.nix

            inputs.nix-colors.homeManagerModule
          ];
        };

        rpi3 = prelude.mkHost {
          system = "aarch64-linux";
          host = "rpi3";
          username = "victor";
          nixosModules = [
            ./modules/meta.nix
            ./nixos/cli.nix
            ./nixos/nix.nix
            ./nixos/user.nix
          ];
        };
      };

      homeConfigurations = {
        # `home-manager switch --flake ".#work"`
        # `nix build ".#homeConfigurations.work.activationPackage"`
        work = prelude.mkHome {
          system = "x86_64-linux";
          username = "victor";
          homeModules = [
            ./home/bash.nix
            ./home/chromium.nix
            ./home/firefox.nix
            ./home/git.nix
            ./home/cli.nix
            ./home/home.nix
            ./home/neovim.nix
            ./home/newsboat.nix
            ./home/vscodium.nix

            ./modules/meta.nix
          ];
        };
      };

      templates = import ./templates;
    } // inputs.utils.lib.eachDefaultSystem (system:
      let pkgs = prelude.mkNixpkgs { inherit system; };
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            nixpkgs-fmt
            rnix-lsp
          ];
        };
      }
    );
}
