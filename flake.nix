{
  description = "My personal dotfiles";

  inputs = {
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs/master";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "unstable";

    hardware.url = "github:NixOS/nixos-hardware";
    homeManager.inputs.nixpkgs.follows = "unstable";
    homeManager.url = "github:nix-community/home-manager";
    nixColors.url = "github:misterio77/nix-colors";
    nur.url = "github:nix-community/nur";
  };

  outputs = { ... }@inputs:
    let
      prelude = import ./prelude inputs;
    in
    {
      nixosConfigurations = {
        bootstrap = prelude.mkHost {
          host = "earth";
          system = "x86_64-linux";
          username = "victor";
          nixosModules = [
            ./modules/meta.nix
            ./nixos/nix.nix
            ./nixos/user.nix

            ./hosts/earth/hardware-configuration.nix

            inputs.hardware.nixosModules.lenovo-thinkpad-t495
          ];
        };

        earth = prelude.mkHost {
          host = "earth";
          system = "x86_64-linux";
          username = "victor";
          nixosModules = [
            ./modules/meta.nix
            ./nixos/cli.nix
            ./nixos/nix.nix
            ./nixos/user.nix
            ./nixos/secrets/earth.nix

            ./hosts/earth/hardware-configuration.nix

            inputs.hardware.nixosModules.lenovo-thinkpad-t495
            inputs.agenix.nixosModule
          ];

          homeModules = [
            ./home/aerc.nix
            ./home/bash.nix
            ./home/chromium.nix
            ./home/cli.nix
            ./home/dconf.nix
            ./home/email.nix
            ./home/firefox.nix
            ./home/git.nix
            ./home/gui.nix
            ./home/home.nix
            ./home/kitty.nix
            ./home/neovim
            ./home/newsboat.nix
            ./home/rbw.nix
            ./home/vscodium.nix

            ./modules/aerc.nix
            ./modules/meta.nix

            inputs.nixColors.homeManagerModule
          ];
        };

        mars = prelude.mkHost {
          host = "mars";
          system = "aarch64-linux";
          username = "victor";
          nixosModules = [
            ./modules/meta.nix
            ./nixos/cli.nix
            ./nixos/nix.nix
            ./nixos/user.nix
            ./nixos/secrets/mars.nix

            "${inputs.unstable}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
            inputs.agenix.nixosModule
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
            ./home/cli.nix
            ./home/dconf.nix
            ./home/firefox.nix
            ./home/git.nix
            ./home/home.nix
            ./home/kitty.nix
            ./home/neovim
            ./home/newsboat.nix
            ./home/rbw.nix
            ./home/vscodium.nix

            ./modules/meta.nix

            inputs.nixColors.homeManagerModule

            ({ pkgs, ... }: {
              home.packages = with pkgs; [
                awscli2
                kubectl
                google-cloud-sdk
                fzfpods
              ];

              # can't run kitty without openGL so we just need its
              # configuration files
              programs.kitty.package = pkgs.hello;
            })
          ];
        };
      };

      templates = import ./templates;
    };
}
