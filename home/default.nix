{ inputs, self, ... }:

{
  mini = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        self.overlays.modifications
      ];
      config = {
        allowUnfree = true;
      };
    };

    modules = [
      ({ pkgs, ... }: {
        home = {
          username = "victor";
          homeDirectory = "/Users/victor";
          sessionPath = [
            "$HOME/.local/bin"
          ];

          packages = with pkgs; [ bun gh ];
          stateVersion = "24.11";
        };
      })

      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@theformfactory.co"; })
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };

  air = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        self.overlays.modifications
      ];
      config = {
        allowUnfree = true;
      };
    };

    modules = [
      ({ pkgs, ... }: {
        home = {
          username = "victor";
          homeDirectory = "/Users/victor";
          sessionPath = [
            "$HOME/.local/bin"
          ];

          packages = [ ];
          stateVersion = "24.11";
        };
      })

      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@freire.dev.br"; })
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };

  victor = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        self.overlays.modifications
      ];
      config = {
        allowUnfree = true;
      };
    };
    modules = [
      inputs.agenix.homeManagerModules.age
    ]
    ++ [
      ./modules
      ./modules/bash.nix
      ./modules/chromium.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/gui.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      # ./modules/email.nix
      # ./modules/firefox.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@freire.dev.br"; })
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };
}
