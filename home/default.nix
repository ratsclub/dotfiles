{ inputs, self, ... }:

{
  mini = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        inputs.nur.overlays.default
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

          packages = [ pkgs.graphite-cli ];
          stateVersion = "24.11";
        };
      })

      ./modules/vscodium.nix
      ./modules/emacs
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/zsh.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@theformfactory.co"; })
    ];
  };

  air = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        inputs.nur.overlays.default
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

          packages = [ pkgs.graphite-cli ];
          stateVersion = "24.11";
        };
      })

      ./modules/vscodium.nix
      ./modules/emacs
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/zsh.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@freire.dev.br"; })
    ];
  };

  code = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        inputs.nur.overlays.default
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
      ./modules/cli.nix
      ./modules/direnv.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@freire.dev.br"; })
    ];

    extraSpecialArgs = {
      inherit inputs;
    };
  };

  victor = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        inputs.nur.overlays.default
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
      # ./modules/email.nix
      # ./modules/firefox.nix
      (import ./modules/git.nix { userName = "Victor Freire"; userEmail = "victor@freire.dev.br"; })
      ./modules/gui.nix
      ./modules/vscodium.nix
    ];

    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
