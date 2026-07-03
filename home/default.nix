{ inputs, self, ... }:

{
  mini = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        self.overlays.default
      ];
      config = {
        allowUnfree = true;
      };
    };

    modules = [
      (
        { ... }:
        {
          home = {
            username = "victor";
            homeDirectory = "/Users/victor";

            stateVersion = "24.11";
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/vcs.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };

  air = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "aarch64-darwin";
      overlays = [
        self.overlays.default
      ];
      config = {
        allowUnfree = true;
      };
    };

    modules = [
      (
        { ... }:
        {
          home = {
            username = "victor";
            homeDirectory = "/Users/victor";

            stateVersion = "24.11";
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/vcs.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };

  victor = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        self.overlays.default
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
      ./modules/vcs.nix
    ];

    extraSpecialArgs = {
      inherit inputs self;
    };
  };
}
