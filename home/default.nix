{ inputs, ... }:

{
  victor = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [ inputs.nur.overlay ];
      config = {
        allowUnfree = true;
      };
    };
    modules = [
      inputs.emacs.overlay
      inputs.nixColors.homeManagerModule
      inputs.nixDoomEmacs.hmModule
    ]
    ++ [
      ./modules
      ./modules/bash.nix
      ./modules/cli.nix
      ./modules/doom
      ./modules/git.nix

      # gui
      ./modules/chromium.nix
      ./modules/firefox.nix
      ./modules/gui.nix
      ./modules/vscodium.nix
    ];

    extraSpecialArgs = {
      inherit inputs;
    };
  };

  work = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        inputs.emacs.overlay
        inputs.nur.overlay
      ];
      config = {
        allowUnfree = true;
      };
    };
    modules = [
      inputs.nixDoomEmacs.hmModule
      inputs.nixColors.homeManagerModule
    ]
    ++ [
      ./modules
      ./modules/bash.nix
      ./modules/cli.nix
      ./modules/doom
      ./modules/git.nix
    ];

    extraSpecialArgs = {
      inherit inputs;
    };
  };

}
