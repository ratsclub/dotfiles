{ inputs, self, ... }:

{
  victor = inputs.homeManager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {
      system = "x86_64-linux";
      overlays = [
        inputs.nur.overlay
        self.overlays.modifications
        inputs.nur.overlay
      ];
      config = {
        allowUnfree = true;
      };
    };
    modules = [
      inputs.nixColors.homeManagerModule
      inputs.agenix.homeManagerModules.age
      inputs.nixColors.homeManagerModule
    ]
    ++ [
      ./modules
      ./modules/bash.nix
      ./modules/chromium.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      # ./modules/doom
      ./modules/emacs
      ./modules/email.nix
      # ./modules/firefox.nix
      ./modules/git.nix
      ./modules/vscodium.nix
    ];

    extraSpecialArgs = {
      inherit inputs;
    };
  };
}
