{
  inputs,
  pkgs,
  self,
  ...
}:

{
  nix = {
    package = pkgs.nix;

    gc = {
      automatic = true;
      options = "--delete-older-than 1w";
    };

    registry.nixpkgs.flake = inputs.nixpkgs;
    registry.self.flake = self;

    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];

      extra-trusted-substituters = [
        "https://devenv.cachix.org"
        "https://cache.numtide.com"
      ];

      extra-trusted-public-keys = [
        # "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
      ];
    };
  };
}
