{
  config,
  inputs,
  pkgs,
  ...
}:

{
  nix = {
    package = pkgs.nix;

    gc = {
      automatic = true;
      options = "--delete-older-than 1w";
    };

    settings = {
      experimental-features = "nix-command flakes";
      auto-optimise-store = true;
      trusted-users = [
        "root"
        "@wheel"
      ];
    };
  };
}
