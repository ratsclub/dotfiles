{
  programs.git.enable = true;

  system.autoUpgrade = {
    enable = true;
    flake = "git+https://code.capivaras.dev/ratsclub/nix-config.git";
    dates = "daily";
  };
}
