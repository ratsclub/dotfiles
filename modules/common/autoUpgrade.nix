{
  programs.git.enable = true;

  system.autoUpgrade = {
    enable = true;
    flake = "github:ratsclub/dotfiles";
    dates = "daily";
  };
}
