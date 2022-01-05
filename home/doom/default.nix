{pkgs, ...}:

{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./config;
  };
}
