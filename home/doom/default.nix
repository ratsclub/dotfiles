{ pkgs, ... }:

let
  emacsPkg = pkgs.emacsUnstable;
in
{
  home.packages = with pkgs; [
    # font
    jetbrains-mono
    # org-roam
    graphviz
  ];

  services.emacs.enable = true;
  programs.doom-emacs = {
    enable = true;
    emacsPackage = emacsPkg;
    doomPrivateDir = ./config;
  };
}
