{ config, pkgs, ... }:

{
  programs = {
    bat.enable = true;
    home-manager.enable = true;

    fzf = {
      enable = true;
      enableBashIntegration = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacsGcc;
    };

    git = {
      enable = true;
      userEmail = "victor@freire.dev.br";
      userName = "Victor Freire";
    };
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    chezmoi
    sqlite
  ];

  home.username = "victor";
  home.homeDirectory = "/home/victor";

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  home.stateVersion = "21.05";
}
