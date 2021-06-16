{ config, pkgs, ... }:

{
  xdg.enable = true;

  imports = [
    ./bash.nix
    ./neovim.nix
  ];

  programs = {
    bat.enable = true;
    broot.enable = true;
    exa.enable = true;
    jq.enable = true;

    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Victor Freire";
      userEmail = "victor@freire.dev.br";
    };
  };

  home.packages = with pkgs; [
    beancount
    chezmoi
    fava
    fd
    nixpkgs-fmt
    ripgrep
    zellij
  ];

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  home.stateVersion = "21.05";
}
