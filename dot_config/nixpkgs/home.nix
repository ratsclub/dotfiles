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
    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Victor Freire";
      userEmail = "victor@freire.dev.br";
    };

    emacs = {
      enable = true;
      package = pkgs.emacs-nox;
    };
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    chezmoi
    nixpkgs-fmt
    beancount
    fava
  ];

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  home.stateVersion = "21.05";
}
