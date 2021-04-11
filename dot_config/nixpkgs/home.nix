{ config, pkgs, ... }:

{
  programs = {
    bat.enable = true;
    home-manager.enable = true;

    fzf = {
      enable = true;
      enableBashIntegration = true;
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

    # ledger
    beancount
    fava
  ];

  home.username = "victor";
  home.homeDirectory = "/home/victor";

  home.stateVersion = "21.05";
}
