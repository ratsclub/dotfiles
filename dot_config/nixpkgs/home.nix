{ config, pkgs, ... }:

{
  xdg.enable = true;
  
  imports = [ 
    ./bash.nix 
  ];

  programs = {
    bat.enable = true;
    home-manager.enable = true;

    git = {
      enable = true;
      userName = "Victor Freire";
      userEmail = "victor@freire.dev.br";
    };

    neovim = {
      enable = true;
      vimAlias = true;
      viAlias = true;

      extraConfig = builtins.readFile ../nvim/config.vim;
      plugins = with pkgs.vimPlugins; [
        coc-nvim
        fzf-vim
        vim-nix
      ];
    };
  };

  home.packages = with pkgs; [
    ripgrep
    fd
    chezmoi
    nixpkgs-fmt
    nodejs
    beancount
    fava
  ];

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  home.stateVersion = "21.05";
}
