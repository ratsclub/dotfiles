{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    # package = pkgs.neovim;
    plugins = with pkgs.vimPlugins; [
      telescope-nvim
      vim-polyglot
    ];

    extraConfig = builtins.readFile ./config.vim;
  };
}
