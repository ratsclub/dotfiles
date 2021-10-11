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

    plugins = with pkgs.vimPlugins; [
      {
        plugin = telescope-nvim;
      }
      {
        plugin = vim-noctu;
      }
      {
        plugin = ale;
      }
      
      vim-polyglot
    ];

    extraConfig = builtins.readFile ./config.vim;
  };
}
