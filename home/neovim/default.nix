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
      ale
      telescope-nvim
      vim-airline
      vim-noctu
      vim-polyglot
    ];

    # https://github.com/nix-community/home-manager/pull/2391
    extraConfig = builtins.readFile ./config.vim;
  };
}
