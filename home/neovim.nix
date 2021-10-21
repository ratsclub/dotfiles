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
      telescope-nvim
      vim-polyglot
    ];

    # https://github.com/nix-community/home-manager/pull/2391
    extraConfig = ''
      " show numbers
      set number

      " show relative numbers
      set relativenumber

      " color scheme
      colorscheme pablo

      " space as leader key
      nnoremap <space> <nop>
      let mapleader=' '

      " escape key
      inoremap jk <Esc>
      " Find files using Telescope command-line sugar.
      nnoremap <leader>ff <cmd>Telescope find_files<cr>
      nnoremap <leader>fg <cmd>Telescope live_grep<cr>
      nnoremap <leader>fb <cmd>Telescope buffers<cr>
      nnoremap <leader>fh <cmd>Telescope help_tags<cr>
    '';
  };
}
