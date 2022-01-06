{ config, pkgs, inputs, ... }:

let 
  inherit (inputs) nix-colors;
  inherit (nix-colors.lib { inherit pkgs; }) vimThemeFromScheme;
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    plugins = with pkgs.vimPlugins; [
      {
        plugin = vimThemeFromScheme { scheme = config.colorscheme; };
        config = "colorscheme nix-${config.colorscheme.slug}";
      }
      { plugin = telescope-nvim; }
      { plugin = vim-polyglot; }
    ];

    # https://github.com/nix-community/home-manager/pull/2391
    extraConfig = ''
      " show column ruler
      set colorcolumn=80

      " show numbers
      set number

      " show relative numbers
      set relativenumber

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
