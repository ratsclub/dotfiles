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
        plugin = vim-polyglot;
        config = ''
          " Find files using Telescope command-line sugar.
          nnoremap <leader>ff <cmd>Telescope find_files<cr>
          nnoremap <leader>fg <cmd>Telescope live_grep<cr>
          nnoremap <leader>fb <cmd>Telescope buffers<cr>
          nnoremap <leader>fh <cmd>Telescope help_tags<cr>
        '';
      }
      {
        plugin = vim-noctu;
        config = ''
          " color scheme
          colorscheme noctu
        '';
      }
      {
        plugin = ale;
        config = ''
          let g:ale_completion_enabled = 1
          let g:ale_linters = {"c": ["clang"], "rust": ["analyzer", "cargo"]}
          let g:ale_fixers = {"rust": ["rustfmt"], "sql": ["pgformatter"], "nix": ["nixfmt"], "json": ["jq"]}
          let g:ale_rust_analyzer_config = {'checkOnSave': {'command': 'clippy', 'enable': v:true}}
        '';
      }
      
      vim-polyglot
    ];

    extraConfig = ''
      " show numbers
      set nu

      " show relative numbers
      set rnu

      " space as leader key
      nnoremap <space> <nop>
      let mapleader=" "

      " escape key
      inoremap jk <Esc>
    '';
  };
}
