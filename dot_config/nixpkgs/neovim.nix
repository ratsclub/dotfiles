{ pkgs, config, ... }:

let 
  cocSettings = {
    languageserver = {
      nix = {
        command = "${pkgs.rnix-lsp}/bin/rnix-lsp";
        filetypes = [ "nix" ];
      };

      lua = {
        command = "${pkgs.luaPackages.lua-lsp}/bin/lua-lsp";
        filetypes = [ "lua" ];
      };
    };
  };
in {
  xdg.configFile."nvim/coc-settings.json".text = builtins.toJSON cocSettings;

  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    extraConfig = builtins.readFile ../nvim/config.vim;
    
    plugins = with pkgs.vimPlugins; [
      coc-go
      coc-nvim
      coc-tsserver
      fzf-vim
      vim-nix
    ];
  };
}
