{
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    extraConfig = builtins.readFile ./config.vim;
  };
}
