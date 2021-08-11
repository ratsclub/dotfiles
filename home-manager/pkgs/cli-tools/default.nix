{ pkgs, ... }:

{
  programs = {
    bat.enable = true;
    exa.enable = true;
    jq.enable = true;

    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
  };

  home.packages = with pkgs; [
    fd
    lazygit
    ripgrep
    aria2
  ];
}
