{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in
{
  programs = {
    aria2.enable = true;
    bat.enable = true;
    jq.enable = true;
  };

  home.packages = with pkgs; [
    aria2
    bat
    entr
    fd
    fzf
    gcc
    htop
    hut
    jq
    ncdu
    nixpkgs-fmt
    ripgrep
    wget

    # lsp
    nil
  ];

  programs.fzf.enable = true;
  home.sessionVariables = {
    EDITOR = "emacs -nw";
    FZF_DEFAULT_OPTS = ''--prompt \" λ \"'';
  };
}
