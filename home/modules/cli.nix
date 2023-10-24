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
    convco
    entr
    fd
    fzf
    htop
    hut
    jq
    nixpkgs-fmt
    ripgrep
    wget
  ];

  programs.fzf.enable = true;
  home.sessionVariables = {
    EDITOR = "emacs -nw";
    FZF_DEFAULT_OPTS = ''--prompt \" λ \"'';
  };
}
