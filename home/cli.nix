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

  home.packages = (import ../pkgs/cli.nix { inherit pkgs; });

  programs.fzf.enable = true;
  home.sessionVariables = {
    FZF_DEFAULT_OPTS = ''--prompt \" λ \"'';
  };
}
