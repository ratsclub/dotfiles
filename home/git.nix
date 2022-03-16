{ config, pkgs, ... }:

let
  inherit (config.meta) email name;
in
{
  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    userName = name;
    userEmail = email;

    aliases = {
      ca = "commit --amend";
      cm = "commit -m";
      co = "checkout";
      df = "diff";
      ri = "rebase --interactive --autosquash";
      st = "status --short --branch";
      hist = "log --graph --pretty=format:'%Cred%h%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)%Creset [%an]' --abbrev-commit --date=relative";
      squash-all = ''!f(){ git reset $(git commit-tree HEAD^{tree} -m "''${1:-A new start}");};f'';
    };

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers decorations";
        delta = {
          navigate = true;
        };
        line-numbers = {
          line-numbers-minus-style = 124;
          line-numbers-plus-style = 28;
        };
      };
    };
  };
}
