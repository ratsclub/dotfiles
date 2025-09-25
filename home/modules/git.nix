{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    ignores = [
      # nix
      "result"

      # direnv/devenv
      ".envrc"
      ".direnv"
      ".devenv"
      ".pre-commit-config.yaml"

      # jetbrains
      ".idea"

      # vscode
      ".vscode"
    ];

    aliases = {
      ca = "commit --amend";
      cm = "commit -m";
      co = "checkout";
      cu = ''!f(){ git stash && git checkout $1 && git fetch --all --prune && git pull origin $1; };f'';
      df = "diff";
      hist = "log --graph --pretty=format:'%Cred%h%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)%Creset [%an]' --abbrev-commit --date=relative";
      ri = "rebase --interactive --autosquash";
      squash-all = ''!f(){ git reset $(git commit-tree HEAD^{tree} -m "''${1:-A new start}");};f'';
      st = "status --short --branch";
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

    includes = [
      {
        condition = "gitdir:~/Projects/Personal/";
        contents.user = {
          email = "victor@freire.dev.br";
          name = "Victor Freire";
        };
      }
      {
        condition = "gitdir:~/Projects/Work/";
        contents.user = {
          email = "victor@theformfactory.co";
          name = "Victor Freire";
        };
      }
    ];

    extraConfig = {
      rerere.enabled = true;
      merge = {
        conflictstyle = "diff3";
      };
      github = {
        user = "ratsclub";
      };
    };
  };
}
