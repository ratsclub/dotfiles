{ pkgs, ... }:

let
  personal = {
    email = "victor@freire.dev.br";
    name = "Victor Freire";
  };
in
{
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
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

  home.packages = with pkgs; [
    # atlassian cli for bitbucket
    acli
    git-town
  ];

  programs.gh = {
    enable = true;
  };

  programs.jujutsu = {
    enable = true;
  };

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

      # claude
      ".claude"
      ".mcp.json"

      # vscode
      ".vscode"
    ];

    settings = {
      alias = {
        ca = "commit --amend";
        cm = "commit -m";
        co = "checkout";
        cu = "!f(){ git stash && git checkout $1 && git fetch --all --prune && git pull origin $1; };f";
        df = "diff";
        hist = "log --graph --pretty=format:'%Cred%h%Creset %s%C(yellow)%d%Creset %Cgreen(%cr)%Creset [%an]' --abbrev-commit --date=relative";
        ri = "rebase --interactive --autosquash";
        squash-all = ''!f(){ git reset $(git commit-tree HEAD^{tree} -m "''${1:-A new start}");};f'';
        st = "status --short --branch";
      };
      rerere.enabled = true;
      merge = {
        conflictstyle = "zdiff3";
      };
      github = {
        user = "ratsclub";
      };
    };

    includes = [
      {
        condition = "gitdir:~/Projects/Personal/";
        contents.user = personal;
      }
      {
        condition = "gitdir:~/Projects/Work/";
        contents.user = {
          email = "victor@theformfactory.co";
          name = "Victor Freire";
        };
      }
      # The org vault sits outside ~/Projects, and its commits are made
      # unattended by the sync in the emacs module, so it needs an identity of
      # its own rather than inheriting one.
      {
        condition = "gitdir:~/org/";
        contents.user = personal;
      }
    ];
  };
}
