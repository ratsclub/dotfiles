{ pkgs, ... }:

{
  programs = {
    bash = {
      enable = true;
      historySize = 10000;
      shellOptions = [
        # Append to history file rather than replacing it.
        "histappend"

        # check the window size after each command and, if
        # necessary, update the values of LINES and COLUMNS.
        "checkwinsize"

        # Extended globbing.
        "extglob"
        "globstar"

        # Warn if closing shell with running jobs.
        "checkjobs"
      ];

      historyControl = [
        "erasedups"
        "ignoredups"
        "ignorespace"
      ];

      shellAliases = {
        ls = "ls --color=auto";
        ll = "ls -l";
        la = "ls -A";
        lt = "ls --human-readable --size -1 -S --classify";
        l = "ls -CF";
        grep = "grep --color=auto";
        ".." = "cd ..";
        mknote = ">$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1).md";
      };

      sessionVariables = {
        EDITOR = "vi";
      };

      profileExtra = builtins.readFile ./.profile;
      bashrcExtra = builtins.readFile ./.bashrc;
    };

    git = {
      enable = true;
      package = pkgs.gitFull;

      userName = "Victor Freire";
      userEmail = "victor@freire.dev.br";

      delta = {
        enable = true;
        options = {
          features = "side-by-side line-numbers decorations";
          syntax-theme = "GitHub";
          decorations = {
            commit-decoration-style = "bold yellow box ul";
            file-style = "bold yellow ul";
            file-decoration-style = "none";
            hunk-header-decoration-style = "cyan box ul";
          };
          delta = {
            navigate = true;
          };
          line-numbers = {
            line-numbers-left-style = "cyan";
            line-numbers-right-style = "cyan";
            line-numbers-minus-style = 124;
            line-numbers-plus-style = 28;
          };
        };
      };
    };

    aria2.enable = true;
    bat.enable = true;
    exa.enable = true;
    fzf.enable = true;
    jq.enable = true;
    lazygit.enable = true;
  };

  home.packages = with pkgs; [
    fd
    ripgrep

    # Nix
    nixpkgs-fmt
    rnix-lsp
  ];
}
