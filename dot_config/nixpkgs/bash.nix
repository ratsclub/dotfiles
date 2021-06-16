{ pkgs, config, ... }:

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

        # configuration
        ce = "chezmoi edit $(chezmoi managed --include=files | sed -e 's/^/~\\//' | fzf)";
        ca = "chezmoi apply -v && home-manager switch";
      };

      sessionVariables = {
        EDITOR = "vi";
      };

      profileExtra = builtins.readFile ../bash/.profile;
      bashrcExtra = builtins.readFile ../bash/.bashrc;
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
