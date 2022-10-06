{
  programs.bash = {
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
      mknote = ''echo -e "---\ntitle:\ndate: $(date -u -Iseconds)\n---" > $(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1).md'';
    };

    bashrcExtra = ''
      # If not running interactively, don't do anything
      case $- in
        *i*) ;;
        *) return;;
      esac

      parse_git_branch() {
        git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
      }
      export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\e[91m\]\$(parse_git_branch)\[\033[00m\]\$ "

      # colored GCC warnings and errors
      export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

      if ! shopt -oq posix; then
        if [ -f /usr/share/bash-completion/bash_completion ]; then
          . /usr/share/bash-completion/bash_completion
        elif [ -f /etc/bash_completion ]; then
          . /etc/bash_completion
        fi
      fi
    '';

    profileExtra = ''
      [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && . $HOME/.nix-profile/etc/profile.d/nix.sh

      # useful for showing icons on non-NixOS systems
      export XDG_DATA_DIRS=$HOME/.nix-profile/share:$XDG_DATA_DIRS

      [ -d "$HOME/.local/bin" ] && export PATH=$PATH:$HOME/.local/bin
      [ -d "$HOME/.poetry/bin" ] && export PATH=$PATH:$HOME/.poetry/bin
      [ -d "$HOME/go/bin" ] && export PATH=$PATH:$HOME/go/bin
      [ -d "$HOME/.dotnet/tools" ] && export PATH=$PATH:$HOME/.dotnet/tools
    '';
  };
}
