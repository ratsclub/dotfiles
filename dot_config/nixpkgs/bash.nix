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
        l = "ls -CF";
        grep = "grep --color=auto";
        ".." = "cd ..";
      };

      profileExtra = ''
        # if running bash
        if [ -n "$BASH_VERSION" ]; then
            # include .bashrc if it exists
            if [ -f "$HOME/.bashrc" ]; then
                . "$HOME/.bashrc"
            fi
        fi

        # set PATH so it includes user's private bin if it exists
        if [ -d "$HOME/bin" ] ; then
            PATH="$HOME/bin:$PATH"
        fi

        # set PATH so it includes user's private bin if it exists
        if [ -d "$HOME/.local/bin" ] ; then
            PATH="$HOME/.local/bin:$PATH"
        fi

        # added by nix installer
        if [ -e /home/victor/.nix-profile/etc/profile.d/nix.sh ]; then . /home/victor/.nix-profile/etc/profile.d/nix.sh; fi
        if [ -d "$HOME/.cargo" ] ; then
            source "$HOME/.cargo/env"
        fi

        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
      '';

      bashrcExtra = ''
        # If not running interactively, don't do anything
        case $- in
            *i*) ;;
              *) return;;
        esac

        # make less more friendly for non-text input files, see lesspipe(1)
        [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

        # set variable identifying the chroot you work in (used in the prompt below)
        if [ -z "\$\{debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
            debian_chroot=$(cat /etc/debian_chroot)
        fi

        force_color_prompt=yes
        if [ -n "$force_color_prompt" ]; then
            if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
                color_prompt=yes
            else
                color_prompt=
            fi
        fi

        if [ "$color_prompt" = yes ]; then
            PS1=' ''${debian_chroot:+(''$debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
        else
            PS1=' ''${debian_chroot:+(''$debian_chroot)}\u@\h:\w\$ '
        fi
        unset color_prompt force_color_prompt

        # colored GCC warnings and errors
        export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

        # enable programmable completion features (you don't need to enable
        # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
        # sources /etc/bash.bashrc).
        if ! shopt -oq posix; then
          if [ -f /usr/share/bash-completion/bash_completion ]; then
            . /usr/share/bash-completion/bash_completion
          elif [ -f /etc/bash_completion ]; then
            . /etc/bash_completion
          fi
        fi
      '';
    };
      
    fzf = {
      enable = true;
      enableBashIntegration = true;
    };
  };
}
