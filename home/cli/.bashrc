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

# Provide a nice prompt if the terminal supports it.
if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
  PROMPT_COLOR="1;31m"
  let $UID && PROMPT_COLOR="1;32m"
  if [ -n "$INSIDE_EMACS" -o "$TERM" == "eterm" -o "$TERM" == "eterm-color" ]; then
    # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
    PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
  else
    PS1="\n\[\033[$PROMPT_COLOR\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\\$\[\033[0m\] "
  fi
  if test "$TERM" = "xterm"; then
    PS1="\[\033]2;\h:\u:\w\007\]$PS1"
  fi
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
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
        PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt
                                                                                                                
# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

if ! shopt -oq posix; then
        if [ -f /usr/share/bash-completion/bash_completion ]; then
                . /usr/share/bash-completion/bash_completion
        elif [ -f /etc/bash_completion ]; then
                . /etc/bash_completion
        fi
fi
