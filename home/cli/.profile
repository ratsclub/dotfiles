# if runnin bash
if [ -n "$BASH_VERSION" ]; then
        # include .bashrc if it exists
        if [ -f "$HOME/.bashrc" ]; then
                . "$HOME/.bashrc"
        fi
fi

if [ -d "$HOME/bin" ] ; then
        PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
        PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ] ; then
        PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/go/bin" ] ; then
        PATH="$HOME/go/bin:$PATH"
fi

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi
export XDG_DATA_DIRS=$HOME/.nix-profile/share:$XDG_DATA_DIRS 
