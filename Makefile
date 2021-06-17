#! /usr/bin/env nix-shell
#! nix-shell -I "nixpkgs=channel:nixpkgs-unstable" -i "make -f" -p gnumake nixUnstable

.PHONY: update
NIX_FLAGS := --experimental-features 'nix-command flakes'

update:
	nix $(NIX_FLAGS) flake update --commit-lock-file