# dotfiles

This is where I host my dotfiles.

<img src="https://img.shields.io/badge/-Nix-informational?style=for-the-badge&logo=NixOS&logoColor=white&color=5277C3" alt="Nix" />

<img src="https://img.shields.io/badge/-Bash-informational?style=for-the-badge&logo=gnu-bash&logoColor=white&color=4EAA25" alt="Gnu Bash" />

<img src="https://img.shields.io/badge/-Neovim-informational?style=for-the-badge&logo=gnu-bash&logoColor=white&color=57A143" alt="Neovim" />

## About

I use [Nix][] as my package manager and [Home Manager][] to manage part of my user environment with the help of [Chezmoi][] to organize and put my dotfiles in place.   

## Getting Started

This repository uses [chezmoi](https://chezmoi.io) to manage the files. You can start it using the following command:

```shell
chezmoi init https://github.com/ratsclub/dotfiles
```

[chezmoi]: https://chezmoi.io
[Nix]: https://nixos.org
[Home Manager]: https://github.com/nix-community/home-manager
