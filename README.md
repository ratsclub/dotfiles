# dotfiles

This is where I host my dotfiles.

![Nix](https://img.shields.io/badge/-Nix-informational?style=for-the-badge&logo=NixOS&logoColor=white&color=5277C3) ![](https://img.shields.io/badge/-Bash-informational?style=for-the-badge&logo=gnu-bash&logoColor=white&color=4EAA25) ![](https://img.shields.io/badge/-Neovim-informational?style=for-the-badge&logo=gnu-bash&logoColor=white&color=57A143)

## About

I use [Nix][] as my package manager and [Home Manager][] to manage part of my user environment with the help of [Chezmoi][] to organize and put my dotfiles in place.   

## Getting Started

You can start using it the following command:

```shell
chezmoi init https://github.com/ratsclub/dotfiles
```

[chezmoi]: https://chezmoi.io
[Nix]: https://nixos.org
[Home Manager]: https://github.com/nix-community/home-manager
