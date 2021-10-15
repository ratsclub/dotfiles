# dotfiles

![OS NixOS](https://img.shields.io/badge/os-nixos%20|%20ubuntu-%235277c3?style=flat-square&logoColor=7eb5e0)
![Shell Bash](https://img.shields.io/badge/shell-bash-%234caa20?style=flat-square)
![Shell Bash](https://img.shields.io/badge/editor-neovim%20|%20vscodium-%23464748?style=flat-square)

Home to my personal configuration files.

<p align="center">
    <img src="assets/nixos.gif" width=40 alt="i use nixos btw">
</p>
<p align="center"><em>I use nixos btw</em></p>

## Technologies

- [Nix] as package manager.
- [Home Manager][] to take care of my home directory on [NixOS][] and non-NixOS.
- [VSCodium](https://vscodium.com/) and [Neovim](https://neovim.io/) as code
  editors.

## Repository structure

- `assets`: everything that is not code such as images, videos and others;
- `home`: [Home Manager][] configuration;
- `hosts`: hosts configurations;
- `lib`: collection of functions and attributes I use extensively throughout my
  configurations;
- `modules`: [Nix Modules][];
- `overlays`: [Nix Overlays][];
- `templates`: [Nix Flake][] templates that I use to start new projects or package
  existing ones.
  - Run `nix flake init -t github:ratsclub/dotfiles#templates.<template>` to
    use the templates. See the [default.nix file](./templates/default.nix) for a
    complete list of available templates.

[NixOS]: https://nixos.org
[Nix]: https://nixos.org
[Home Manager]: https://github.com/nix-community/home-manager/
[Nix Modules]: https://nixos.wiki/wiki/Module
[Nix Overlays]: https://nixos.wiki/wiki/Overlays
[Nix Flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
