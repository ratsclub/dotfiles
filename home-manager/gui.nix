# Use this file to manage systems with a graphical interface
{ pkgs, inputs, system, ... }:

let
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.nur.overlay
    ];
    config = {
      allowUnfree = true;
    };
  };
in
{
  imports = [
    ./pkgs/bash
    ./pkgs/cli-tools
    ./pkgs/direnv
    ./pkgs/emacs
    ./pkgs/firefox
    ./pkgs/git
    ./pkgs/neovim
    ./pkgs/newsboat
    ./pkgs/vscode
  ];

  programs.home-manager.enable = true;
  fonts.fontconfig.enable = true;

  home.username = "ratsclub";
  home.homeDirectory = "/home/ratsclub";

  dconf =
    let
      wallpaper = "${pkgs.nixos-artwork.wallpapers.nineish-dark-gray}/share/wallpapers/nineish-dark-gray-2020-07-02/contents/images/nix-wallpaper-nineish-dark-gray.png";
    in
    {
      enable = true;
      settings = {
        "org/gnome/desktop/peripherals/mouse" = {
          "left-handed" = true;
        };

        "org/gnome/desktop/background" = {
          "picture-uri" = wallpaper;
        };

        "org/gnome/nautilus/list-view" = {
          "use-tree-view" = true;
          "default-zoom-level" = "small";
        };
      };
    };

  home.packages = with pkgs; [
    discord
    tdesktop
    bitwarden
    obsidian
  ];

  home.stateVersion = "20.09";
}
