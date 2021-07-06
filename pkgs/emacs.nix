{ pkgs, ... }:

let
  emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages;
in
emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
  doom-themes
  magit
  nix-mode
  use-package
]) ++ (with epkgs.elpaPackages; [
  company
  org
  swiper
]))
