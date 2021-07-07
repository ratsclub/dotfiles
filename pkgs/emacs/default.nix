{ pkgs, ... }:

let
  emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages;
in
emacsWithPackages
  (epkgs: (with epkgs.melpaPackages; [
    doom-themes
    magit
    smex
    use-package

    # languages
    nix-mode
    which-key
  ]) ++ (with epkgs.elpaPackages; [
    company
    counsel
    org
    swiper
  ]))
