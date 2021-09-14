{ pkgs, ... }:

let
  myEmacs = pkgs.emacs-nox;
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
in
{
  programs.emacs = {
    enable = true;
    extraConfig = builtins.readFile ./default.el;
    package = emacsWithPackages
      (epkgs: (with epkgs.melpaPackages; [
        magit
        counsel
      ]) ++ (with epkgs.elpaPackages; [
        beacon
      ]) ++ [
      ]);
  };
}
