{ inputs, pkgs, ... }:

let
  configFile = ./init.el;
  customEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = configFile;
    package = pkgs.emacsGit;
    alwaysEnsure = true;
  };
in
{
  programs.emacs = {
    enable = true;
    package = customEmacs;
  };

  home.file.".emacs.d/init.el".source = configFile;
}
