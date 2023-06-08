{ inputs, pkgs, ... }:

let
  emacsConfig = pkgs.writeTextDir "config/init.el" ''
    (load "${./init.el}")
  '';

  customEmacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    alwaysEnsure = true;
    package =
      pkgs.emacs-unstable.overrideAttrs (old:
        { withTreeSitter = true; });
    extraEmacsPackages = epkgs: [
      epkgs.treesit-grammars.with-all-grammars
    ];
  };
in
{
  programs.emacs = {
    enable = true;
    package = customEmacs;
  };

  home.file.".emacs.d/init.el".source = ./init.el;
}
