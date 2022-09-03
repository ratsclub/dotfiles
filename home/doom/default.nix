{ pkgs, ... }:

let
  inherit (pkgs) emacs;
in
{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./config;
    emacsPackage = emacs;
    emacsPackagesOverlay = final: prev: {
      ts-fold = prev.ts;
      tree-sitter-langs = prev.tree-sitter-langs.override { plugins = pkgs.tree-sitter.allGrammars; };
    };
  };
}
