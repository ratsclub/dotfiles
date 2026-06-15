{ pkgs, ... }:
{
  qmd = pkgs.callPackage ./qmd { };
  reasonix = pkgs.callPackage ./reasonix { };
}
