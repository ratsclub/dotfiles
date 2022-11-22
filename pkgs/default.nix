{ pkgs, ... }:

{
  readarr = pkgs.callPackage ./readarr.nix { };
}
