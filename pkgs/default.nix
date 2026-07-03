{ pkgs, ... }:
{
  claude-statusline = pkgs.callPackage ./claude-statusline { };
  reasonix = pkgs.callPackage ./reasonix { };
}
