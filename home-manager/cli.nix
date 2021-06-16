{ super, config, lib, pkgs, ... }:

{
  programs = {
    bat.enable = true;
    exa.enable = true;
    jq.enable = true;
  };

  home.packages = with pkgs; [
    fd
    nixpkgs-fmt
    ripgrep
  ];
}
