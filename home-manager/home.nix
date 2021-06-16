{ super, lib, ... }:

{
  imports = [
    ./git.nix
    ./cli.nix
    ./bash.nix
  ];

  programs.home-manager.enable = true;

  home.stateVersion = "20.09";
}
