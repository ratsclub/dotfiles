{ super, lib, ... }:

{
  imports = [
    ../bash.nix
    ../cli.nix
    ../git.nix
    ../neovim.nix
    ../vscode.nix
    ../firefox.nix
  ];

  programs.home-manager.enable = true;

  home.stateVersion = "20.09";
}
