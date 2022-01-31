{ pkgs, ... }:

{
  programs = {
    fzf.enable = true;
  };

  home.packages = with pkgs; [
    # Nix
    nixpkgs-fmt
    rnix-lsp
  ];
}
