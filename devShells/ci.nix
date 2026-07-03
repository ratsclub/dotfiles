{ pkgs, ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    curl
    gitMinimal
    jq
    nix-update
  ];
}
