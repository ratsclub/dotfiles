{ pkgs, ... }:

{
  home.packages = with pkgs.jetbrains; [
    goland
    pycharm-professional
    rider
  ];
}
