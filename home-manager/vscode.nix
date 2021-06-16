{ pkgs, config, ... }:

let
  settings = builtins.fromJSON ../config/vscode/settings.json;
in {
    programs.vscode = {
        enable = true;
        package = pkgs.vscodium;
        userSettings = settings;
    };
}