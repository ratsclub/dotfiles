{ super, lib, pkgs, ... }:

let
  isGraphical = super.device.type == "graphical";
in
{
  programs.vscode = {
    enable = isGraphical;
    package = pkgs.vscodium;
    userSettings = {
      # auto update tags when edited
      "editor.linkedEditing" = true;
      "editor.rulers" = [ 80 120 ];
      "editor.formatOnSave" = true;
      "editor.fontFamily" = "Jetbrains Mono";

      "workbench.colorTheme" = "GitHub Dark";
      "workbench.iconTheme" = "material-icon-theme";

      "window.titleBarStyle" = "custom";
      "window.zoomLevel" = 0;
      "terminal.integrated.tabs.enabled" = true;
      "[html]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };

      # Nix
      "nix.enableLanguageServer" = true;
      "[nix]" = {
        "editor.tabSize" = 2;
        "editor.insertSpaces" = true;
      };
    };

    extensions = with pkgs.vscode-extensions; [
      # themes
      github.github-vscode-theme
      pkief.material-icon-theme

      # Angular
      angular.ng-template

      # Deno
      denoland.vscode-deno

      # Nix
      jnoortheen.nix-ide

      # Go
      golang.go

      # Python
      ms-python.python
      ms-toolsai.jupyter

      # Rust
      matklad.rust-analyzer

      # Markdown
      yzhang.markdown-all-in-one

      # Misc
      eamodio.gitlens
      editorconfig.editorconfig
      esbenp.prettier-vscode
      usernamehw.errorlens
    ];
  };
}
