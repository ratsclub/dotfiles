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

      "workbench.colorTheme" = "GitHub Dark";
      "workbench.iconTheme" = "material-icon-theme";

      "window.titleBarStyle" = "custom";
      "window.zoomLevel" = 0;
      "terminal.integrated.tabs.enabled" = true;
      "[html]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "nix.enableLanguageServer" = true;
      "editor.fontFamily" = "Jetbrains Mono";
    };
    extensions = with pkgs.vscode-extensions; [
      # Theme
      github.github-vscode-theme

      # Icons
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
      #ms-python.python
      #ms-toolsai.jupyter

      # Rust
      matklad.rust-analyzer

      # Markdown
      yzhang.markdown-all-in-one

      # Misc
      eamodio.gitlens
      esbenp.prettier-vscode
      usernamehw.errorlens
    ];
  };

  home.packages = with pkgs; [
    jetbrains-mono
  ];
}
