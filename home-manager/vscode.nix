{ pkgs, config, ... }:

{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    userSettings = {
      "editor.rulers" = [ 80 120 ];
      "workbench.colorTheme" = "Solarized Dark";
      "window.titleBarStyle" = "custom";
    };
    extensions = with pkgs.vscode-extensions; [
      # Nix
      jnoortheen.nix-ide

      # Go
      golang.Go

      # Rust
      matklad.rust-analyzer

      # Markdown
      yzhang.markdown-all-in-one
    ];
  };
}
