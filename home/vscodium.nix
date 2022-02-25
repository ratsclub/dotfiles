{ super, lib, pkgs, ... }:

let
  inherit (pkgs) rust-analyzer;
in
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    userSettings = {
      # auto update tags when edited
      "editor.linkedEditing" = true;
      "editor.rulers" = [ 80 120 ];
      "editor.formatOnSave" = true;
      "workbench.tree.indent" = 15;

      "editor.fontFamily" = "Jetbrains Mono";
      "workbench.colorTheme" = "Tomorrow Night";
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

      # Rust
      "rust-analyzer.server.path" = "${rust-analyzer}/bin/rust-analyzer";

      # Svelt
      "svelte.enable-ts-plugin" = true;
    };

    extensions = with pkgs.vscode-extensions; [
      # themes
      github.github-vscode-theme
      pkief.material-icon-theme
      ms-vscode.theme-tomorrowkit

      # Angular
      angular.ng-template

      # .NET
      ionide.ionide-fsharp
      ms-dotnettools.csharp

      # Deno
      denoland.vscode-deno

      # Nix
      jnoortheen.nix-ide

      # Go
      golang.go

      # Python
      ms-python.python
      ms-toolsai.jupyter
      ms-pyright.pyright

      # Rust
      matklad.rust-analyzer

      # Svelte
      svelte.svelte-vscode

      # Markdown
      yzhang.markdown-all-in-one

      # Misc
      eamodio.gitlens
      editorconfig.editorconfig
      esbenp.prettier-vscode
      usernamehw.errorlens
    ];
  };

  home.packages = with pkgs; [
    jetbrains-mono
  ];
}
