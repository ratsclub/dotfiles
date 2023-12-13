{ config, pkgs, super, ... }:

let
  inherit (pkgs)
    nixpkgs-fmt
    nil
    rust-analyzer
    ;
in
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    userSettings = {
      "update.mode" = "none";

      "editor.formatOnSave" = false;
      "editor.linkedEditing" = true;
      "editor.rulers" = [ 80 120 ];

      # excluded files
      "files.exclude" = {
        # removes these from the search
        "**/.direnv" = true;
        "**/.devenv" = true;
      };

      "workbench.tree.indent" = 15;
      "workbench.colorTheme" = "Default Dark Modern";

      "terminal.integrated.tabs.enabled" = true;

      "window.titleBarStyle" = "custom";
      "window.zoomLevel" = 0;

      # F#
      "FSharp.inlayHints.enabled" = false;
      "FSharp.inlayHints.typeAnnotations" = false;
      "FSharp.inlayHints.parameterNames" = false;

      # HTML
      "[html]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };

      # Nix
      "nix" = {
        "enableLanguageServer" = true;
        "formatterPath" = "${nixpkgs-fmt}/bin/nixpkgs-fmt";
        "serverPath" = "${nil}/bin/nil";
      };
      "[nix]" = {
        "editor.insertSpaces" = true;
        "editor.tabSize" = 2;
      };

      # Rust
      "rust-analyzer.server.path" = "${rust-analyzer}/bin/rust-analyzer";
    };

    extensions = with pkgs.vscode-extensions; [
      # .NET
      ionide.ionide-fsharp
      ms-dotnettools.csharp

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
      ms-pyright.pyright

      # Rust
      matklad.rust-analyzer

      # SML
      azdavis.millet

      # Markdown
      yzhang.markdown-all-in-one

      # Misc
      eamodio.gitlens
      editorconfig.editorconfig
      esbenp.prettier-vscode
      gruntfuggly.todo-tree
      mkhl.direnv
    ];
  };

  programs.vscode.userSettings.editor.fontFamily = "Jetbrains Mono";
  home.packages = with pkgs; [ jetbrains-mono ];
}
