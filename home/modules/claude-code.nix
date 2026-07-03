{ pkgs, ... }:
let
  # Shopify developer tools: docs search, GraphQL/Liquid/UI-extension codegen.
  shopify-ai-toolkit = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "2de64b683f8120e215e783fbee12aa037ce77f55";
    hash = "sha256-n7g4CnOFxVvTWPl9SSl4GjL7yJK+JwIRNR9jDJIFK4w=";
  };

  # Anthropic's official marketplace repo; several plugins live in subdirs of it.
  claude-plugins-official = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "claude-plugins-official";
    rev = "1a8324639ea08fb605600a3d5b095cd59e7e57f2";
    hash = "sha256-qg37jdb/jPRCzNRgpd/TTb6VWE48Wh3XfsWH6K5k1lM=";
  };

  # Anthropic's official skills repo; individual skills live under skills/.
  anthropic-skills = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "skills";
    rev = "9d2f1ae187231d8199c64b5b762e1bdf2244733d";
    hash = "sha256-U7Nt1xrFOSOEm4vuWmy4pVsEyvv+Hj4sv8yXOofmwAw=";
  };
in
{
  programs.claude-code = {
    enable = true;

    plugins = [
      shopify-ai-toolkit
      "${claude-plugins-official}/plugins/code-review"
      "${claude-plugins-official}/plugins/playground"
    ];

    skills = {
      skill-creator = "${anthropic-skills}/skills/skill-creator";
    };

    settings = {
      attribution = {
        commit = "";
        pr = "";
      };

      permissions.deny = [
        "Edit(//nix/store/**)"
        "Write(//nix/store/**)"
      ];

      tui = "fullscreen";

      statusLine = {
        type = "command";
        command = pkgs.lib.getExe pkgs.claude-statusline;
        padding = 0;
        refreshInterval = 10;
      };
    };

    context = ''
      # Fetching packages

      This machine is managed with Nix. To use a CLI tool that isn't already on
      the PATH, don't install it globally or with a system package manager. Run
      it on demand with `nix run`, e.g. `nix run nixpkgs#<pkg> -- <args>`, or
      drop into an ephemeral shell with `nix shell nixpkgs#<pkg>` when you need
      several invocations.
    '';

    lspServers = {
      nix = {
        command = "${pkgs.nixd}/bin/nixd";
        args = [ ];
        extensionToLanguage = {
          ".nix" = "nix";
        };
      };
      go = {
        command = "${pkgs.gopls}/bin/gopls";
        args = [ "serve" ];
        extensionToLanguage = {
          ".go" = "go";
        };
      };
      typescript = {
        command = "${pkgs.typescript-language-server}/bin/typescript-language-server";
        args = [ "--stdio" ];
        extensionToLanguage = {
          ".js" = "javascript";
          ".jsx" = "javascriptreact";
          ".ts" = "typescript";
          ".tsx" = "typescriptreact";
        };
      };
      python = {
        command = "${pkgs.ty}/bin/ty";
        args = [ "server" ];
        extensionToLanguage = {
          ".py" = "python";
          ".pyi" = "python";
        };
      };
    };
  };
}
