{ pkgs, ... }:
{
  programs.claude-code = {
    enable = true;

    plugins = [
      pkgs.claude-shopify-ai-toolkit
      "${pkgs.claude-plugins-official}/plugins/code-review"
      "${pkgs.claude-plugins-official}/plugins/playground"
    ];

    skills = {
      skill-creator = "${pkgs.claude-anthropic-skills}/skills/skill-creator";
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
