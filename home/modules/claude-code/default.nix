{ pkgs, ... }:
{
  programs.claude-code = {
    enable = true;

    plugins = {
      shopify-ai-toolkit = pkgs.claude-shopify-ai-toolkit;
      team = pkgs.claude-team;
      mattpocock-skills = pkgs.claude-mattpocock-skills;
      emacs-skills = pkgs.claude-emacs-skills;
      code-review = "${pkgs.claude-plugins-official}/plugins/code-review";
      playground = "${pkgs.claude-plugins-official}/plugins/playground";
    };

    skills = {
      skill-creator = "${pkgs.claude-anthropic-skills}/skills/skill-creator";
      simple-english = "${pkgs.claude-simple-english}/skills/simple-english";
    };

    outputStyles = {
      #   curl -o home/modules/claude-code/attention-kind.md \
      #     https://raw.githubusercontent.com/alexgreensh/attention-span/<tag>/output-styles/attention-kind.md
      attention-kind = ./attention-kind.md;
    };

    settings = {
      outputStyle = "Attention-kind";

      attribution = {
        commit = "";
        pr = "";
      };

      permissions.deny = [
        "Edit(//nix/store/**)"
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

      # Navigating code

      When navigating or refactoring code symbols (definitions, references,
      renames, type/hover info), prefer the LSP tools over text search: they
      resolve imports and scoping, so they won't match a string in a comment or
      an unrelated same-named symbol. Use grep/ripgrep for non-symbol text
      (config keys, log strings, comments, filenames) and for any language with
      no configured language server.
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
