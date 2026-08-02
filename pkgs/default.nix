{ pkgs, ... }:
{
  claude-anthropic-skills = pkgs.callPackage ./claude-anthropic-skills { };
  claude-mattpocock-skills = pkgs.callPackage ./claude-mattpocock-skills { };
  claude-plugins-official = pkgs.callPackage ./claude-plugins-official { };
  claude-shopify-ai-toolkit = pkgs.callPackage ./claude-shopify-ai-toolkit { };
  claude-team = pkgs.callPackage ./claude-team { };

  claude-statusline = pkgs.callPackage ./claude-statusline { };
  forgejo-runner-image = pkgs.callPackage ./forgejo-runner-image { };
  forgejo-runner-tools = pkgs.callPackage ./forgejo-runner-tools { };
  reasonix = pkgs.callPackage ./reasonix { };
}
