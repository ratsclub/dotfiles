{ pkgs, ... }:
{
  claude-anthropic-skills = pkgs.callPackage ./claude-anthropic-skills { };
  claude-plugins-official = pkgs.callPackage ./claude-plugins-official { };
  claude-shopify-ai-toolkit = pkgs.callPackage ./claude-shopify-ai-toolkit { };

  claude-statusline = pkgs.callPackage ./claude-statusline { };
  reasonix = pkgs.callPackage ./reasonix { };
}
