{ pkgs, ... }:
{
  claude-anthropic-skills = pkgs.callPackage ./claudePackages/anthropic-skills { };
  claude-plugins-official = pkgs.callPackage ./claudePackages/claude-plugins-official { };
  claude-shopify-ai-toolkit = pkgs.callPackage ./claudePackages/shopify-ai-toolkit { };

  claude-statusline = pkgs.callPackage ./claude-statusline { };
  reasonix = pkgs.callPackage ./reasonix { };
}
