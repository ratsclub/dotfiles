{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "shopify-ai-toolkit";
  version = "1.2.2-unstable-2026-08-05";

  src = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "cc5af6505c27939222072449278f6356857cb064";
    hash = "sha256-GTc79LMA6ZE2zCi9LJ4zBp/1BLNK3slCQTpklrBmjIQ=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };

  meta.description = "Shopify AI developer tools: docs search, GraphQL/Liquid/UI-extension codegen";
})
