{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "shopify-ai-toolkit";
  version = "1.2.2-unstable-2026-08-19";

  src = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "51a7d6cbed88fec14658f4e2d243dcdab128cf77";
    hash = "sha256-NVId2URTdCIpEoefBFgErPFVrsJ3sZTnnb8xmJ2R6zQ=";
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
