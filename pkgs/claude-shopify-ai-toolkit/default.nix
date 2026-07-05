{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "shopify-ai-toolkit";
  version = "1.2.2-unstable-2026-06-30";

  src = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "6980909f2e0eaaf59b4801077fe7e3731bad1b71";
    hash = "sha256-yGAaT7azAfrRh8IJku4wYUp9QN4EWh4pYSB4qsEGFms=";
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
