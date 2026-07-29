{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "shopify-ai-toolkit";
  version = "1.2.2-unstable-2026-07-27";

  src = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "0e06bc35611e505e372de7f8cdf265e6d6dbc311";
    hash = "sha256-RrYKj1ii4Ir0JBxawUJVMeP7WyGeB79/yjw7Ie0oPNM=";
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
