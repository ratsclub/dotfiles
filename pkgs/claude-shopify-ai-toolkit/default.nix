{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "shopify-ai-toolkit";
  version = "1.2.2-unstable-2026-07-16";

  src = pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "Shopify-AI-Toolkit";
    rev = "556811e94dd45c795abe5c0b1bf6b5a4b098149d";
    hash = "sha256-xVj+4/r522s1Fun2eDZahk45fIvhbbaTtYmkLEk+C3A=";
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
