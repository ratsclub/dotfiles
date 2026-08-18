{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "anthropic-skills";
  version = "0-unstable-2026-08-18";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "skills";
    rev = "0a64e398ec6bb34a494f0c347e8ccae53a862f8e";
    hash = "sha256-0ZtHTJVHeW8jIprKgCo/yU2ZI2cZxUqD3Riet3UWdt8=";
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

  meta.description = "Anthropic's official Claude skills repository";
})
