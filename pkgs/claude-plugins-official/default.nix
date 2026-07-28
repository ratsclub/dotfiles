{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-plugins-official";
  version = "0-unstable-2026-07-26";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "claude-plugins-official";
    rev = "4acf1a2a30144b9c426bc27d2c341507a91c3b33";
    hash = "sha256-rrL30AYkUs4N2q1TDMFG6WOJi3moF2Chr5GGH7k6RC0=";
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

  meta.description = "Anthropic's official Claude plugin marketplace repository";
})
