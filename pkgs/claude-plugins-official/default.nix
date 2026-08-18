{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-plugins-official";
  version = "0-unstable-2026-08-16";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "claude-plugins-official";
    rev = "0bc1c2e378c6d9d04c2914ce61c646832cd4e337";
    hash = "sha256-fCmzPMU4PVZX7WL4K6WmYVdmQ2gfZHey2a3+93SmMCQ=";
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
