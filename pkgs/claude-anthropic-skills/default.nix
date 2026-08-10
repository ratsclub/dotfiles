{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "anthropic-skills";
  version = "0-unstable-2026-08-07";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "skills";
    rev = "f17010c9bb483898c1d9c9f42dde2b3a98889434";
    hash = "sha256-vTqAu8eRY+8ymbf065SWHHjNX/li3SOR+sWq1npteTM=";
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
