{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "anthropic-skills";
  version = "0-unstable-2026-08-13";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "skills";
    rev = "f6656c1256d5a8adfa37db9110046ef20bac644c";
    hash = "sha256-5/0f5AnGWX3oM+M9Xm/zSmooz11+S1YRdFPmAX+DXi0=";
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
