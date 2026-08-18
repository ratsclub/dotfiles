{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-team";
  version = "0.52.0";

  src = pkgs.fetchFromGitHub {
    owner = "bostonaholic";
    repo = "team";
    rev = "v${finalAttrs.version}";
    hash = "sha256-bXGrFcHwo6r5omJYIrmWWZEmMnNCJVqhj/RMlV7dnyk=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script { extraArgs = [ "--flake" ]; };

  meta.description = "Multi-agent team workflow toolkit for Claude Code";
})
