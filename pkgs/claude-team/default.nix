{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-team";
  version = "0.53.0";

  src = pkgs.fetchFromGitHub {
    owner = "bostonaholic";
    repo = "team";
    rev = "v${finalAttrs.version}";
    hash = "sha256-FUPDVV83Mk/Ei5rLi15xPteFf3eceLvtZz6JvVxroTQ=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script { extraArgs = [ "--flake" ]; };

  meta.description = "Multi-agent team workflow toolkit for Claude Code";
})
