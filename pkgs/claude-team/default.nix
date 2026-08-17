{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-team";
  version = "0.51.0";

  src = pkgs.fetchFromGitHub {
    owner = "bostonaholic";
    repo = "team";
    rev = "v${finalAttrs.version}";
    hash = "sha256-X+vWvPnFrf+Khkld9LzJHjS3zo6DaTEAa6o5ve5sPqM=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script { extraArgs = [ "--flake" ]; };

  meta.description = "Multi-agent team workflow toolkit for Claude Code";
})
