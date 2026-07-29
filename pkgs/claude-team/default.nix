{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-team";
  version = "0.25.0";

  src = pkgs.fetchFromGitHub {
    owner = "bostonaholic";
    repo = "team";
    rev = "v${finalAttrs.version}";
    hash = "sha256-/qolnTsaE6fUL9+ZI4jNKFEhg3LyrBKyjAlfoWHysE8=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script { };

  meta.description = "Multi-agent team workflow toolkit for Claude Code";
})
