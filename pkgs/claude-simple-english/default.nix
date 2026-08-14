{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-simple-english";
  version = "1.2.0";

  src = pkgs.fetchFromGitHub {
    owner = "AminBlg";
    repo = "SimpleEnglish";
    rev = "v${finalAttrs.version}";
    hash = "sha256-62IdviEpLgMXYzJwjdM6G7VVJtyaAHGhQGHw2oFCAHE=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script {
    extraArgs = [
      "--version-regex"
      "^v(.*)$"
    ];
  };

  meta.description = "Claude Code skill for writing docs in ASD-STE100 Simplified Technical English";
})
