{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-mattpocock-skills";
  version = "1.2.3";

  src = pkgs.fetchFromGitHub {
    owner = "mattpocock";
    repo = "skills";
    rev = "v${finalAttrs.version}";
    hash = "sha256-I/EXHGW92nXz6JCLp8SKGgzXrbbUTkLAfxv8bc/ThwQ=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    cp -r . $out
  '';

  passthru.updateScript = pkgs.nix-update-script {
    extraArgs = [
      "--flake"
      "--version-regex"
      "^v(.*)$"
    ];
  };

  meta.description = "Matt Pocock's Claude Code skills for engineering and productivity workflows";
})
