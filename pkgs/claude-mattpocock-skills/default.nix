{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "claude-mattpocock-skills";
  version = "1.1.0";

  src = pkgs.fetchFromGitHub {
    owner = "mattpocock";
    repo = "skills";
    rev = "v${finalAttrs.version}";
    hash = "sha256-XqF709Y9GMKINzZITlbCTyatG9AxRZh0qn2vcv1Z8yo=";
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

  meta.description = "Matt Pocock's Claude Code skills for engineering and productivity workflows";
})
