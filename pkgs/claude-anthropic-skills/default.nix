{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "anthropic-skills";
  version = "0-unstable-2026-07-17";

  src = pkgs.fetchFromGitHub {
    owner = "anthropics";
    repo = "skills";
    rev = "fa0fa64bdc967915dc8399e803be67759e1e62b8";
    hash = "sha256-QZ+zJkyLd/42rxgtJEZSUOz9R75Tse6UXW7G0nOkFS8=";
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
