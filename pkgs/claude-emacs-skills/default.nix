{ pkgs, ... }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "claude-emacs-skills";
  version = "0-unstable-2026-07-21";

  src = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "emacs-skills";
    rev = "a158238bd630ebe68f57fb9caf99e984e757ca4f";
    hash = "sha256-ZWikhVPlgTw5TqgXU8pCZSRPvnSAxHCqnqgiZvAuV+8=";
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

  meta.description = "Emacs integration skills for Claude Code";
}
