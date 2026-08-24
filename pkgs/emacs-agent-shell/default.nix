{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "agent-shell";
  version = "0.74.3";

  src = pkgs.fetchFromForgejo {
    domain = "src.capivaras.dev";
    owner = "vendor";
    repo = "xenodium.agent-shell";
    tag = "v${finalAttrs.version}";
    hash = "sha256-2eUny9VWtb1XqLrw1uQISb32jMiFL2ESDP2DhAOb2yE=";
  };

  packageRequires = [
    emacsPackages.acp
    emacsPackages.shell-maker
  ];

  passthru.updateScript = pkgs.nix-update-script { extraArgs = [ "--flake" ]; };

  meta = {
    description = "Comint shell for ACP-powered coding agents such as Claude Code";
    homepage = "https://github.com/xenodium/agent-shell";
    license = pkgs.lib.licenses.gpl3Plus;
  };
})
