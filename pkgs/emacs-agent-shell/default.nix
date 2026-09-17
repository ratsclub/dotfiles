{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "agent-shell";
  version = "0.76.1";

  src = pkgs.fetchFromForgejo {
    domain = "src.capivaras.dev";
    owner = "vendor";
    repo = "xenodium.agent-shell";
    tag = "v${finalAttrs.version}";
    hash = "sha256-XwTgzVvUVueJjRrkDMaRJN9KxtlDy6KIObcQ2pojEbE=";
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
