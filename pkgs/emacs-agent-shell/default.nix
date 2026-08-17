{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "agent-shell";
  version = "0.73.2";

  src = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    tag = "v${finalAttrs.version}";
    hash = "sha256-5/IzIjb9qfkW5nGx1LesJaJZsNKeAM9ttQYRkyMuRyc=";
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
