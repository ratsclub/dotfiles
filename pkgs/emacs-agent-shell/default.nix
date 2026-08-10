{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "agent-shell";
  version = "0.70.2";

  src = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    tag = "v${finalAttrs.version}";
    hash = "sha256-LSeV8xpPuloh4XRh6HrYUwnozWlxh/J17DC53E+WCy8=";
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
