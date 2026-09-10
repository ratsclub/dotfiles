{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "tabspaces";
  version = "0-unstable-2026-09-08";

  src = pkgs.fetchFromForgejo {
    domain = "src.capivaras.dev";
    owner = "vendor";
    repo = "mclear-tools.tabspaces";
    rev = "2bfb7361b8d82f660eca8bd2e131b5ca53d56916";
    hash = "sha256-qN83yNyuLJbNIpx21qcuPwl6ExxQcSxasrAf3hzNRjc=";
  };

  postPatch = ''
    rm tabspaces-tests.el
  '';

  passthru.updateScript = pkgs.nix-update-script {
    extraArgs = [
      "--flake"
      "--version=branch"
    ];
  };

  meta = {
    description = "Buffer-isolated workspaces built on tab-bar and project.el";
    homepage = "https://codeberg.org/mclear-tools/tabspaces";
    license = pkgs.lib.licenses.gpl3Plus;
  };
})
