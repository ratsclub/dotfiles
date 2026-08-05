{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "tabspaces";
  version = "0-unstable-2026-08-04";

  src = pkgs.fetchFromCodeberg {
    owner = "mclear-tools";
    repo = "tabspaces";
    rev = "ef19a5dc4147f6e69442bd2e64c00672438eeb04";
    hash = "sha256-kSxk6/NqDtAOLFSsuekBv2dkaZhga55RxaI1I7wLjDI=";
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
