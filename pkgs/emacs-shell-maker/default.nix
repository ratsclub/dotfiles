{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "shell-maker";
  version = "0.97.2";

  src = pkgs.fetchFromForgejo {
    domain = "src.r6b.dev";
    owner = "vendor";
    repo = "xenodium.shell-maker";
    tag = "v${finalAttrs.version}";
    hash = "sha256-+bqe2Ss879Dj5iypzL4jRH+UEGQ/9HVxU7qIUAG8NI8=";
  };

  postPatch = ''
    rm markdown-overlays-tables-tests.el
  '';

  passthru.updateScript = pkgs.nix-update-script { extraArgs = [ "--flake" ]; };

  meta = {
    description = "Interactive comint-based shell framework for LLM and agent front-ends";
    homepage = "https://github.com/xenodium/shell-maker";
    license = pkgs.lib.licenses.gpl3Plus;
  };
})
