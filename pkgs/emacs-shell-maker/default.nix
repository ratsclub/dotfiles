{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "shell-maker";
  version = "0.95.3";

  src = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    tag = "v${finalAttrs.version}";
    hash = "sha256-KC/dE35hdQPJ6fgmp5nVlDtRjACzTnTIeh7rluORVYA=";
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
