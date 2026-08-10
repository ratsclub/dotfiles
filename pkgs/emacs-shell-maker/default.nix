{
  pkgs,
  emacsPackages ? pkgs.emacsPackages,
  ...
}:

emacsPackages.trivialBuild (finalAttrs: {
  pname = "shell-maker";
  version = "0.96.1";

  src = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    tag = "v${finalAttrs.version}";
    hash = "sha256-+zVA2rbXXOISbKbugnp4MuEsPBCf/MJd/5jgPySsnoc=";
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
