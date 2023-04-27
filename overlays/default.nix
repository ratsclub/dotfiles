{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev:
    let
      inherit (final.lib.attrsets) recursiveUpdate;
      inherit (final.vscode-utils) buildVscodeMarketplaceExtension;
    in
    {
      vscode-extensions = recursiveUpdate prev.vscode-extensions
        {
          ionide.ionide-fsharp = buildVscodeMarketplaceExtension {
            mktplcRef = {
              name = "Ionide-fsharp";
              publisher = "Ionide";
              version = "7.5.3";
              sha256 = "sha256-y3vb5Zte/fyESC63NqHCBeZ4yzE1EmseGGtHWrYGnv4=";
            };
          };

          eamodio.gitlens = buildVscodeMarketplaceExtension {
            mktplcRef = {
              name = "gitlens";
              publisher = "eamodio";
              # Stable versions are listed on the GitHub releases page and use a
              # semver scheme, contrary to preview versions which are listed on
              # the VSCode Marketplace and use a calver scheme. We should avoid
              # using preview versions, because they expire after two weeks.
              version = "13.4.0";
              sha256 = "sha256-CYI62sWPlJNRP2KIkg4vQutIMC6gaCxtTVoOWZIS8Lw=";
            };
          };
        };
    };
}
