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
              version = "7.17.0";
              sha256 = "sha256-CC6ySeuO61O/mAkQYGoK/1cd4hlyS0vG+Lqv0HQ7K6c==";
            };
          };
        };
    };
}
