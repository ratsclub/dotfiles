{ nixpkgs, small }:

let
  _smallPkgs =
    system:
    import small {
      inherit system;
      config.allowUnfree = true;
    };
in
{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications =
    final: prev:
    let
      inherit (final) lib stdenv;
      inherit (final.lib.attrsets) recursiveUpdate;
      inherit (final.vscode-utils) buildVscodeMarketplaceExtension;
    in
    {
      vscode-extensions = recursiveUpdate prev.vscode-extensions {
        inko.inko-lang = buildVscodeMarketplaceExtension {
          mktplcRef = {
            name = "inko";
            publisher = "inko-lang";
            version = "0.5.0";
            sha256 = "sha256-dW6ixIIAz2zPAhE3ihR/tmobxOKyyXR9M8KHeCmFxgI=";
          };
        };
      };
    };
}
