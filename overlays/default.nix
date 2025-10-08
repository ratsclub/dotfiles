{ nixpkgs, small }:

let
  _smallPkgs = system: import small {
    inherit system;
    config.allowUnfree = true;
  };
in
{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev:
    let
      inherit (final.lib.attrsets) recursiveUpdate;
      inherit (final.vscode-utils) buildVscodeMarketplaceExtension;
    in
    {
      typos = prev.typos.overrideAttrs (_old: rec {
        pname = "typos";
        version = "1.38.1";

        src = final.fetchFromGitHub {
          owner = "crate-ci";
          repo = "typos";
          tag = "v${version}";
          hash = "sha256-xr3k3wx9EWKm00kt1GxE31Mw5wa3N3VJJCKaUbQa4ic=";
        };

        doCheck = false;

        cargoDeps = final.rustPlatform.fetchCargoVendor {
          inherit src;
          hash = "sha256-2XgnCXYqBvx7LRWaPt4iXznIXIEzYBlWMXbwEVZyGA8=";
        };

      });
      vscode-extensions = recursiveUpdate prev.vscode-extensions
        {
          ionide.ionide-fsharp = buildVscodeMarketplaceExtension {
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
