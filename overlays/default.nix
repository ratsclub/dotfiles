{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev:
    let
      inherit (final.lib.attrsets) recursiveUpdate;
    in
    {
      vscode-extensions = recursiveUpdate prev.vscode-extensions {
        ionide.ionide-fsharp = prev.vscode-extensions.ionide.ionide-fsharp.overrideAttrs (oldAttrs: rec {
          version = "7.5.1";
          sha256 = "sha256-AiDYqYF+F69O/aeolIEzqLmg20YN/I4EV6XMa8UgMns=";
        });
      };
    };
}
