{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev:
    let
      inherit (final.lib.attrsets) recursiveUpdate;
    in
    {
    };
}
