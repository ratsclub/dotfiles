{ nixpkgs, small }:

let
  _smallPkgs = system: import small {
    inherit system;
    config.allowUnfree = true;
  };
in
{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev: { };
}
