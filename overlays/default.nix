{ nixpkgs, small }:

let
  smallPkgs = system: import small {
    inherit system;
    config.allowUnfree = true;
  };
in
{
  default = final: _prev: import ../pkgs { pkgs = final; };
  modifications = final: prev: {
    # TODO: wait this to get to unstable
    # https://nixpk.gs/pr-tracker.html?pr=334777
    jetbrains = (smallPkgs final.system).jetbrains;
  };
}
