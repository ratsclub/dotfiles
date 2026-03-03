{ inputs, outputs, ... }:

let
  inherit (inputs)
    nixpkgs
    stable
    small
    ;

  mkPkgs =
    {
      nixpkgs,
      system,
      overlay,
      ...
    }:
    import nixpkgs {
      inherit system;
      overlays = [
        outputs.overlays.default
        overlay
      ];
      config.allowUnfree = true;
    };
in
{ }
