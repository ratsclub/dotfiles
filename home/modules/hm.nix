{
  config,
  pkgs,
  inputs,
  ...
}:

let
  inherit (inputs) nixpkgs agenix;
in
{
  imports = [
    agenix.homeManagerModules.default
  ];

  fonts.fontconfig.enable = true;

  age.identityPaths = [
    "${config.home.homeDirectory}/.ssh/id_ed25519"
  ];

  nix = {
    package = pkgs.nixVersions.latest;
    registry.nixpkgs.flake = nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  home = {
    sessionPath = [
      "$HOME/.local/bin"
    ];

    stateVersion = "26.05";
  };
}
