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

  home = {
    sessionPath = [
      "$HOME/.local/bin"
    ];

    stateVersion = "26.05";
  };
}
