{ config, inputs, username, ... }:

let
  inherit (inputs) nixColors;
  inherit (config.meta) username;
in
{
  colorscheme = nixColors.colorSchemes.tomorrow-night;
  fonts.fontconfig.enable = true;

  nix.registry.nixpkgs.flake = inputs.nixpkgs;

  meta = {
    username = "victor";
    email = "victor@freire.dev.br";
  };

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "20.09";
  };
}