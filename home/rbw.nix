{ config, ... }:

let
  inherit (config.meta) email;
in
{
  programs.rbw = {
    enable = true;
    settings = {
      inherit email;
      # change 'gnome3' to 'tty' and run 'rbw register'
      pinentry = "gnome3";
    };
  };
}
